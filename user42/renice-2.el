;;; renice.el --- change process niceness (CPU priority)

;; Copyright 2014, 2015, 2016, 2017 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 2
;; Keywords: process, processes, nice
;; URL: http://user42.tuxfamily.org/renice/index.html

;; renice.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; renice.el is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; This is an M-x renice command to "nice" down a sub-process, by running
;; the renice(1) program.  It can be used in `compilation-mode' and similar
;; buffers with a sub-process, and also from M-x list-processes.
;; (M-x proced and M-x top have their own renices too.)
;;
;; The main use is to turn down priority of a slow program so it can be left
;; to run without degrading other jobs.  A program you know will be slow can
;; be started with "nice -10 ...", or can itself lower priority with the
;; right system calls, but sometimes you don't know in advance and when you
;; see how slow it's good to turn it down without restarting etc.

;;; Emacsen:

;; Designed for Emacs 20 up and XEmacs 21 up.

;;; Install:
;;
;; To make `M-x renice' available, put renice.el in one of your `load-path'
;; directories and the following in your .emacs
;;
;;     (autoload 'renice "renice" nil t)
;;
;; If you want the suggested keybindings of `renice-keybindings' then add
;; also
;;
;;     (autoload 'renice-keybindings "renice" nil t)
;;     (add-hook 'compilation-mode-hook 'renice-keybindings)
;;     (add-hook 'process-menu-mode-hook 'renice-keybindings)
;;
;; There's autoload cookies for `renice' and `renice-keybindings' if you
;; install via `M-x package-install' or know how to use
;; `update-file-autoloads'.
;;
;; The autoloads also add a Renice menu entry in compilation mode.

;;; History:
;;
;; Version 1 - the first version
;; Version 2 - quieten the byte compiler a bit

;;; Code:

(eval-when-compile
  (unless (fboundp 'dolist) ;; for macro in emacs20
    (require 'cl)))

;; in compile.el, and autoloaded to quieten byte compile miscbits-el-loaddefs.el
;;;###autoload
(defvar compilation-mode-map)


;;-----------------------------------------------------------------------------

;; maybe better say +5 or +10 from the current niceness, if that is known
(defconst renice-default-nice 10
  "An internal part of renice.el.
The default niceness to offer in `renice'.")

(defvar renice-nice-history nil
  "History list of nicenesses entered for `renice'.")
  
(defun renice--process-attribute (pid key)
  "An internal part of renice.el.
PID is a system process ID (an integer).
Return property KEY (a symbol) from `process-attributes', or nil
if KEY not available or if `process-attributes' function doesn't
exist at all (new in Emacs 23)."

  (and (eval-when-compile (fboundp 'process-attributes))
       (cdr (assq key (process-attributes pid)))))

(defun renice-pid-and-name-at-point ()
  "An internal part of renice.el.
Return a pair (PID . NAME) of process ID (an integer) and name
string of process at point, which renice will act on.

In a compilation buffer this is the sub-process.  In
`process-menu-mode' it is the process on the line at point."

  (eval-and-compile
    (require 'derived)) ;; for emacs21 since `derived-mode-p' is not preloaded

  (cond ((derived-mode-p 'process-menu-mode)
         ;; emacs24 M-x list-processes
         (let ((process (tabulated-list-get-id)))
           (unless (and process
                        (eq (process-status process) 'run))
             (error "No process on this line"))
           (unless (process-live-p process)
             (error "Process not live"))
           (cons (process-id process)
                 (process-name process))))

        ((derived-mode-p 'proced-mode)
         (let ((pid (proced-pid-at-point)))
           (cons pid
                 ;; Name from `process-attributes' if available.
                 ;; Could parse out from the "top" display, but
                 ;; `process-attributes' should be more reliable.
                 (or (renice--process-attribute pid 'comm)
                     (number-to-string pid)))))

        ((derived-mode-p 'top-mode)
         (let ((pid (top-mode-get-pid-from-line)))
           (cons pid
                 (or (renice--process-attribute pid 'comm)
                     (number-to-string pid)))))

        (t
         (let (process)
           ;; First of current-buffer, *compilation* buffer, or top-most of
           ;; buffer-list with a live process.
           (dolist (buffer (cons (current-buffer)
                                 (cons "*compilation*"
                                       (buffer-list))))
             (or process
                 (setq process (renice-this-buffer-process buffer))))
           (unless process
             (error "No sub-process"))
           (cons (process-id process)
                 (process-name process))))))

(defun renice--read-niceness (name pid)
  "An internal part of renice.el.
Read a niceness value from the user.
NAME is a process name shown in the prompt.
PID is used to display current niceness if possible."

  (let ((nice (renice--process-attribute pid 'nice)))
    (string-to-number
     (read-string (if nice
                      (format "%s niceness currently %d (default %d): "
                              name nice renice-default-nice)
                    (format "%s niceness (default %d): "
                            name renice-default-nice))
                  nil;; initial input
                  'renice-nice-history
                  (number-to-string renice-default-nice)))))

;;;###autoload
(defun renice (pid &optional nice)
  ;; checkdoc-params: (pid nice)
  "Set the niceness (CPU priority) of a sub-process.
Niceness is a number 0 to 19.  19 is the nicest, meaning the
lowest priority.  Niceness is set using the \"renice\" program.

The process to act on is the process of the current buffer, or of
a *compilation* buffer, or otherwise the top-most buffer with a
process.  Or in an `M-x list-processes', `M-x proced' or
`M-x top' \(top-mode.el) the process on the current line.
\(`M-x proced' and `M-x top' have their own renice though.)

An ordinary user can generally only increase niceness, lowering
priority.  Once lowered it's normally not possible to return to
higher priority, not even just back to where it was before.

The super-user can set any niceness, higher or lower.  The
super-user can even set negative niceness up to -20 to have
higher priority than ordinary processes.

The \"renice -g\" option is used so that if the process is a
process group leader then all its sub-processes are reniced too.

See the \"renice(1)\" man page for more.  The Linux kernel has an
\"ionice\" which is priority for I/O operations such as disk
access.  The default ionice is based on the CPU niceness and
usually doesn't need to be set separately.

----
The renice.el home page is
URL `http://user42.tuxfamily.org/renice/index.html'"

  (interactive
   (let* ((pair (renice-pid-and-name-at-point))
          (pid  (car pair))
          (name (cdr pair)))
     (list pid (renice--read-niceness name pid))))

  (unless nice (setq nice renice-default-nice))
  (with-temp-buffer
    (call-process "renice"
                  nil               ;; input
                  (current-buffer)  ;; output
                  nil;; no redisplay
                  "--priority" (number-to-string nice)
                  "--pgrp" (number-to-string pid))
    (skip-chars-backward " \t\r\n")
    (message "%s" (buffer-substring (point-min) (point)))))

;;;###autoload
(defun renice-this-buffer ()
  "Set the niceness (CPU priority) of the buffer process.
This is per `renice' but acts only on a process in the current
buffer."
  (interactive)
  (let* ((process (or (renice-this-buffer-process)
                      (error "No running sub-process")))
         (pid (process-id process)))
    (renice pid (renice--read-niceness (process-name process) pid))))

(defun renice-this-buffer-process (&optional buffer)
  "An internal part of renice.el.
Return a process object in BUFFER.
If BUFFER is omitted or nil then look at the current buffer.
If the buffer doesn't have a process or it has one but is no
longer running then return nil."

  (let ((process (get-buffer-process (or buffer (current-buffer)))))
    (and process
         (eq (process-status process) 'run)
         process)))

;;;###autoload
(defun renice-keybindings ()
  "Set some key bindings for `renice'.
These are meant to be some sensible keys.  Currently

   N   `compilation-mode'
   N   `list-processes' in Emacs 24 `process-menu-mode'

\"N\" is for niceness here (and with a view to leaving \"R\" free
for `rename-buffer' which is something also wanted when a process
is taking a long time)."

  (interactive)
  (eval-after-load "compile"
    '(define-key compilation-mode-map "N" 'renice))

  ;; M-x list-processes is mode `process-menu-mode' in emacs24.
  ;; Was fundamental-mode or help-mode previously.
  (if (eval-when-compile (boundp 'process-menu-mode-map)) ;; new in emacs24
      (define-key process-menu-mode-map "N" 'renice)))

;;;###autoload
(custom-add-option 'compilation-mode-hook 'renice-keybindings)
;;;###autoload
(custom-add-option 'process-menu-mode-hook 'renice-keybindings)

;;-----------------------------------------------------------------------------
;; Compile/Renice menu entry
;;
;; The menu entry acts only on the current buffer `renice-this-buffer'.

;; GNU Emacs
;;
;;;###autoload
(when (eval-when-compile (fboundp 'define-key-after))
  (eval-after-load "compile"
    '(let ((keymap (or (lookup-key compilation-mode-map [menu-bar compilation])
                       ;; Emacs 20
                       (lookup-key compilation-mode-map [menu-bar compilation-menu]))))
       (when (keymapp keymap)
         (define-key-after keymap [renice]
           '(menu-item "Renice" renice-this-buffer
                       :enable (renice-this-buffer-process)
                       :help "Change process priority.")
           ;; after "Recompile"
           (if (lookup-key compilation-mode-map [menu-bar compilation-menu
                                                          compilation-mode-recompile])
               'compilation-mode-recompile  ;; Emacs 20
             'compilation-recompile))))))

;; XEmacs 21
;; Insert new menu entry after Recompile.
;; Must mutate `Compilation-mode-popup-menu' so that
;; `Compilation-mode-menubar-menu' sees the same change.
;;
;;;###autoload
(when (eval-when-compile (fboundp 'add-menu-button))
  (eval-after-load "compile"
    '(when (boundp 'Compilation-mode-popup-menu)
       (let ((after ["Recompile" recompile t]))
         (when (member after Compilation-mode-popup-menu)
           (let ((cell (member after Compilation-mode-popup-menu)))
             (setcdr cell (cons ["Renice" renice-this-buffer
                                 (renice-this-buffer-process)]
                                (cdr cell))))
           (set-menubar-dirty-flag))))))

(provide 'renice)

;; LocalWords: nicenesses ionice renice renices reniced

;;; renice.el ends here
