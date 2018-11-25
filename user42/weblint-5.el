;;; weblint.el --- run the weblint program

;; Copyright 2011, 2012, 2013, 2014 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 5
;; Keywords: wp, html
;; URL: http://user42.tuxfamily.org/weblint/index.html
;; EmacsWiki: HtmlMode

;; weblint.el is free software; you can redistribute
;; it and/or modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; weblint.el is distributed in the hope that it will
;; be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; `M-x weblint' runs the "weblint" program on the current buffer to check
;; syntax of HTML.
;;
;; `weblint-after-save-setup' can run weblint automatically whenever saving
;; HTML.
;;
;; Both ways display errors in a compilation-mode buffer.  For Emacs 22 and
;; earlier see compilation-weblint.el to add error regexps for this.  (Emacs
;; 23 matches them already.)

;;; Install:
;;
;; Put weblint.el in one of your `load-path' directories and to make
;; `M-x weblint' available add to your .emacs
;;
;;     (autoload 'weblint "weblint" nil t)
;;
;; Or for after-save checking in all `html-mode' buffers
;;
;;     (autoload 'weblint-after-save-setup "weblint" nil t)
;;     (add-hook 'html-mode-hook 'weblint-after-save-setup)
;;
;; There's cookies below for the autoloads if you know how to use
;; `update-file-autoloads' and friends.

;;; History:

;; Version 1 - the first version
;; Version 2 - window size after extra initial lines
;; Version 3 - quieten xemacs byte compile
;; Version 4 - ensure error window when just 1 line of error
;; Version 5 - allow for output buffer killed when re-running

;;; Code:

;;-----------------------------------------------------------------------------
;; compatibility

;; not in xemacs, quieten its byte compile
(defvar max-mini-window-height)
(defvar compilation-disable-input)


;;-----------------------------------------------------------------------------
;; weblint interactive

;;;###autoload
(defun weblint ()
  "Run the \"weblint\" program on the current buffer file.

weblint is part of the Perl HTML-Lint package,
URL `http://search.cpan.org/dist/HTML-Lint/'

The weblint.el home page is
URL `http://user42.tuxfamily.org/weblint/index.html'"

  (interactive)
  (let ((weblint--target (current-buffer)))
    (save-some-buffers nil (lambda ()
                             (equal weblint--target (current-buffer)))))
  (let ((compilation-disable-input t)
        (compilation-buffer-name-function (lambda (mode) "*weblint*")))
    (compile (concat "weblint "
                     (shell-quote-argument (buffer-file-name))))))


;;-----------------------------------------------------------------------------
;; weblint after-save

(defvar weblint-output-buffer nil
  "The *weblint* output buffer for the current buffer.
This is an internal part of weblint.el.")

(defun weblint-kill-output-buffer ()
  "Kill the *weblint* output buffer of the current buffer.
This function is put in `kill-buffer-hook' so that if a buffer is
killed then its `weblint-output-buffer' is killed too."
  (when (buffer-live-p weblint-output-buffer)
    (delete-windows-on weblint-output-buffer)
    (kill-buffer weblint-output-buffer)
    (setq weblint-output-buffer nil)))

(defun weblint-after-save-handler ()
  "Run weblint on the current buffer.
This function is designed for use from `after-save-hook'.
See `weblint-after-save-setup' for details."

  (unless (buffer-live-p weblint-output-buffer)
    ;; when this buffer is killed, kill its weblint-output-buffer
    (if (eval-when-compile (fboundp 'make-local-hook))
        (make-local-hook 'kill-buffer-hook)) ;; for xemacs21
    (add-hook 'kill-buffer-hook
              'weblint-kill-output-buffer
              t  ;; append
              t) ;; buffer-local

    ;; ENHANCE-ME: this local weblint-output-buffer is lost on a mode
    ;; change, would like to preserve it somehow
    (set (make-local-variable 'weblint-output-buffer)
         (generate-new-buffer (concat "*weblint* " (buffer-name)))))

  (let* ((filename        (buffer-file-name))
         (existing-window (get-buffer-window weblint-output-buffer)))

    (with-current-buffer weblint-output-buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (call-process "weblint"
                    nil      ;; stdin /dev/null
                    (list t  ;; stdout current buffer
                          t) ;; stderr mixed in
                    t
                    filename))

    (cond ((with-current-buffer weblint-output-buffer
             (equal (point-min) (point-max)))
           ;; no error messages
           (delete-windows-on weblint-output-buffer)
           (kill-buffer weblint-output-buffer)
           (kill-local-variable 'weblint-output-buffer))
          (t
           ;; show error messages
           (with-current-buffer weblint-output-buffer
             ;; emacs21 ignores the first two lines of a compilation-mode
             ;; buffer, so two dummy lines
             (goto-char (point-min))
             (insert "weblint " filename "\n\n")
             (compilation-mode))
           (display-buffer weblint-output-buffer)
           (save-selected-window
             (if (and (get-buffer-window weblint-output-buffer)
                      (not existing-window))
                 (shrink-window-if-larger-than-buffer
                  (get-buffer-window weblint-output-buffer))))))))

;;;###autoload
(defun weblint-after-save-setup ()
  "Setup to run weblint on the current buffer when saved.
\"weblint\" checks syntax etc of HTML and running it when saving
is a good way to avoid errors creeping into hand-written markup.
For example to have it from any `html-mode' buffer,

    (add-hook 'html-mode-hook 'weblint-after-save-setup)

If you don't want weblint on some `html-mode' buffers then a
conditional might be used in the hook, such as

    (add-hook 'html-mode-hook
              (lambda ()
                (if (something)
                    (weblint-after-save-setup))))

Note that `save-some-buffers' prevents window changes so a
weblint error window is not shown (if it doesn't already exist).
Error buffers are created but not displayed.  Is there a good way
to fix that?  Probably not without changing `save-some-buffers'."

  (if (fboundp 'make-local-hook) ;; for xemacs21
      (make-local-hook 'after-save-hook))
  (add-hook 'after-save-hook 'weblint-after-save-handler
            t   ;; append
            t)) ;; local

;;;###autoload
(custom-add-option 'html-mode-hook 'weblint-after-save-setup)

;; LocalWords: el weblint regexps

(provide 'weblint)

;;; weblint.el ends here
