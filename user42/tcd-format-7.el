;;; tcd-format.el --- view XTide .tcd tide constituent database files

;; Copyright 2008, 2009, 2010 Kevin Ryde

;; Author: Kevin Ryde <user42@zip.com.au>
;; Version: 7
;; Keywords: data
;; URL: http://user42.tuxfamily.org/tcd-format/index.html
;; EmacsWiki: XTide

;; tcd-format.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; tcd-format.el is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This code sets up human-readable decode of XTide .tcd tide constituent
;; database files using the `buffer-file-format' mechanism and the
;; restore_tide_db program from XTide tcd-utils,
;;
;;     http://www.flaterco.com/xtide/files.html#extras
;;
;; It's only meant for browsing tcd files if you don't have the source
;; handy.  There's no support for re-writing a .tcd file.
;;
;; For more on XTide see <http://www.flaterco.com/xtide>.
;; And have a look at xtide.el for running within Emacs
;; <http://user42.tuxfamily.org/xtide/index.html>

;;; Emacsen:

;; Designed for Emacs 22 and up.  Works in Emacs 21 and XEmacs 21.
;; Works in Emacs 20 if you've got either Gnus mm-util.el or APEL poe.el for
;; make-temp-file.

;;; Install:

;; Put tcd-format.el in one of your `load-path' directories and the following
;; in your .emacs
;;
;;     (require 'tcd-format)
;;
;; If you're using Emacs 21 see xml-coding.el to notice the charset in the
;; .xml substations part (Emacs 22 does that already).
;;
;; Or to defer loading then just the bits below marked for autoload,
;;
;;     (add-to-list 'format-alist
;;                  '(tcd
;;                    "XTide tide constituent data file."
;;                    "\\`\\[VERSION] = PFM Software - libtcd"
;;                    tcd-format-decode
;;                    tcd-format-encode
;;                    t
;;                    nil))
;;     (autoload 'tcd-format-decode "tcd-format")
;;     (autoload 'tcd-format-encode "tcd-format")
;;     (modify-coding-system-alist 'file "\\.tcd\\'" 'raw-text-unix)
;;
;; If a future version of tcd-format.el has a new format-alist entry you'll
;; have to update your .emacs.  If you know how to use
;; `update-file-autoloads' and friends you can have it grab those forms out
;; automatically.  The debian/emacsen-startup in the source .tar does that.

;;; History:

;; Version 1 - the first version
;; Version 2 - kill *tcd-format-errors* buffer if no errors
;; Version 3 - cope with non-existent `default-directory'
;; Version 4 - new home page, now as a tar+deb too
;; Version 5 - use pipe rather than pty for subprocess
;; Version 6 - delete errors window when no errors
;; Version 7 - display errors in a different window
;;           - use APEL poe.el for make-temp-file in emacs20

;;; Code:

;;-----------------------------------------------------------------------------
;; compatibility

(eval-and-compile
  (cond ((or (eval-when-compile (fboundp 'make-temp-file))
             (fboundp 'make-temp-file))
         ;; emacs21 up, noticed at compile time or run time
         (defalias 'tcd-format--make-temp-file 'make-temp-file))

        ((locate-library "mm-util") ;; from gnus
         ;; xemacs21
         (autoload 'mm-make-temp-file "mm-util")
         (defalias 'tcd-format--make-temp-file 'mm-make-temp-file))

        ((locate-library "poe") ;; from APEL
         ;; emacs20 with poe.el add-on
         (require 'poe)
         (defalias 'tcd-format--make-temp-file 'make-temp-file))

        (t
         ;; umm, dunno, hope the user can define it
         (message "tcd-format.el: don't know where to get `make-temp-file'")
         (defalias 'tcd-format--make-temp-file 'make-temp-file))))

(eval-and-compile
  (defalias 'tcd-format--set-buffer-multibyte
    (if (eval-when-compile (fboundp 'set-buffer-multibyte))
        'set-buffer-multibyte  ;; emacs
      'identity)))             ;; not applicable in xemacs21


;;-----------------------------------------------------------------------------
;; generic

(defun tcd-format-switch-to-buffer-other-window (buffer)
  "Switch to display BUFFER in another window.
If it isn't already in a window then the window is shrunk with
`shrink-window-if-larger-than-buffer'."
  (let ((existing-window (get-buffer-window buffer)))
    (condition-case nil
        ;; emacs two args
        (switch-to-buffer-other-window buffer t) ;; no-record
      (error
       ;; xemacs one arg
       (switch-to-buffer-other-window buffer)))
    (if (not existing-window)
        (shrink-window-if-larger-than-buffer (get-buffer-window buffer)))))

;;-----------------------------------------------------------------------------
;;;###autoload
(modify-coding-system-alist 'file "\\.tcd\\'" 'raw-text-unix)

;;;###autoload
(add-to-list 'format-alist
             '(tcd
               "XTide tide constituent data file."
               "\\`\\[VERSION] = PFM Software - libtcd"
               tcd-format-decode
               tcd-format-encode
               t
               nil))

(defconst tcd-format-errors-buffer
  "*tcd-format-errors*"
  "The name of the buffer holding tcd decode error messages.")

;;;###autoload
(defun tcd-format-encode (beg end buffer)
  ;; checkdoc-params: (beg end buffer)
  "Sorry, cannot encode `tcd' format.
There's no support for editing and re-writing tcd files.  It'd be
possible, but if you're changing the data you're almost certainly
either going from some source files or using the tideEditor
program.  The idea of `tcd' format is really just to see .tcd
contents when you don't have the source handy."
  (error "Sorry, `tcd' format is read-only"))

;;;###autoload
(defun tcd-format-decode (beg end)
  ;; checkdoc-params: (beg end)
  "Run restore_tide_db on raw .tcd bytes in the current buffer.
This function is for use from `format-alist'.

The buffer should be unibyte and contain raw tcd format bytes, as
per a `raw-text-unix' read.  Those bytes are put through
restore_tide_db to get the source text, including xml
substations, and the buffer switched to multibyte.

An error is thrown if restore_tide_db can't be run or the buffer
contents are invalid.

A .tcd decompiles to a .txt file and if it includes substations
then a .xml of those too.  The .xml is appended in the decoded
buffer.

The tcd-format.el home page is
URL `http://user42.tuxfamily.org/tcd-format/index.html'"

  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (let* ((tmpdir   (file-name-as-directory
                        (tcd-format--make-temp-file "tcd-format-" t)))
             (basename (concat tmpdir "foo"))
             (tcdfile  (concat basename ".tcd"))
             (txtfile  (concat basename ".txt"))
             (xmlfile  (concat basename ".xml")))

        (unwind-protect
            (progn
              (write-region (point-min) (point-max) tcdfile)

              (with-current-buffer
                  (get-buffer-create tcd-format-errors-buffer)
                (erase-buffer))

              (message "Running restore_tide_db ...")
              (let* ((default-directory tmpdir) ;; might inherit non-existent
                     (process-connection-type nil) ;; pipe
                     (status (call-process
                              "restore_tide_db"
                              nil       ;; infile
                              (list tcd-format-errors-buffer
                                    t)  ;; stdout + stderr together
                              nil       ;; no redisplay
                              tcdfile
                              basename)))

                (cond ((eq 0 status)
                       ;; Success.

                       ;; If no error messages then kill errors buffer and
                       ;; window.  `delete-windows-on' needs arg before
                       ;; emacs23.
                       (with-current-buffer tcd-format-errors-buffer
                         (when (= (point-min) (point-max))
                           (delete-windows-on (current-buffer))
                           (kill-buffer nil)))
                       (tcd-format--set-buffer-multibyte t)

                       ;; Fairly sure the .txt file is supposed to be
                       ;; latin-1, that's the libtcd.html spec.
                       ;;
                       ;; The .xml has an "encoding" declared in its header,
                       ;; which `insert-file-contents' will follow as
                       ;; necessary.
                       ;;
                       (let ((coding-system-for-read 'iso-8859-1))
                         (insert-file-contents txtfile nil nil nil t))
                       (when (file-exists-p xmlfile)
                         (goto-char (point-max))
                         (insert-file-contents xmlfile))
                       (message nil))

                      (t
                       ;; Error.
                       (tcd-format-switch-to-buffer-other-window
                        tcd-format-errors-buffer)
                       (error "Error running restore_tide_db program")))))

          (condition-case nil (delete-file tcdfile) (error))
          (condition-case nil (delete-file txtfile) (error))
          (condition-case nil (delete-file xmlfile) (error))
          (delete-directory tmpdir))

        (point-max)))))

;; LocalWords: XTide tcd db utils www flaterco tuxfamily org

(provide 'tcd-format)

;;; tcd-format.el ends here
