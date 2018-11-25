;;; localedef-overlay.el --- mode for glibc locale definition files

;; Copyright 2009, 2010, 2011, 2012, 2013, 2015 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 4
;; Keywords: tools, unicode, display
;; URL: http://user42.tuxfamily.org/localedef-overlay/index.html

;; localedef-overlay.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; localedef-overlay.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; `M-x localedef-overlay-mode' puts overlays on strings like "<U12BA>"
;; showing instead the corresponding unicode character.  This is good for
;; viewing glibc locale .def files, and ICU .ucm files.  See the
;; `localedef-overlay-mode' docstring below for more.

;;; Install:
;;
;; Put localedef-overlay.el in one of your `load-path' directories, and in
;; your .emacs add the following to make `M-x localedef-overlay-mode'
;; available.
;;
;;     (autoload 'localedef-overlay-mode "localedef-overlay")
;;
;; There's an autoload cookie for this below if you know how to use
;; `update-file-autoloads' and friends.

;;; Emacsen:

;; Designed for Emacs 21 and up.  Works in XEmacs if you have mule-ucs for
;; unicode characters.  In Emacs 21 you may want mule-ucs too for extra
;; east-Asian unicode characters.

;;; History:
;;
;; Version 1 - the first version
;; Version 2 - xemacs21 decode latin1
;; Version 3 - must save-match-data in after-change-functions
;; Version 4 - new email

;;; Code:

;; xemacs21 overlay.el, builtin featurep in emacs21 up
(eval-and-compile
  (unless (fboundp 'overlay-put)
    (require 'overlay)))

(defun localedef-overlay-decode-ucs (str)
  "Decode hex STR like \"2B1F\" to a single-char unicode string.
If there's no unicode coding available (eg. xemacs21 without
mule-ucs) then return nil."

  (let ((num (string-to-number str 16)))
    (cond ((eval-when-compile (fboundp 'decode-char))
           ;; emacs21 up
           (string (decode-char 'ucs num)))

          ((<= num 255)
           ;; xemacs21 latin1, maybe emacs20
           (decode-coding-string (string num) 'iso-8859-1))

          ((memq 'utf-16-be (coding-system-list))
           ;; xemacs21 with mule-ucs, maybe emacs20
           ;; not sure this is right on the surrogates D800 etc, but they
           ;; shouldn't occur
           (decode-coding-string (string (ash num -8)
                                         (logand num 255))
                                 'utf-16-be)))))
(eval-when-compile
  (put 'localedef-overlay-decode-ucs 'side-effect-free t))

(defun localedef-overlay-put-display (overlay str)
  "Set properties in OVERLAY to make it display as STR.
This is the 'display property in Emacs, or 'invisible plus 'end-glyph in
XEmacs (overlays emulated with extents)."
  (if (and (eval-when-compile (fboundp 'extentp))
           (extentp overlay))
      ;; xemacs21
      (progn
        (overlay-put overlay 'invisible t)
        (overlay-put overlay 'end-glyph (make-glyph str)))
    ;; emacs
    (overlay-put overlay 'display str)))

;;-----------------------------------------------------------------------------

(defun localedef-overlay-remove-overlays (beg end)
  "Remove `localedef-overlay' overlays between BEG and END."
  ;; emacs21 and xemacs21 don't have `remove-overlays' (new in emacs22), but
  ;; this is a bit different anyway as don't need to split overlays crossing
  ;; the endpoints but just delete the whole thing
  (dolist (overlay (overlays-in beg end))
    (if (overlay-get overlay 'localedef-overlay)
        (delete-overlay overlay))))

(defun localedef-overlay-after-change (beg end prev-len)
  ;; checkdoc-params: (beg end prev-len)
  "Update overlays on <U1234> between BEG and END.
This function is designed for use from `after-change-functions'."

  (save-match-data
    (save-excursion
      ;; extend to take in whole of a <U....> if first or last of the 7 chars
      ;; just entered
      (setq beg (max (point-min) (- beg 6)))
      (setq end (min (point-max) (+ end 6)))

      ;; extend across whole of any existing overlay and the ends in case a char
      ;; deleted now makes the overlay unwanted
      (dolist (overlay (overlays-at beg))
        (if (overlay-get overlay 'localedef-overlay)
            (setq beg (overlay-start overlay))))
      (dolist (overlay (overlays-at end))
        (if (overlay-get overlay 'localedef-overlay)
            (setq end (overlay-end overlay))))

      ;; lose existing overlays so as not to add multiple overlays onto
      ;; unchanged bits; and to remove on stuff no longer in <U....> form
      ;; (including any zero-length overlays left after deletes)
      (localedef-overlay-remove-overlays beg end)

      (goto-char beg)
      (let ((case-fold-search nil))
        (while (re-search-forward "<U\\([0-9A-F]\\{4\\}\\)>" end t)
          (let ((str (localedef-overlay-decode-ucs (match-string 1))))
            (when str
              (let ((overlay (make-overlay (match-beginning 0) (match-end 0)
                                           (current-buffer)
                                           t))) ;; front-advance, excl new text
                (overlay-put overlay 'localedef-overlay t)
                (localedef-overlay-put-display overlay str)))))))))


;;-----------------------------------------------------------------------------

;;;###autoload
(define-minor-mode localedef-overlay-mode
  "Overlay <U1234> forms with corresponding unicode characters.
This is good for viewing GNU C Library \"localedef\" .def
definition files and ICU .ucm files.

The overlays update with edits in the buffer, but they're meant
mainly for viewing and might get in the way when doing more than
a little cut and paste or comments.

If you don't have unicode charsets loaded (eg. XEmacs 21 without
mule-ucs) then only latin-1 characters are overlaid.

----
The .ucm file format is described at
URL `http://userguide.icu-project.org/conversion/data#TOC-.ucm-File-Format'

The localedef-overlay.el home page is
URL `http://user42.tuxfamily.org/localedef-overlay/index.html'"

  ;; default no :global, so buffer-local
  ;; default :group localedef-overlay

  :type 'boolean
  (if localedef-overlay-mode
      ;; enable
      (progn
        (if (eval-when-compile (fboundp 'make-local-hook))
            (make-local-hook 'after-change-functions)) ;; for xemacs21
        (add-hook 'after-change-functions
                  'localedef-overlay-after-change
                  t  ;; append
                  t) ;; buffer-local
        (localedef-overlay-after-change (point-min) (point-max) 0)) ;; initial

    ;; disable
    (remove-hook 'after-change-functions
                 'localedef-overlay-after-change
                 t) ;; buffer-local
    (save-restriction
      (widen)
      (localedef-overlay-remove-overlays (point-min) (point-max)))))

(defun localedef-overlay-unload-function ()
  "Remove `localedef-overlay' overlays everywhere.
This is called by `unload-feature'."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (localedef-overlay-mode 0)))
  nil) ;; and normal unload-feature actions

;; LocalWords: unicode glibc ucm docstring ucs http userguide TOC def

(provide 'localedef-overlay)

;;; localedef-overlay.el ends here
