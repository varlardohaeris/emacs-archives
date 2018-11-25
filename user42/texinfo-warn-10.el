;;; texinfo-warn.el --- warn about tabs and more in texinfo

;; Copyright 2008, 2009, 2010, 2011, 2012, 2013, 2015, 2016 Kevin Ryde
;;
;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 10
;; Keywords: tex, texinfo
;; EmacsWiki: Texinfo
;; URL: http://user42.tuxfamily.org/texinfo-warn/index.html

;; texinfo-warn.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; texinfo-warn.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; `texinfo-warn-mode' puts a warning overlay face on tabs characters to
;; warn that they are not handled well by makeinfo, and also on @: or }) at
;; the end of a line which were not well handled in the past.  See the
;; `texinfo-warn-mode' docstring for details.

;;; Install:
;;
;; Put texinfo-warn.el in one of your `load-path' directories, and in your
;; .emacs add
;;
;;     (autoload 'texinfo-warn-enable "texinfo-warn")
;;     (add-hook 'texinfo-mode-hook 'texinfo-warn-enable)
;;
;; There's autoload cookies for the functions and a custom options if you
;; install via `M-x package-install' or know `update-file-autoloads'.  Then
;; `add-hook' or customize as desired.

;;; Emacsen:

;; Designed for Emacs 21 and up, works in XEmacs with its overlay.el.

;;; History:

;; Version 1 - the first version
;; Version 2 - tweaks for multiple tabs, and for defface in xemacs21
;; Version 3 - minor mode to be more structured, restrict }) a bit
;; Version 4 - make enable/disable functions use the minor mode function
;; Version 5 - undo defadvice on unload-feature
;; Version 6 - add make-local-hook for xemacs21
;; Version 7 - when disabling remove overlays hidden by narrowing
;; Version 8 - must save-match-data in after-change-functions
;; Version 9 - some bits fixed in makeinfo
;; Version 10 - mark internal funcs


;;; Code:

;; xemacs21 overlay.el, builtin featurep in emacs21 up
(eval-and-compile
  (unless (fboundp 'overlay-put)
    (require 'overlay)))

;;;###autoload
(defgroup texinfo-warn nil "Texinfo-Warn"
  :prefix "texinfo-warn-"
  :group 'tex
  :link  '(url-link
           :tag "texinfo-warn.el home page"
           "http://user42.tuxfamily.org/texinfo-warn/index.html"))

(defface texinfo-warn
  '((((class color))
     (:background "red"))
    (t
     (:inverse-video t)))
  "Face for warning of bad bits in texinfo source.
The default is the same as `trailing-whitespace' face, namely red
background on a colour screen, or inverse video for black and
white."
  :group 'texinfo-warn)

;;-----------------------------------------------------------------------------

(defun texinfo-warn-remove-overlays (beg end)
  "An internal part of texinfo-warn.el.
Remove `texinfo-warn' overlays between BEG and END."
  ;; emacs21 and xemacs21 don't have `remove-overlays' (new in emacs22), but
  ;; this is a bit different anyway as don't really want to split overlays
  ;; crossing the endpoints but just delete the lot
  (dolist (overlay (overlays-in beg end))
    (if (eq 'texinfo-warn (overlay-get overlay 'face))
        (delete-overlay overlay))))

(defun texinfo-warn-after-change (beg end prev-len)
  ;; checkdoc-params: (beg end prev-len)
  "An internal part of texinfo-warn.el.
Put a warning face on tabs between BEG and END.
This function is designed for use from `after-change-functions'."

  (save-match-data
    (save-excursion
      ;; Extend a bit.  Don't go past line-beginning-position in case that
      ;; overlaps an @: on the previous line, wrongly removing some of its
      ;; warning.  Likewise line-end-position and an @: on the next line.
      ;;
      (goto-char beg)
      (beginning-of-line)
      (forward-line -1)
      (setq beg (point))

      (goto-char end)
      (setq end (min (point-max) (max (+ 3 end)
                                      (+ 2 (line-end-position)))))

      ;; lose existing overlays in case offending bits are now ok; or
      ;; offending bits have been deleted leaving a zero-length overlay; and
      ;; so as not to add multiple overlays onto unchanged bits
      (texinfo-warn-remove-overlays beg end)

      ;; but the limit of the re-search-forward must be past the END
      (let ((limit (+ end 5)))

        ;; Match each tab individually, so can delete-overlay instead of
        ;; having to split it when new text is inserted in between tabs.
        ;;
        ;; @: followed by two newlines is ok, it's the end of a paragraph.
        ;;
        ;; }) of say @var{foo}) at the end of a line is ok.  Only @ref and
        ;; @pxref are bad.  But due to only examining one line on a change the
        ;; command name is only reliable for a command on the one line.
        ;; Deliberately enforce that by [^}\n].  This also allows for @: in
        ;; the middle of say an @i{...})
        ;;
        (goto-char beg)
        (while (re-search-forward "\
\t\
\\|\
\\(@:[ \t]*\n\\)[^\n]\
\\|\
\\(@\\([a-zA-Z]+\\){\\([^}\n]\\|@}\\)*\\)?\\(})\\)[ \t]*\\(\n\\|$\\)" limit t)
          (let ((this-start (or (match-beginning 1)    ;; @:
                                (match-beginning 5)    ;; })
                                (match-beginning 0)))  ;; tab
                (this-end   (or (match-end 1)   ;; @: inc its newline
                                (match-end 6)   ;; }) inc its newline
                                (match-end 0))) ;; tab
                (command    (match-string 3)))
            (when (and (<= this-end end)
                       (member command '(nil "ref" "pxref")))
              (let ((overlay (make-overlay this-start this-end
                                           (current-buffer) nil nil)))
                (overlay-put overlay 'face 'texinfo-warn)))))))))

;; is this a good idea?
;;                      ;; don't worry about }) when in an @c comment
;;                      (not (and (match-beginning 2)
;;                                (save-excursion
;;                                  (goto-char this-start)
;;                                  (beginning-of-line)
;;                                  (looking-at "@c ")))))

;;-----------------------------------------------------------------------------

;;;###autoload
(define-minor-mode texinfo-warn-mode
  "Add a warning overlay on doubtful bits of texinfo.
This highlights tabs which don't work quite right in makeinfo 5.2
\(April 2015).

* Tabs in the source are moved by indentation or paragraph
  flowing so in the final info output they no longer line up
  where they did in the source.

And some past bit which didn't work quite right in makeinfo 4.13
\(September 2008).

* \"@:\" meaning \"not the end of a sentence\" didn't have the
  desired effect at the end of a source line.  In info output you
  get two spaces where you wanted one.

* A pxref like \"(@pxref{Fooing,,, foo, Foo Manual})\" at the end
  of a source line ended up with two spaces in the info output,
  where you wanted one.  (The matching is slightly loose, so some
  non-pxref commands ending \"})\" may be highlighted.)

The overlay face is designed to be relatively unobtrusive.  It
shows a likely problem, but doesn't force you to act.  In your
own documents you might want something more aggressive like
always `untabify', or refuse to save.  But that tends to be very
unhelpful when working on a shared or external document where you
want to make an isolated patch or change.

Because the warnings are just overlays, any text cut and pasted
gets only the buffer contents, not the highlights.

See also `texinfo-nobreak-enable' which helps avoid bad line
breaks after \"@:\" or \"})\" in the first place.

The texinfo-warn.el home page is
URL `http://user42.tuxfamily.org/texinfo-warn/index.html'"

  ;; default no :global, so buffer-local
  ;; default :group texinfo-warn

  :type 'boolean
  (if texinfo-warn-mode
      ;; enable
      (progn
        (texinfo-warn-after-change (point-min) (point-max) 0) ;; initial disp
        (if (eval-when-compile (fboundp 'make-local-hook))
            (make-local-hook 'after-change-functions)) ;; for xemacs21
        (add-hook 'after-change-functions
                  'texinfo-warn-after-change
                  t   ;; append
                  t)) ;; buffer-local

    ;; disable
    (remove-hook 'after-change-functions
                 'texinfo-warn-after-change
                 t) ;; buffer local
    (save-restriction
      (widen)
      (texinfo-warn-remove-overlays (point-min) (point-max)))))

;;;###autoload
(defun texinfo-warn-enable ()
  "Turn on `texinfo-warn-mode' highlighting."
  (interactive)
  (texinfo-warn-mode 1))
;;;###autoload
(custom-add-option 'texinfo-mode-hook 'texinfo-warn-enable)

(defun texinfo-warn-disable ()
  "Turn off `texinfo-warn-mode' highlighting."
  (interactive)
  (texinfo-warn-mode 0))

;;-----------------------------------------------------------------------------

(defun texinfo-warn-unload-function ()
  "Remove `texinfo-warn-mode' overlays everywhere.
This is called by `unload-feature'."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (texinfo-warn-disable)))
  nil) ;; and normal unload-feature actions

;; LocalWords: makeinfo docstring

(provide 'texinfo-warn)

;;; texinfo-warn.el ends here
