;;; tex-increase.el --- increase or decrease TeX \bigl etc

;; Copyright 2015, 2016, 2017, 2019 Kevin Ryde
;;
;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 7
;; Keywords: wp, TeX, LaTeX
;; URL: http://user42.tuxfamily.org/tex-increase/index.html
;; EmacsWiki: TeX

;; tex-increase.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; tex-increase.el is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; `M-x tex-increase' and `M-x tex-decrease' increase or decrease various
;; size macros for TeX and LaTeX.  For example
;;
;;     \bigl(    --->    \Bigl(
;;
;; Similar can be found in `YaTeX-change-parentheses', but it has
;; `read-char' for desired new size whereas tex-increase just goes a size up
;; or down.  tex-increase has font sizes and some skips to increase or
;; decrease too.  sEE `tex-increase-lists' for all the forms.

;;; Install:

;; Put tex-increase.el in one of your `load-path' directories, and get
;; commands M-x tex-increase and M-x tex-decreaase with the following in
;; your .emacs,
;;
;;     (autoload 'tex-increase "tex-increase" nil t)
;;     (autoload 'tex-decrease "tex-decrease" nil t)
;;
;; There's autoload cookies for these commands if you install by
;; `M-x package-install' or know how to use `update-file-autoloads'.
;;
;; There's no keybindings currently.  The suggestion is control +/-, which
;; are [?\C-+] [?\C--], although the latter might be inconveniently close to
;; C-_ undo if - and _ are adjacent on the keyboard.  Those control keys
;; probably only work (ie. are different from plain +/-) under X or other
;; GUI, not tty or console.  But for TeX you're probably on a GUI to see
;; previews anyway.

;;; History:

;; Version 1 - the first version
;; Version 2 - allow for ,;> not word ends
;; Version 3 - add \thinmuskip etc
;; Version 4 - no paren ( to \bigl( on decrease
;; Version 5 - more prefer macro at point over before point
;; Version 6 - fix decrease of \thinmuskip
;; Version 7 - fix for point in middle of \bigl\lvert

;;; Code:

(eval-when-compile
  (unless (and (fboundp 'dolist)
               (fboundp 'dotimes))
    (require 'cl))) ;; for macros in emacs20

;; emacs21 `derived-mode-p' is not preloaded
(autoload 'derived-mode-p "derived")

(eval-and-compile
  (defconst tex-increase-parens-left
    '("\\lbrace" "\\langle" "\\lvert" "\\lceil" "\\lfloor"
      "\\lparen"  ;; from mathtools package
      )
    "An internal part of tex-increase.el.
A list of left parentheses macros which might have \\=\\bigl etc."))

;; cf hairy code in yatexmth.el `YaTeX-on-parenthesis-p'
(eval-and-compile
  (defconst tex-increase-parens-right
    '("\\rbrace" "\\rangle" "\\rvert" "\\rceil" "\\rfloor"
      "\\rparen"  ;; from mathtools package
      )
    "An internal part of tex-increase.el.
A list of right parentheses macros which might have \\=\\bigr etc."))

(eval-and-compile
  (defconst tex-increase-lists
    '((nil "\\bigl" "\\Bigl" "\\biggl" "\\Biggl" "\\left")
      (nil "\\bigr" "\\Bigr" "\\biggr" "\\Biggr" "\\right")
      (nil "\\big" "\\Big" "\\bigg" "\\Bigg")
      ("\\smallskip" "\\medskip" "\\bigskip")
      ("\\smallskipamount" "\\medskipamount" "\\bigskipamount")
      ("\\tiny" "\\scriptsize" "\\footnotesize" "\\small" "\\normalsize"
       "\\large" "\\Large" "\\LARGE" "\\huge" "\\Huge")
      ("\\scriptscriptstyle" "\\scriptstyle" "\\textstyle" "\\displaystyle")
      (nil "\\," "\\>" "\\;" "\\quad" "\\qquad")
      (nil "\\negthinspace" "\\negmedspace" "\\negthickspace") ;; amsmath
      ("\\thinmuskip" "\\medmuskip" "\\thickmuskip")
      )
    "An internal part of tex-increase.el.
Lists of size macros.  Each list has macros in increasing size,
so for example \"\\=\\bigl\" followed by \"\\=\\Bigl\" means Bigl is the
next bigger size.  nil means no macro at all, which is for
parens ( ) etc with no bigness macro."))

(eval-and-compile
  (defconst tex-increase-regexp-paren
    (eval-when-compile
      (concat "[][()<>]\\|\\\\[{}]\\|"
              (regexp-opt (append tex-increase-parens-left
                                  tex-increase-parens-right))
              "\\b"))
    "An internal part of tex-increase.el.
Regexp matching `tex-increase-parens-left' and `tex-increase-parens-right'
and paratheses characters."))

(defconst tex-increase-regexp
  ;; , ; > is not a word end for \b, so special case for them
  ;; (seemed ok in Emacs, but not XEmacs)
  (eval-when-compile
    (let ((lsts (mapcar (lambda (l) (remq nil l))
                        tex-increase-lists))
          lsts-for-paren lsts-not-paren)
      (dolist (l lsts)
        (if (string-match "\\\\[Bb]ig" (car l))
            (push l lsts-for-paren)
          (push l lsts-not-paren)))
      (concat "\\("                                            ;; match 1
              (regexp-opt (apply 'append lsts-not-paren) t)     ;; match 2
              "\\b" ;; whole macro name, not other things beginning \bigXXX
              "\\|\\\\[,>;]"
              "\\)"
              "\\|"
              "\\("
              (regexp-opt (apply 'append lsts-for-paren) t)    ;; match 3
              "\\b" ;; whole macro name, not other things beginning \bigXXX
              "\\)"
              "\\("
              tex-increase-regexp-paren
              "\\)?")))
  "An internal part of tex-increase.el.
Regexp matching any of the `tex-increase-lists' TeX macros,
optionally followed by a `tex-increase-regexp-paren'.

Match group 1 or 3 is the \\=\\bigl etc part to increase.")

(defun tex-increase-position-in-list (elem lst)
  "An internal part of tex-increase.el.
Return the position of ELEM in LST.  The first position in LST
is 0.  Elements are compared with `equal'."
  (let (ret)
    (dotimes (i (length lst))
      (if (equal elem (nth i lst))
          (setq ret i)))
    ret))

(defun tex-increase-replace (beg end str)
  "An internal part of tex-increase.el.
Replace buffer BEG to END with STR.
Replacements are made character by character so as to preserve
point and other marks within BEG to END.

This is good when point is in the middle of a TeX macro being
changed.  Probably only works properly on non-advance markers
\(like point, which is insertion type nil)."

  (save-excursion
    (goto-char end) 
    (setq end (point-marker))
    (let ((pos 0))
      (goto-char beg)
      (while (< (point) end)
        ;; insert before char to be replaced, then delete char
        (if (< pos (length str)) (insert (substring str pos (1+ pos))))
        (delete-region (point) (1+ (point)))
        (setq pos (1+ pos)))
      (if (< pos (length str)) (insert (substring str pos))))))

;;;###autoload
(defun tex-increase (&optional arg)
  "Increase a TeX \\bigl or similar at point.
ARG is how many increase steps to apply.  To decrease give a
negative increment or use `tex-decrease'.

The TeX macros which can be increased or decreased are

    \\=\\bigl ... \\=\\left        \\=\\ maths parens modifiers
    \\=\\bigr ... \\=\\right       /
    \\=\\big  ... \\=\\Bigg        maths modifiers
    \\=\\, \\=\\> \\=\\;               maths spacing
    \\=\\negthinspace .. \\=\\negthickspace       amsmath spacing
    \\=\\thinmuskip ... \\=\\thickmuskip          maths glues
    \\=\\scriptscriptstyle ... \\=\\displaystyle  maths style
    \\=\\tiny ... \\=\\Huge                       font
    \\=\\smallskip ... \\=\\bigskip               vertical space
    \\=\\smallskipamount ... \\=\\bigskipamount

A decrease of \\=\\big etc removes the modifier.  An increase of
a bare paren ( [ \\=\\{ \\=\\lvert \\=\\lbrace etc inserts an
initial \\=\\bigl or \\=\\bigr.

Increasing or decreasing this way is good if you forget the
sequence of Bigr and biggr or the names of the font sizes.

Maths medium space \\=\\> is inserted as \\=\\: in `latex-mode'
and derived modes, since that's what Lamport's LaTeX book
describes.  Plain TeX \\=\\> works too.

----
The tex-increase.el home page is
URL `http://user42.tuxfamily.org/tex-increase/index.html'"

  (interactive "P")
  (unless arg (setq arg 1))
  (require 'thingatpt)

  (save-restriction
    (narrow-to-region (line-beginning-position) (line-end-position))

    (while (/= 0 arg)
      (or (prog1 (or (thing-at-point-looking-at tex-increase-regexp)
                     (thing-at-point-looking-at tex-increase-regexp-paren))
            ;; if match ends exactly at point then check for a match after
            ;; point so as to prefer that
            (when (and (= (point) (match-end 0))
                       (not (match-beginning 3)))
              (looking-at tex-increase-regexp)
              (looking-at tex-increase-regexp-paren)))
          (error "Nothing to %s" (if (> arg 0) "increase" "decrease")))

      (if (or (match-beginning 1) (match-beginning 3))
          (let (pos lst)
            (dolist (l tex-increase-lists)
              (unless pos
                (setq pos (tex-increase-position-in-list
                           (or (match-string 1) (match-string 3))
                           l))
                (setq lst l)))
            (when pos
              (setq pos (+ pos (if (> arg 0) 1 -1)))
              (when (or (< pos 0)
                        (>= pos (length lst)))
                (error "Nothing more for %s"
                       (or (match-string 1) (match-string 3))))

              (let ((rep (nth pos lst)))
                ;; maths medium space \: in latex, though \> still works
                (when (and (equal rep "\\>")
                           (derived-mode-p 'latex-mode))
                  (setq rep "\\:"))

                (tex-increase-replace (or (match-beginning 1) (match-beginning 3))
                                      (or (match-end 1) (match-end 3))
                                      (or rep "")))))

        ;; initial increase for ( ) etc
        (if (< arg 0)
            (error "Nothing more for %s" (match-string 0))
          (save-excursion
            (goto-char (match-beginning 0))
            (insert (if (or (member (match-string 0) tex-increase-parens-left)
                            (member (match-string 0) '("(" "[" "\\{" "<")))
                        "\\bigl" "\\bigr")))))

      (setq arg (- arg (if (> arg 0) 1 -1))))))

;;;###autoload
(defun tex-decrease (&optional arg)
  "Decrease a TeX \\bigl or similar at point.
ARG is how many decreases to apply.
See `tex-increase' for details."
  (interactive "P")
  (unless arg (setq arg 1))
  (tex-increase (- arg)))

(provide 'tex-increase)

;;; tex-increase.el ends here
