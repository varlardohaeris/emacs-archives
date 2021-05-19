;;; sentence-spacing.el --- two spaces for sentence end

;; Copyright 2016, 2017, 2020 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 3
;; Keywords: wp
;; URL: http://user42.tuxfamily.org/sentence-spacing/index.html

;; sentence-spacing.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; sentence-spacing.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a spot of code to fix sentence spacing in text, ensuring two
;; spaces between sentences.  Such spacing is standard, and aids readability
;; by giving a visual separation corresponding to logical separation of
;; ideas in sentences.
;;
;; `M-x sentence-spacing' fixes the current buffer, eg. for bad text from
;; elsewhere.
;;
;; Function `sentence-spacing-display' fixes the current buffer, even when
;; read-only.  This is good in the hook of a viewing mode.  It can be used
;; interactively `M-x sentence-spacing-display' for a one-off fix too.
;;
;; HTML viewing usually needs fixing since HTML is notorious for lacking a
;; proper way to indicate sentence ends.  In emacs-w3m, this can be done
;; from w3m-display-hook.  eval-after-load is because the hook doesn't start
;; empty.
;;
;;   (eval-after-load "w3m"
;;     '(add-hook 'w3m-display-hook 'sentence-spacing-display))
;;
;; Man page or Perl POD viewing ought not need spacing fixes, since nroff
;; and the POD tools put through the right thing, but an author might have
;; failed to do so in the source, and a few manpage conversion tools go out
;; of their way to do the wrong thing.  The source would be the place to fix
;; it for everyone, but just the display is helped from a hook or by
;; `M-x sentence-spacing-display' until then.
;;
;; Gnus mail and news articles can be fixed from the article "washing" hook.
;; This only changes the display, not the underlying message or anything
;; saved.  It's disappointingly common to see posters who should know better
;; only putting one space, and HTML mail (generally the domain of the
;; spammer of course) is nearly certain to be wrong.
;;
;;   (add-hook 'gnus-part-display-hook 'sentence-spacing-display)
;;
;; Some English abbreviations are recognised as not the end of a sentence,
;; but in general there's always some ambiguity.  The attempt here is to
;; make a best guess.  Perhaps some configurations could give sentence end
;; rules, desired abbreviations, etc.

;;; Install:

;; Put sentence-spacing.el in one of your `load-path' directories and add
;; to your .emacs
;;
;;     (autoload 'sentence-spacing         "sentence-spacing" nil t)
;;     (autoload 'sentence-spacing-display "sentence-spacing" nil t)
;;
;; There's autoload cookies below for these, if you install via
;; `M-x package-install' or know how to use `update-file-autoloads'.

;;; Emacsen:

;; Designed for Emacs 21 up.
;; Doesn't work in XEmacs 21 (want [[:alpha:]] etc).

;;; History:
;; 
;; Version 1 - the first version
;; Version 2 - tweak for single-letter initials
;; Version 3 - don't change dots without alnum before them

;;; Code:

(eval-when-compile
  (unless (fboundp 'ignore-errors)
    (require 'cl))) ;; for `ignore-errors' macro

(defconst sentence-spacing-abbreviations
  '("Cf" "cf" "Ie" "ie" "i.e" "Vol" "vol" "vs"
    "p" "pp"  ;; page or pages
    "Dr" "Mr" "Mrs" "Ms")
  "An internal part of sentence-spacing.el.
List of words which are abbreviations, so a following \".\" is
not the end of a sentence.")

;; ?\u2018
;; \u2019\u201C
;; ?\u201D
;; (decode-char 'ucs 8216)  ;; single quote left
;; (decode-char 'ucs 8220)  ;; double quote left
                                     
(eval-and-compile
  (defconst sentence-spacing-quotes-and-parens
    (eval-when-compile
      (concat "])}\"'"
              (ignore-errors
                (string (decode-char 'ucs 8217) ;; single quote right
                        (decode-char 'ucs 8221) ;; double quote right
                        ))))
    "An internal part of sentence-spacing.el.
Characters which are quote marks or closing parentheses."))

(defconst sentence-spacing-regexp
  (eval-when-compile
    (concat "[.?!]"
            "[" sentence-spacing-quotes-and-parens ".?!]*"
            "\\( \\)[^[:space:][:lower:]]"))
  "An internal part of sentence-spacing.el.
This pattern is a full stop with single space after it, possibly
with quotes and parens after the full stop.")

;;;###autoload
(defun sentence-spacing ()
  "Fix sentence spacing in the current buffer.
Sentences should end with two spaces.  The end of a sentence is
\".\", \"?\" or \"!\" and the next letter not lower case.  If
only one space then an extra is added there.

Some English abbreviations like \"Mr.\" or \"Mrs.\" are
recognised as not the end of a sentence.  A single capital letter
is an abbreviation, for people's initials.

Some abbreviations are ambiguous, for example \"St.\" might be
\"Saint\" which is not the end of a sentence, or it might be
\"Street\" which is the end of a sentence.  There's no
customization for what to prefer yet."

  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (while (re-search-forward sentence-spacing-regexp nil t)
        (let ((beg       (match-beginning 0))
              (beg-space (match-beginning 1)) ;; before the space
              punct-start
              punct-str
              before-word)
          (goto-char (match-end 1)) ;; after the space
          (save-excursion
            (goto-char beg)         ;; before full stop
            (skip-chars-backward sentence-spacing-quotes-and-parens)
            (setq punct-start (point))
            (setq punct-str   (buffer-substring punct-start beg-space))
            (skip-chars-backward ".[:alnum:]")
            (setq before-word (buffer-substring (point) punct-start)))

          (cond
           ;; no word before, just .!? is not sentence end
           ((equal "" before-word))

           ;; full stop might be abbreviation
           ((and (equal "." punct-str)
                 (or
                  ;; single capital letter not sentence end, like TeX
                  (string-match "\\(\\`\\|\\.\\)[[:upper:]]\\'" before-word)
              
                  ;; list of abbreviations
                  (member before-word sentence-spacing-abbreviations))))

           ;; numbered section like "Chap. 1" or "Vol. 1" not sentence end
           ((and (string-match "\\`[[:alpha:]]\\'" before-word)
                 (looking-at "\\`[[:digit:]]")))

           (t
            (insert " "))))))))

;;;###autoload
(defun sentence-spacing-display (&rest args)
  "Fix sentence spacing in the current buffer.
This is designed for use from hooks such as `w3m-display-hook' or
`gnus-part-display-hook'.

Spacing is fixed as per `sentence-spacing', but any
`buffer-read-only' is ignored, and the buffer-modified state is
left unchanged.  ARGS are ignored, for the benefit of hooks like
`w3m-display-hook' which give an argument.

It doesn't matter if this function runs multiple times from a
hook (due to revisiting or some such).  The sentence spacing
fixes will be done the first time then nothing further changed."

  (interactive)
  (let ((inhibit-read-only t)
        (old-modified (buffer-modified-p)))
    (sentence-spacing)
    (set-buffer-modified-p old-modified)))

;;;###autoload
(defun sentence-spacing-region (beg end)
  ;; checkdoc-params: (beg end)
  "Fix sentence spacing in the current region.
Apply the changes of `sentence-spacing' just to the region
between point and mark."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (sentence-spacing))))

(provide 'sentence-spacing)

;;  LocalWords:  parens customization

;;; sentence-spacing.el ends here
