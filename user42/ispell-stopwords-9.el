;;; ispell-stopwords.el --- use perl POD "=for stopwords" in ispell

;; Copyright 2010, 2011, 2012, 2013, 2015 Kevin Ryde
;;
;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 9
;; Keywords: wp, ispell
;; URL: http://user42.tuxfamily.org/ispell-stopwords/index.html
;; EmacsWiki: InteractiveSpell
;;
;; ispell-stopwords.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; ispell-stopwords.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `ispell-stopwords' sets `ispell-words-keyword' to "=for stopwords" so
;; that Ispell will use a Perl Pod::Spell style
;;
;;     =for stopwords myword anotherword blahblah xyzzy
;;
;; as the local words.  See the `ispell-stopwords' docstring for more.

;;; Emacsen:

;; Designed for Emacs 20 up, works in XEmacs 21 too.

;;; Install:

;; To make `ispell-stopwords' available put ispell-stopwords.el in one of
;; your `load-path' directories and the following in your .emacs
;;
;;     (autoload 'ispell-stopwords "ispell-stopwords" nil t)
;;
;; There's an autoload cookie for the function and a custom option on
;; `perl-mode-hook' if you install via `M-x package-install' or know how to
;; use `update-file-autoloads'.

;;; History:

;; Version 1 - the first version
;; Version 2 - also don't send digits to ispell
;; Version 3 - also don't send "." dots to ispell
;; Version 4 - "=for :stopwords" too
;; Version 5 - also don't send "()," to ispell
;; Version 6 - also don't send "/" to ispell
;; Version 7 - also don't send "+" to ispell
;; Version 8 - suppress line breaks in "A" accept
;; Version 9 - new email

;;; Code:

;; Explicit dependency on advice.el since `ispell-stopwords-unload-function'
;; needs `ad-find-advice' macro when running not byte compiled and that
;; macro is not autoloaded.
(require 'advice)

;; Believe need defvar of `ispell-words-keyword' before making buffer local
;; bindings of it ...
(require 'ispell)

;;;###autoload
(defun ispell-stopwords ()
  "Set `ispell-words-keyword' to use \"=for stopwords\".
If the current `ispell-words-keyword' string is not in the buffer
and either

    =for stopwords
    =for :stopwords

is in the buffer then set `ispell-words-keyword' to that for the
per-file word list.

This function is designed for use in `perl-mode', `cperl-mode' or
`pod-mode' to have `ispell' use the same local words list as
Pod::Spell.  This function can be put in the mode hook
`perl-mode-hook' etc, or used interactively (\\[ispell-stopwords])
one-off or in other modes.

When stopwords is used a hack is applied to ispell.el to suppress
words with characters \"-0-9.,()\" in the stopwords list.
Pod::Spell requires such things explicitly listed, but Ispell
doesn't like them.  The error from ispell.el (Emacs 24.1 circa
2012) is

    \"Ispell and its process have different character maps\"

But this means almost any kind of Ispell error, not just charset
trouble.

Note that ispell.el requires an =for stopwords at the start of
each lines, so it cannot be a POD style multi-line paragraph,

    =for stopwords jkjk
    xyzzy                    <-- no good for ispell.el

Perhaps `ispell-stopwords' could help the filling, but for now a
hack is applied to `ispell-add-per-file-word-list' (key `A') to
have it make a long line, no filling or new =for.  Multiple
\"=for\" lines can be used if desired.

    =for stopwords jkjk

    =for stopwords xyzzy     <-- ok

The ispell-stopwords.el home page is
URL `http://user42.tuxfamily.org/ispell-stopwords/index.html'"

  (interactive)
  ;; case sensitive per `ispell-add-per-file-word-list'
  (when (let ((case-fold-search nil))
          (and (boundp 'ispell-words-keyword)   ;; not if unloaded
               (not (member ispell-words-keyword ;; not if already set
                            '("=for stopwords " "=for :stopwords ")))
               (not (save-excursion   ;; if current "LocalWords: " not found
                      (goto-char (point-min))
                      (search-forward ispell-words-keyword nil t)))
               (save-excursion        ;; but "=for stopwords" is found
                 (goto-char (point-min))
                 (re-search-forward "^\\(=for :?stopwords \\)" nil t))))
    (set (make-local-variable 'ispell-words-keyword)
         (match-string 1)) ;; "=for stopwords " or "=for :stopwords "
    (ispell-kill-ispell t)))

;;;###autoload
(custom-add-option 'cperl-mode-hook 'ispell-stopwords)
;;;###autoload
(custom-add-option 'perl-mode-hook  'ispell-stopwords)
;;;###autoload
(custom-add-option 'pod-mode-hook   'ispell-stopwords)

(defadvice ispell-send-string (around ispell-stopwords activate)
  "Don't send hyphenated etc \"=for stopwords\" to ispell."
  ;; args: (ispell-send-string STRING)

  ;; ispell doesn't like "-(),+" anywhere, or "0-9./" in the middle or at the
  ;; end.  ispell.el itself suppresses some words starting non-alpha.
  (unless (and (member ispell-words-keyword ;; not if already set
                       '("=for stopwords " "=for :stopwords "))
               (string-match "\\`@\\(.*[-(),+]\\|.+[0-9./]\\)"
                             (ad-get-arg 0))) ;; STRING
    ;; (message "send %S" (ad-get-arg 0))
    ad-do-it))

(defadvice ispell-add-per-file-word-list (around ispell-stopwords activate)
  "No line length limit for \"=for stopwords\"."
  (let ((fill-column (if (member ispell-words-keyword ;; not if already set
                                 '("=for stopwords " "=for :stopwords "))
                         99999
                       fill-column)))
    ad-do-it))

(defun ispell-stopwords-unload-function ()
  "Remove defadvice applied to `ispell-send-string'.
This is called by `unload-feature'."
  (when (ad-find-advice 'ispell-send-string 'around 'ispell-stopwords)
    (ad-remove-advice   'ispell-send-string 'around 'ispell-stopwords)
    (ad-activate        'ispell-send-string))
  (when (ad-find-advice 'ispell-add-per-file-word-list 'around 'ispell-stopwords)
    (ad-remove-advice   'ispell-add-per-file-word-list 'around 'ispell-stopwords)
    (ad-activate        'ispell-add-per-file-word-list))
  nil) ;; and do normal unload-feature actions too

;;-----------------------------------------------------------------------------
;; LocalWords: stopwords myword blahblah xyzzy docstring el charset jkjk
;; LocalWords: for's ok anotherword

(provide 'ispell-stopwords)

;;; ispell-stopwords.el ends here
