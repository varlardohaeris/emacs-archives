;;; nroff-filladapt.el --- nroff comment prefixes for filladapt

;; Copyright 2007, 2009, 2011, 2013, 2014, 2015 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 5
;; Keywords: wp, filladapt
;; URL: http://user42.tuxfamily.org/nroff-filladapt/index.html
;; EmacsWiki: NroffMode

;; nroff-filladapt.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; nroff-filladapt.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; This is a spot of code to help filladapt work with the various possible
;; nroff comment prefixes, such as \" .\" and '''.  See the docstring in
;; `nroff-filladapt-setups' below for the details.

;;; Emacsen:

;; Designed for Emacs 21 and up.  Works in XEmacs 21.

;;; Install
;;
;; Put nroff-filladapt.el in one of your `load-path' directories and add to
;; your .emacs
;;
;;     (autoload 'nroff-filladapt-setups "nroff-filladapt")
;;     (add-hook 'nroff-mode-hook 'nroff-filladapt-setups)
;;
;; You must enable filladapt separately, as and when wanted.  If you want it
;; on by default then
;;
;;     (add-hook 'nroff-mode-hook 'turn-on-filladapt-mode)
;;
;; There's autoload cookies for the function and some custom options if you
;; install via `M-x package-install' or know `update-file-autoloads'.  Then
;; just `add-hook' or customize as desired.

;;; History:
;;
;; Version 1 - the first version.
;; Version 2 - tweak auto-fill-inhibit-regexp to what's been adopted in emacs23
;; Version 3 - quieten byte compiler on filladapt variables
;; Version 4 - token-match and token-conversion can be set globally
;; Version 5 - new email


;;; Code:

;; in filladapt.el, declare here to quieten the byte compiler
(defvar filladapt-token-table)
(defvar filladapt-token-match-table)
(defvar filladapt-token-conversion-table)

;;;###autoload
(defun nroff-filladapt-setups ()
  "Setup filladapt to recognise nroff comments.
These setups let filladapt \\[fill-paragraph] and friends work
with the various comment syntax forms used by nroff,

    \\=\\\"
    .\\=\\\"
    '''

Filladapt is not turned on as such, that's left to your usual
setups and preferences.

`fill-paragraph-handle-comment' is forcibly turned off, so as to
use filladapt's determined prefix, but also because it doesn't do
the right thing in Emacs 22.1 (picks just the \\=\\\" part of of
a .\\=\\\" form and so fills only one line).

As a bonus, `paragraph-separate' is extended to let commented out
blank lines separate paragraphs, which is how filladapt will
treat them.  `comment-start-skip' is extended with start of line
forms like .\\=\\\" etc.  And `auto-fill-inhibit-regexp' is setup
to leave directives (non-comment directives) alone.

Theses changes are only applied buffer-local, since they're
specific to nroff.  For example \\=\\\" means other things in
various programming languages and so shouldn't be a prefix
everywhere.

If filladapt is not available then `nroff-filladapt-setups' skips
those setups (just doing the paragraph separator etc).

----
The nroff-filladapt home page is
URL `http://user42.tuxfamily.org/nroff-filladapt/index.html'"

  ;; The possible forms for nroff/troff/groff comments are
  ;;
  ;;     \"         although it inserts a blank line
  ;;     .\"        a bogus directive doesn't
  ;;     .  \"      whitespace allowed after the .
  ;;     '\"        and ' can be used instead of .
  ;;     '''        another conventional bogus directive
  ;;     \#         groff also has # instead of "
  ;;     .\#        and as a directive etc

  ;; `paragraph-separate' here is:
  ;;     blank line
  ;;     comment directives above with only whitespace to eol
  ;;     any directive .X, 'X, etc, other than the comment ones
  ;;
  ;; The regexp is a little hairy, breaking out the nesting makes it
  ;; clearer; it's any of the following as separators
  ;;
  ;;     [ \t\f]*$                     blank line
  ;; second line:
  ;;     \"[ \t]*$                     \"
  ;;     \#[ \t]*$                     \#
  ;;     '''[ \t]*$                    '''
  ;; third line:
  ;;     [.'][ \t]*$                   .    filler
  ;;     [.'][ \t]*[^\\]               .X   non-comment directive
  ;;     [.'][ \t]*[\\]$               .\   non-comment directive
  ;;     [.'][ \t]*[\\][^"#]           .\X  non-comment directive
  ;;     [.'][ \t]*[\\]["#][ \t]*$     .\" or .\# blank to eol
  ;;
  (set (make-local-variable 'paragraph-separate)
       "[ \t\f]*$\
\\|\\(\\\\[#\"]\\|'''\\)[ \t]*$\
\\|[.'][ \t]*\\($\\|[^'\\ \t]\\|\\\\\\($\\|[^#\"]\\|[#\"][ \t]*$\\)\\)\
")
  (set (make-local-variable 'paragraph-start)
       paragraph-separate)

  ;; `comment-start-skip' adding .\" and ''' to what nroff-mode already has.
  ;; This doesn't end up doing quite the right thing as of Emacs 22.1,
  ;; suspect comment-search-forward ends up preferring \" in the middle of
  ;; the line over .\" at the start.
  (set (make-local-variable 'comment-start-skip)
       "\\(\\(^[.'][ \t]*\\)?\\\\[\"#]\\|^'''\\)[ \t]*")

  ;; Don't auto-fill directive lines starting . or ' since they normally
  ;; have to be one line.  But do auto-fill comments .\" .\# and '''
  ;;
  ;; Comment directives (those starting . or ') are [.'][ \t]*\\[#"] or a
  ;; ''', and this regexp is everything except those.  So [.'] followed by
  ;; not backslash and not ' or followed by backslash but then not # or "
  ;;
  ;; Emacs 23 has adopted this setting as a builtin.
  ;;
  (set (make-local-variable 'auto-fill-inhibit-regexp)
       "[.'][ \t]*\\([^ \t\\']\\|\\\\[^#\"]\\)")

  ;; Must load filladapt to add to its variables, or if it's not available
  ;; then do nothing.  xemacs21 `require' doesn't have a NOERROR arg, so
  ;; test with `locate-library' instead.
  (when (or (featurep 'filladapt)
            (and (locate-library "filladapt")
                 (require 'filladapt)))

    ;; `fill-paragraph-handle-comment' interferes with filladapt filling of
    ;; .\" lines, so turn if off.
    ;;
    ;; As of Emacs 22.1 it seems `fill-comment-paragraph' decides the \" is
    ;; the comment part (true enough, but it's the entire .\" which is
    ;; really wanted) and just splits a single line instead of a whole
    ;; paragraph, or something like that.
    ;;
    (if (eval-when-compile
          (boundp 'fill-paragraph-handle-comment)) ;; new in emacs22
        (set (make-local-variable 'fill-paragraph-handle-comment) nil))

    ;; The . and ' controls only work at the start of a line, but no need to
    ;; enforce that for prefix finding, since could be filling a
    ;; commented-out comment for instance.
    ;;
    (make-local-variable 'filladapt-token-table)
    (add-to-list 'filladapt-token-table
                 '("\\([.']?[ \t]*\\)?\\\\[\"#]" nroff-comment)
                 t) ;; append
    (add-to-list 'filladapt-token-table
                 '("'''" nroff-comment)
                 t) ;; append
    (add-to-list 'filladapt-token-match-table
                 '(nroff-comment nroff-comment))
    (add-to-list 'filladapt-token-conversion-table
                 '(nroff-comment . exact))))

;;;###autoload
(custom-add-option 'nroff-mode-hook 'nroff-filladapt-setups)

;; with the setups here filladapt is a good for nroff-mode, so show it
;;;###autoload
(custom-add-option 'nroff-mode-hook 'turn-on-filladapt-mode)

;; LocalWords: filladapt filladapt's nroff docstring el

(provide 'nroff-filladapt)

;;; nroff-filladapt.el ends here
