;;; mailcap-mode.el --- mailcap file editing mode

;; Copyright 2013, 2015, 2017, 2019 Kevin Ryde
;;
;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 5
;; Keywords: wp, mailcap
;; EmacsWiki: Mailcap
;; URL: http://user42.tuxfamily.org/mailcap-mode/index.html

;; mailcap-mode.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; mailcap-mode.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This is a major mode for editing /etc/mailcap, ~/.mailcap and similar
;; files.  See the `mailcap-mode' docstring for details.

;;; Emacsen:
;;
;; Designed for Emacs 20 and up and XEmacs 21 and up.

;; Install:
;;
;; To make `mailcap-mode' available put mailcap-mode.el in one of your
;; `load-path' directories and in your .emacs add
;;
;;     (autoload 'mailcap-mode "mailcap-mode" nil t)
;;
;; To use `mailcap-mode' automatically on mailcap and .mailcap files as per
;; the RFC specified ~/.mailcap, /etc/mailcap, /usr/etc/mailcap,
;; /usr/local/etc/mailcap, add the following in your .emacs
;;
;;     (add-to-list 'auto-mode-alist '("/\\.?mailcap\\'" . mailcap-mode))
;;
;; In Debian, files /usr/lib/mime/packages/foo are mailcap fragments, and in
;; the sources those files are debian/mime or debian/packagename.mime.
;; These can be handled with
;;
;;     (add-to-list 'auto-mode-alist '("/usr/lib/mime/packages/[^\/]+\\'" . mailcap-mode))
;; 
;;     (add-to-list 'auto-mode-alist '("/debian/\\([^/]*\\.\\)?mime\\'" . mailcap-mode))
;;
;; There's autoload cookies for all of the above if you use
;; `M-x install-package' or know `update-file-autoloads' and friends.
;;
;; If you have Debian packagename.mime files in other directories then you
;; might like *.mime anywhere, not just under a debian subdir,
;;
;;    (add-to-list 'auto-mode-alist '("[/.]mime\\'" . mailcap-mode))
;;
;; The only caveat would be that Gnome /usr/share/mime-info/gnome-vfs.mime
;; would be matched by this but it's not a mailcap (closer to mime.types).
;; If you never visit or never edit that then it doesn't matter.

;;; History:
;; 
;; Version 1 - the first version
;; Version 2 - add info-lookup-symbol
;; Version 3 - correction to ispell, "x11-bitmap" can't be in localwords
;; Version 4 - new email
;; Version 5 - docstring typo

;;; Code:

;; cf RFC 1524 errata
;; http://www.rfc-editor.org/errata_search.php?rfc=1524
;; Fixes example in appendix B which had flowed to 78 columns and so split
;; some comment lines.
;;
;; cf vim
;; /usr/share/vim/vim74/ftplugin/mailcap.vim
;; /usr/share/vim/vim74/syntax/mailcap.vim
;;
;; cf vile has mailcap keywords in mailcap.key,
;; http://fossies.org/linux/misc/vile-9.8k.tgz:a/vile-9.8k/filters/mailcap.key


(defvar ispell-buffer-session-localwords) ;; ispell.el

;;-----------------------------------------------------------------------------
;; compatibility

(eval-when-compile
  (defmacro mailcap-mode--prog-mode-if-available (defn)
    "Mangle a `define-derived-mode' form to remove `prog-mode' if unavailable.
This is an internal part of mailcap-mode.el and doesn't exist
after byte compiling.

DEFN is a form (define-derived-mode mailcap-mode prog-mode ...).
If `prog-mode' is not available then change it to `fundamental-mode'.
`prog-mode' is new in Emacs 24."

    (unless (fboundp 'prog-mode)
      (setq defn (copy-sequence defn))
      (setcar (cdr (cdr defn)) 'fundamental-mode))
    defn))

;;-----------------------------------------------------------------------------
;; font-lock

(eval-and-compile
  (defconst mailcap-mode-flag-keywords
    '("copiousoutput"
      "needsterminal")
    "An internal part of mailcap-mode.el.
List of mailcap \"flag\" keywords.
These are optional parameters which don't take an \"=value\".

A draft update to RFC 1524 from May 1994
URL `http://www.ietf.org/archive/id/draft-borenstein-mailcap-00.ps'
included a \"needsx11\" flag.  Was that draft ever adopted?
Suspect not, instead test=test -n \"$DISPLAY\" for X.  So omit
\"needsx11\" for now."))

(eval-and-compile
  (defconst mailcap-mode-fieldname-keywords
    '("compose"
      "composetyped"
      "description"
      "edit"
      "nametemplate"
      "print"
      "test"
      "textualnewlines"
      "x11-bitmap"
      "priority")

    "An internal part of mailcap-mode.el.
List of mailcap \"fieldname\" keywords.
These are optional parameters which take an \"=value\" part.

\"priority\" is a Debian extension used in fragments
/usr/lib/mime/packages/foo as for example \"priority=9\".  It
establishes an order among entries from different packages when
the \"update-mime\" program generates /etc/mailcap.  It's
included here for convenience when editing Debian bits."))

(defconst mailcap-mode-keywords
  (eval-when-compile (append mailcap-mode-flag-keywords
                             mailcap-mode-fieldname-keywords))
    "An internal part of mailcap-mode.el.
A list of all optional parameter keywords, so both
`mailcap-mode-flag-keywords' and
`mailcap-mode-fieldname-keywords'.")

(defun mailcap-mode-first-line-p ()
  "An internal part of mailcap-mode.el.
Return non-nil if point is somewhere in the first line of a
mailcap entry.  Return non-nil for a blank line or comment line
too.

This means the current line is either not preceded by a
backslash-newline continuation, or it is but only # comments.

    # a comment \\=\\
    text/plain; cat %s \\=\\
    # part of command ; \\=\\
    copiousoutput"

  ;; Perhaps an re-search-backward like
  ;;     (concat "^[ \t]*[^#].*\\\\n"       ;; non-# continuation
  ;;             "\\(^[ \t]*#.*\\\\\n\\)*"  ;; # continuation
  ;;             "\\="                      ;; ending at current point
  ;; But would that scan back through all lines before deciding no match?

  (save-excursion
    (let ((ret t))
      (while (and
              ;; loop while more preceding lines, stop at `bob'
              (= 0 (forward-line -1))

              ;; loop while line ends with backslash-newline
              (looking-at "^[ \t]*\\(#\\)?.*\\\\\n")

              ;; If # matched then it's a comment line.  So ret=true still and
              ;; keep looping.
              ;; If # not matched then it's a non-comment line.  So the
              ;; original point was not in the first line of the entry, set
              ;; ret=nil and stop looping.
              (setq ret (match-beginning 1))))
      ret)))
        
(defun mailcap-mode-font-lock-leading-whitespace (limit)
  "An internal part of mailcap-mode.el.
This function is designed for use as font lock matcher.
It matches leading whitespace on a mailcap entry.

If some leading whitespace is found before LIMIT then set match 0
to its extent, move point to just after, and return non-nil.  If
no such leading whitespace found then return nil."

  (let (found)
    (while (and (setq found (re-search-forward "^[ \t]+" limit t))
                (not (save-match-data (mailcap-mode-first-line-p)))))
    found))

(defun mailcap-mode-font-lock-comment (limit)
  "An internal part of mailcap-mode.el.
This function is designed for use as font lock matcher.
It matches a comment line, taking care not to match # characters
within a mailcap entry.

If a comment is found before LIMIT then set match 1 to the # or
repeated ###, and match 2 to the rest of the line, move point to
just after, and return non-nil.  If no comment found before LIMIT
then return nil."

  (let (found)
    (while (and (setq found (re-search-forward "^[ \t]*\\(#+\\)\\(.*\\)$"
                                               limit t))
                (not (save-match-data (mailcap-mode-first-line-p)))))
    found))

(defconst mailcap-mode-font-lock-keywords
  `(
    ;; Warning face for leading whitespace since the RFC disallows it.
    ;; `trailing-whitespace' face since bad leading is similar to bad
    ;; trailing.
    ;;
    ;; Emacs 20 and XEmacs 21 don't have face `trailing-whitespace'.  If the
    ;; user hasn't created it then use `font-lock-warning-face' instead.
    ;; However `font-lock-warning-face' doesn't set the background which
    ;; makes it almost invisible on whitespace (only the cursor goes red).
    ;;
    ;; `trailing-whitespace' face not in emacs20,xemacs21
    (mailcap-mode-font-lock-leading-whitespace
     . ,(if (facep 'trailing-whitespace)
            ''trailing-whitespace
          'font-lock-warning-face))

    ;; Comment body, with whole of ###### run reckoned the delimiter.
    ;; `font-lock-comment-delimiter-face' only exists in emacs22 up.  Use it
    ;; if available, though it's the same as `font-lock-comment-face' by
    ;; default.
    ;;
    (mailcap-mode-font-lock-comment
     (1 ,(if (boundp 'font-lock-comment-delimiter-face)
             'font-lock-comment-delimiter-face ;; new in emacs22
           'font-lock-comment-face)) ;; emacs21,xemacs21
     (2 font-lock-comment-face))

    ;; The keywords highlighted are only the RFC specified ones, so
    ;; possible spelling errors in the optional fields don't look valid.
    ;;
    ;; A matcher function could restrict to match only at the start of the
    ;; optional fields, not in the mime type or the commands or the field
    ;; value parts.  In practice the `mailcap-mode-flag-keywords' such as
    ;; "copiousoutput" are unusual enough not to match elsewhere and
    ;; `mailcap-mode-fieldname-keywords' are matched as "test=" etc and an
    ;; "=" like that is not normally found elsewhere.

    ;; "flag" field words, without "=".
    (,(eval-when-compile (concat "\\b"
                                 (regexp-opt mailcap-mode-flag-keywords
                                             t) ;; with group \( \)
                                 "\\b"))
     . font-lock-keyword-face)

    ;; "namedfield" words with "=", and fontifying only the word not the "=".
    (,(eval-when-compile (concat "\\b"
                                 (regexp-opt mailcap-mode-fieldname-keywords
                                             t) ;; with group \( \)
                                 "="))
     1 font-lock-keyword-face)
    )
  "`font-lock-keywords' for `mailcap-mode'.")

;;-----------------------------------------------------------------------------
;; ispell

(defconst mailcap-mode-ispell-localwords
  '("copiousoutput"
    "needsterminal"
    ;; "compose"
    "composetyped"
    ;; "description"
    ;; "edit"
    "nametemplate"
    ;; "print"
    ;; "test"
    "textualnewlines"
    ;; "x11-bitmap" excluded because ispell doesn't allow digit 1 of "x11",
    ;; nor a hyphen of "x11-bitmap", so just "bitmap"
    "bitmap"
    ;; "priority"
    )
  "An internal part of mailcap-mode.el.
List of words to add to `ispell-buffer-session-localwords' as
correct for mailcap.

This is `mailcap-mode-keywords' without the ordinary English
words like \"test\", and with \"x11-bitmap\" reduced to just
\"bitmap\" since ispell doesn't accept digits or hyphens (nor
does aspell).")


;;-----------------------------------------------------------------------------
;; info-lookup-symbol

;; Mailcap is shell commands, so follow `sh-mode' info bits.
;; Don't think there's any info documents for the mime type names or
;; mailcap keywords.

(eval-after-load "info-look"
  '(progn
     ;; Emacs 21 info-look.el didn't have any sh-mode setups, so check
     ;; before copying.
     (if (info-lookup->mode-value 'symbol 'sh-mode)
         (info-lookup-maybe-add-help
          :topic       'symbol
          :mode        'mailcap-mode
          :regexp      (info-lookup->regexp     'symbol 'sh-mode)
          :parse-rule  (info-lookup->parse-rule 'symbol 'sh-mode)
          :other-modes '(sh-mode)))

     ;; As of Emacs 24.3 info-look.el doesn't have any `file' bits for
     ;; sh-mode.  If it gains them in the future or if the user has added
     ;; some then use those in mailcap-mode too.
     (if (info-lookup->mode-value 'file 'sh-mode)
         (info-lookup-maybe-add-help
          :topic       'file
          :mode        'mailcap-mode
          :regexp      (info-lookup->regexp     'file 'sh-mode)
          :parse-rule  (info-lookup->parse-rule 'file 'sh-mode)
          :other-modes '(sh-mode)))))

;;-----------------------------------------------------------------------------
;; mailcap-mode

;;;###autoload (autoload 'mailcap-mode "mailcap-mode" "Major mode for editing /etc/mailcap and similar" t)

(mailcap-mode--prog-mode-if-available
 (define-derived-mode mailcap-mode prog-mode "Mailcap"
   "Major mode for editing /etc/mailcap and similar.
The mailcap file format is described in RFC 1524.

Comments are #, and `comment-style' is set to `plain' so that
each # is at the start of the line as required by the RFC.

`auto-fill-inhibit-regexp' prevents filling of non-comment
mailcap lines.  Mailcap entries can be continued across newline
with a \\=\\ before the newline but there's nothing automatic for
that and in practice longish lines are usual.

`font-lock-mode' setups fontify comments, the standard keywords,
and put a warning face on leading whitespace (since it's not
permitted by the RFC).

\\{mailcap-mode-map}

See variables `mailcap-mode-ac-source-mime-types' and
`mailcap-mode-ac-source-keywords' for tie-ins to
`auto-complete-mode'.  Refer to the auto-complete manual on how
to enable that mode and add to `ac-sources'.

----
mailcap.el (from Gnus) is a good way to parse mailcap files for
use in Emacs.

extview.el, mailcap-view.el and `org-open-file' can view
according to mailcap.el info, with various levels of automation.

The mailcap-mode.el home page is
URL `http://user42.tuxfamily.org/mailcap-mode/index.html'
----"

   (set (make-local-variable 'font-lock-defaults)
        '(mailcap-mode-font-lock-keywords
          t     ;; only keywords, no syntactic fontification (of strings etc)
          t     ;; case-fold, since field keywords are case-insensitive
          nil)) ;; no changes to syntax table

   ;; Mailcap keywords reckoned correct for ispell.
   ;; If ispell-buffer-session-localwords is bound then use its value.
   ;; If not then just set it since the defvar in ispell.el is nil and so
   ;; don't need to worry about appending to it later.
   (set (make-local-variable 'ispell-buffer-session-localwords)
        (append mailcap-mode-ispell-localwords
                (and (boundp 'ispell-buffer-session-localwords)
                     ispell-buffer-session-localwords)))

   (set (make-local-variable 'comment-start) "#")
   (set (make-local-variable 'comment-end)   "")

   ;; Comments always start in column 0.
   (set (make-local-variable 'comment-style) 'plain)

   ;; FIXME: What does or should comment-indent-function do?
   ;; (set (make-local-variable 'comment-indent-function) (lambda () 0))

   ;; Match comments not starting in column 0 too in case somehow indented.
   (set (make-local-variable 'comment-start-skip) "^[ \t]*#+[ \t]*")

   ;; Inhibit auto-fill on non-comment lines, usually don't want to break at
   ;; an arbitrary point, not without carefully backslashing the newline.
   (set (make-local-variable 'auto-fill-inhibit-regexp) "[ \t]*[^#]")

   ;; Not sure whether mailcap files must end with a newline, but it seems
   ;; prudent to do so.
   (set (make-local-variable 'require-final-newline) 'ask)))

(defvar mailcap-mode-syntax-table) ;; quieten emacs20 byte compile
(modify-syntax-entry ?\\ "\\" mailcap-mode-syntax-table) ;; \ is an escape
(modify-syntax-entry ?\" "\"" mailcap-mode-syntax-table) ;; " strings
(modify-syntax-entry ?'  "\"" mailcap-mode-syntax-table) ;; ' strings
(modify-syntax-entry ?#  "<"  mailcap-mode-syntax-table) ;; comment start
(modify-syntax-entry ?\n ">"  mailcap-mode-syntax-table) ;; comment end


;; /etc/mailcap ~/.mailcap
;;;###autoload (add-to-list 'auto-mode-alist '("/\\.?mailcap\\'" . mailcap-mode))

;; /usr/lib/mime/packages/foo
;;;###autoload (add-to-list 'auto-mode-alist '("/usr/lib/mime/packages/[^\/]+\\'" . mailcap-mode))

;; debian/mime and debian/foo.mime
;;;###autoload (add-to-list 'auto-mode-alist '("/debian/\\([^/]*\\.\\)?mime\\'" . mailcap-mode))


;;-----------------------------------------------------------------------------
;; auto-complete.el tie-in -- mailcap-mode-ac-source-mime-types

(eval-and-compile
  (defconst mailcap-mode-whitespace-regexp
    "\\([ \t]\\|\\\\\n\\)*"
    "An internal part of mailcap-mode.el.
Regexp matching whitespace, which is reckoned as spaces, tabs and
backslashed newlines."))

(defconst mailcap-mode-unbackslashed-semicolon-regexp
  (eval-when-compile
    (concat "\\(^\\|[^\\]\\)\\(\\\\\\\\\\)*;"
            mailcap-mode-whitespace-regexp))
  "An internal part of mailcap-mode.el.
Regexp matching an unbackslashed ; and following whitespace.

This pattern matches a semicolon with no backslash, or an even
number of preceding backslashes.  It does not match a semicolon
with any odd number of backslashes.  Spaces and tabs following
the semicolon are included in the match so as to give the start
proper of the following field.")

(defun mailcap-mode-beginning-of-entry ()
  "An internal part of mailcap-mode.el.
Move point to the start of a mailcap entry.

This is the start of the current line, or the start of a previous
line if any backslash-newline continuations.

Backslash-newline at the end of a comment line is not a
continuation, unless it's a continuation from a non-comment
backslash-newline across a # within the entry, eg.

    text/plain; cat %s \\=\\
    # part of command ; \\=\\
    copiousoutput"

  (beginning-of-line)
  (let ((non-comment (point)))
    (while (equal ?\\ (char-before (1- (point))))
      (forward-line -1)
      (unless (looking-at comment-start-skip)
        (setq non-comment (point))))
    (goto-char non-comment)))

(defun mailcap-mode-beginning-of-mime-type-position ()
  "An internal part of mailcap-mode.el.
If point is in a mime type field of a mailcap entry then return
the position of the start of the mime type.  If not in such a
field then return nil.

If point is in some spaces and tabs at the start of the line then
return it unchanged.  This behaviour suits
`mailcap-mode-ac-source-mime-types'."

  (save-excursion
    (let ((orig-point (point)))
      (mailcap-mode-beginning-of-entry)
      ;; allow for leading whitespace, though that's not valid RFC syntax
      (looking-at mailcap-mode-whitespace-regexp)
      (goto-char (min orig-point (match-end 0)))
      ;; only in the mime type, so not if there's a semi before orig-point
      (and (not (re-search-forward mailcap-mode-unbackslashed-semicolon-regexp
                                   orig-point
                                   t))
           (point)))))

(defvar mailcap-mode-ac-source-mime-types

  ;; `mailcap-mime-types' returns its types in reverse order of the
  ;; mime.types files, or something like that.  Would auto-complete prefer
  ;; it sorted, or does its candidate scoring override that anyway?
  ;;
  '((depends    . (mailcap)) ;; mailcap.el
    (candidates . mailcap-mime-types)
    (prefix     . mailcap-mode-beginning-of-mime-type-position)
    (requires   . 1))

  "auto-complete.el source for mailcap mime type.
This is designed for use from `ac-sources' as for instance

    (add-to-list 'ac-sources 'mailcap-mode-ac-source-mime-types)

This source completes mime type names at the start of a mailcap
entry.  Type names are obtained from `mailcap-mime-types' (from
Gnus mailcap.el) which in turn reads /etc/mime.types and more.

The \"requires\" is 1 so that completion is offered after just 1
character typed.  This is convenient since a single character is
enough to start say text/ or image/.

\"requires\" can be reduced to 0 if desired for immediate
completion at the start of a line, though that would need an
explicit trigger (`ac-set-trigger-key').")


;;-----------------------------------------------------------------------------
;; auto-complete.el tie-in -- mailcap-mode-ac-source-keywords

(defun mailcap-mode-beginning-of-optional-field-position ()
  "An internal part of mailcap-mode.el.
If point is in an optional field of a mailcap entry then return
the position of the start of that field.

    text/plain; cat %s; copiousoutput; description=Plain text
                        ^              ^
                        start          start

If point is in the whitespace after a ; then it's returned unchanged.


    text/plain; cat %s;    copiousoutput
                        ^
                        point returned unchanged

If point is not in an optional field then return nil.  In
particular return nil if point is in the mime type or command
fields.  If the line is a comment then return nil too.

This behaviour is designed for `mailcap-mode-ac-source-keywords'."

  (save-excursion
    (let ((orig-point (point)))
      (mailcap-mode-beginning-of-entry)
      (and (not (looking-at comment-start-skip))
           (re-search-forward mailcap-mode-unbackslashed-semicolon-regexp
                              orig-point t 2) ;; past mime type and command
           (progn
             (while (re-search-forward mailcap-mode-unbackslashed-semicolon-regexp
                                       orig-point t))
             (point))))))

(defvar mailcap-mode-ac-source-keywords
  '((candidates . mailcap-mode-keywords)
    (prefix     . mailcap-mode-beginning-of-optional-field-position)
    (requires   . 1))
  "auto-complete.el source for mailcap field keywords.
This is designed for use from `ac-sources' as for instance

  (add-to-list 'ac-sources 'mailcap-mode-ac-source-keywords)

This source completes mailcap keywords such as \"needsterminal\"
at the start of an optional field (not in the mime type or
command).

The \"requires\" is 1 so completion is offered after just 1
character typed.  This is convenient since there's only a few
keywords.

\"requires\" can be reduced to 0 if desired to offer all
candidates at the start of a field, though that would need an
explicit trigger \(`ac-set-trigger-key').")


;;-----------------------------------------------------------------------------

(provide 'mailcap-mode)

;; LocalWords:  mailcap docstring el debian usr packagename copiousoutput
;; LocalWords:  unbackslashed whitespace eg needsterminal matcher aspell

;;; mailcap-mode.el ends here
