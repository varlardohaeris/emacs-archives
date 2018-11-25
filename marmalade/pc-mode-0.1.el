;;; pc-mode.el --- major mode for editing PC code,
;;; based on php-mode maintained by Turadg Aleahmad.

;; Copyright (C) 2007-2007 Liang Hongxin

;; Filename: pc-mode.el
;; Author: Honnix Liang <hxliang1982@gmail.com>
;; Keywords: pc languages
;; Version: 0.1
;; Created: 2007-03-27
;; Last Modified: 2007-03-27

(defconst pc-version "0.1"
  "PC Mode version number.")

;;; License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; Usage

;; Rename this file to pc-mode.el if it isn't already then place it in
;; your Emacs lisp path (eg. site-lisp) and add to your .emacs file:
;;   (require 'pc-mode)

;; If you want colorization, turn on global-font-lock or
;; add this to your .emacs:
;;   (add-hook 'pc-mode-user-hook 'turn-on-font-lock)

;; To use abbrev-mode, add lines like this:
;;   (add-hook 'pc-mode-user-hook
;;     '(lambda () (define-abbrev pc-mode-abbrev-table "ex" "extends")))

;; Since it inherits much functionality from c-mode, look there too
;;  Programming/Languages/C

;;; Commentary:

;; PC mode is a major mode for editing Ericsson PC and IPC source code.  It's
;; an extension of C mode; thus it inherits all C mode's navigation
;; functionality.  But it colors according to the PC grammar and indents
;; according to the PC coding guidelines.

;;; Contributors: (in chronological order)

;;; Changelog:

;; 0.1
;;   Created based on php-mode.

;;; Code:

(require 'font-lock)
(require 'cc-mode)
(require 'custom)
(eval-when-compile
  (require 'regexp-opt))

;; Local variables
(defgroup pc nil
  "Major mode for editing PC/IPC code."
  :prefix "pc-"
  :group 'languages)

;;;###autoload
(defcustom pc-file-patterns (list "\\.pc\\'" "\\.ipc\\'")
  "*List of file patterns for which to automatically invoke pc-mode."
  :type '(repeat (regexp :tag "Pattern"))
  :group 'pc)

(defcustom pc-mode-user-hook nil
  "List of functions to be executed on entry to pc-mode"
  :type 'hook
  :group 'pc)

(defcustom pc-mode-force-ericsson nil
  "Turning this on will force Ericsson coding rules on all PC/IPC files."
  :type 'boolean
  :group 'pc)

;; Note whether we're in XEmacs
(defconst xemacsp (string-match "Lucid\\|XEmacs" emacs-version)
  "Non nil if using XEmacs.")

;; It's a VERY VERY bad implementation. Right now, forget it.
;; (defun pc-indent-command ()
;;   (interactive)
;;   (make-local-variable 'tmp)
;;   (setq tmp kill-whole-line)
;;   (setq kill-whole-line t)
;;   (forward-line -1)
;;   (end-of-line)
;;   (newline)
;;   (push-mark nil t)
;;   (princ ";" (mark-marker))
;;   (c-indent-command)
;;   (forward-line 1)
;;   (c-indent-command)
;;   (forward-line -1)
;;   (beginning-of-line)
;;   (kill-line)
;;   (beginning-of-line-text)
;;   (setq kill-whole-line tmp)
;;   )

;; (defun pc-indent-region (start end &optional quiet)
;;   "Indent every line whose first char is between START and END inclusive.
;; Be silent about syntactic errors if the optional argument QUIET is non-nil."
;;   (save-excursion
;;     (goto-char start)
;;     ;; Advance to first nonblank line.
;;     (skip-chars-forward " \t\n")
;;     (beginning-of-line)
;;     (setq c-parsing-error
;; 	  (or (let ((endmark (copy-marker end))
;; 		    (c-parsing-error nil)
;; 		    ;; shut up any echo msgs on indiv lines
;; 		    (c-echo-syntactic-information-p nil))
;; 		(unwind-protect
;; 		    (progn
;; 		      (c-progress-init start end 'pc-indent-region)
;; 		      (while (and (bolp)
;; 				  (not (eobp))
;; 				  (< (point) endmark))
;; 			;; update progress
;; 			(c-progress-update)
;; 			;; skip blank lines
;; 			(skip-chars-forward " \t\n")
;; 			(beginning-of-line)
;; 			;; indent the current line
;; 			(pc-indent-command nil t)
;; 			(forward-line)))
;; 		  (set-marker endmark nil)
;; 		  (c-progress-fini 'pc-indent-region))
;; 		(c-echo-parsing-error quiet))
;; 	      c-parsing-error))))

;;;###autoload
(define-derived-mode pc-mode c-mode "PC"
  "Major mode for editing PC/IPC code.\n\n\\{pc-mode-map}"
  
  (setq comment-start "-- "
        comment-end   " --;"
        comment-start-skip "-- *"
;;         comment-start-skip "\\(--+\\|//+\\|/\\*+\\)\\s *"
        c-comment-start-regexp "-- *")
  
;;   (local-unset-key (kbd "TAB"))
;;   (local-set-key (kbd "TAB") 'pc-indent-command)

  (local-unset-key (kbd "C-c C-c"))

  ;;   (setq c-class-key pc-class-key)
  ;;   (setq c-conditional-key pc-conditional-key)

;;   (defvar pc-mode-syntax-table pc-mode-syntax-table)
  (defvar pc-mode-syntax-table
    (copy-syntax-table c-mode-syntax-table))
  
  ;; The above causes XEmacs to handle shell-style comments correctly,
  ;; but fails to work in GNU Emacs which fails to interpret \n as the
  ;; end of the comment.
  (if xemacsp (progn
		(modify-syntax-entry ?# "< b" pc-mode-syntax-table)
		(modify-syntax-entry ?\n "> b" pc-mode-syntax-table)
		))
  
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '((pc-font-lock-keywords-1
;;            pc-font-lock-keywords-2
           ;;            ;; Comment-out the next line if the font-coloring is too
           ;;            ;; extreme/ugly for you.
           ;;            pc-font-lock-keywords-3
           )
          nil				; KEYWORDS-ONLY
          T				    ; CASE-FOLD
          nil				; SYNTAX-ALIST
          nil				; SYNTAX-BEGIN
          (font-lock-syntactic-keywords . pc-font-lock-syntactic-keywords)))
  
;;   (setq font-lock-maximum-decoration t
;;         case-fold-search t		; PC/IPC vars are case-sensitive
;; 	imenu-generic-expression pc-imenu-generic-expression)
  
  ;; Do not force newline at end of file.
  (set (make-local-variable 'require-final-newline) nil)
  (set (make-local-variable 'next-line-add-newlines) nil)
  
  ;; PEAR coding standards
  (make-local-hook 'pc-mode-ericsson-hook)
;;   (add-hook 'pc-mode-ericsson-hook
;;             (lambda nil (set (make-local-variable 'tab-width) 4)) nil t)
;;   (add-hook 'pc-mode-ericsson-hook
;;             (lambda nil (set (make-local-variable 'c-basic-offset) 4)) nil t)
;;   (add-hook 'pc-mode-ericsson-hook
;;             (lambda nil (set (make-local-variable 'c-hanging-comment-ender-p) nil)) nil t)
;;   (add-hook 'pc-mode-ericsson-hook
;;             (lambda nil (set (make-local-variable 'indent-tabs-mode) nil)) nil t)
;;   (add-hook 'pc-mode-ericsson-hook
;;             (lambda nil (c-set-offset 'block-open' - )) nil t)
;;   (add-hook 'pc-mode-ericsson-hook
;;             (lambda nil (c-set-offset 'block-close' 0 )) nil t)
  
  (if pc-mode-force-ericsson
      (run-hooks 'pc-mode-ericsson-hook)
    (run-hooks 'pc-mode-user-hook)
    )
  )

;; Make pc-mode the default mode for PC/IPC source code buffers.
;;;###autoload
(let ((pc-file-patterns-temp pc-file-patterns))
  (while pc-file-patterns-temp
    (add-to-list 'auto-mode-alist
		  (cons (car pc-file-patterns-temp) 'pc-mode))
    (setq pc-file-patterns-temp (cdr pc-file-patterns-temp))))

;; Define function name completion function
;; (defun pc-complete-function ()
;;   "Complete the function name at the point from known PC internal functions."
;;   (interactive)
;;   (message "pc-complete-function not implemented yet")
;;   ;; how to read the list of functions?
;;   )

;; Absolutely not enough, because I'm not familiar with PC
;; right now.
(defconst pc-constants
  (eval-when-compile
    (regexp-opt
     '(;; core constants
       "TRUE" "FALSE" "NULL"
       )))
  "PC constants.")

(defconst pc-keywords
  (eval-when-compile
    (regexp-opt
     '("break" "continue" "declare" "do" "else"
       "for" "if" "include"
       "return" "switch" "default"
       "while" "throw" "catch" "try" "break" "continue"
       "finally")))
  "PC keywords.")

(defconst pc-identifiers
  (eval-when-compile
    '"[a-zA-Z\_\x7f-\xff][a-zA-Z0-9\_\x7f-\xff]*")
  "Characters in a PC identifier.")

(defconst pc-types
  (eval-when-compile
    (regexp-opt '("SET" "BOOLEAN" "IA5String" "INTEGER"
                  "VOID" "ANY" "CONST")))
  "PC types.")

;; (defconst pc-superglobals
;;   (eval-when-compile
;;     (regexp-opt '("_GET" "_POST" "_COOKIE" "_SESSION" "_ENV" "GLOBALS"
;; 		  "_SERVER" "_FILES" "_REQUEST")))
;;   "PC superglobal variables.")

;; Set up font locking
(defconst pc-font-lock-keywords-1
  (list
   ;; Fontify constants
   (cons
    (concat "\\<\\(" pc-constants "\\)\\>")
    'font-lock-constant-face)
   
   ;; Fontify keywords
   (cons
    (concat "\\<\\(" pc-keywords "\\)\\>")
    'font-lock-keyword-face)

   ;; Fontify types
   (cons
    (concat "\\<\\(" pc-types "\\)\\>")
    'font-lock-type-face)

;;    ;; Fontify identifiers
;;    (cons
;;     (concat "\\<\\(" pc-identifiers "\\)\\>")
;;     'font-lock-constant-face)

   ;; Fontify case and its following constant
   (list "\\<\\(case\\)\\>[ \t]*\\(-?\\(?:\\sw\\|\\s_\\)+\\)?"
	 '(1 font-lock-keyword-face) '(2 font-lock-constant-face t t))
   ;; This must come after the one for keywords and targets.
   '(":" ("^[ \t]*\\(\\(?:\\sw\\|\\s_\\)+\\)[ \t]*:[ \t]*$"
	  (beginning-of-line) (end-of-line)
	  (1 font-lock-constant-face)))
   
   ;; treat '__print' as keyword only when not used like a function name
;;    '("\\<__print\\s-*(" . default)
;;    '("\\<__print\\>" . font-lock-keyword-face)
   )
  "Subdued level highlighting for PC mode.")

;; (defconst pc-font-lock-keywords-2
;;   (append
;;    pc-font-lock-keywords-1
;;    (list
    
;;     ;; class declaration
;;     '("\\<\\(class\\|interface\\)[ \t]*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
;;       (1 font-lock-keyword-face) (2 font-lock-type-face nil t))
;;     ;; handle several words specially, to include following word,
;;     ;; thereby excluding it from unknown-symbol checks later
;;     ;; FIX to handle implementing multiple
;;     ;; currently breaks on "class Foo implements Bar, Baz"
;;     '("\\<\\(new\\|extends\\|implements\\)\\s-+\\$?\\(\\(?:\\sw\\|\\s_\\)+\\)"
;;       (1 font-lock-keyword-face) (2 font-lock-type-face))

    ;; function declaration
;;     '("\\<\\(function\\)\\s-+&?\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*("
;;       (1 font-lock-keyword-face)
;;       (2 font-lock-function-name-face nil t))
    
    ;; const string declaration
;;     '("\\<\\(\"\\)\\>[ \t]*\\(-?\\(?:\\sw\\|\\s_\\)+\\)?\\<\\(\"\\)\\>"
;;       (1 font-lock-constant-face))
    
;;     ;; class hierarchy
;;     '("\\(self\\|parent\\)\\W" (1 font-lock-constant-face nil nil))

;;     ;; method and variable features
;;     '("\\<\\(private\\|protected\\|public\\)\\s-+\\$?\\(?:\\sw\\|\\s_\\)+"
;;       (1 font-lock-keyword-face))

;;     ;; method features
;;     '("^[ \t]*\\(abstract\\|static\\|final\\)\\s-+\\$?\\(?:\\sw\\|\\s_\\)+"
;;       (1 font-lock-keyword-face))

;;     ;; variable features
;;     '("^[ \t]*\\(CONST\\)\\s-+\\$?\\(?:\\sw\\|\\s_\\)+"
;;       (1 font-lock-keyword-face))
;;     ))
;;   "Medium level highlighting for PC mode.")

;; (defconst pc-font-lock-keywords-3
;;   (append
;;    php-font-lock-keywords-2
;;    (list
    
;;     ;; <word> or </word> for HTML
;;     '("</?\\sw+[^>]*>" . font-lock-constant-face)

;;     ;; HTML entities
;;     '("&\\w+;" . font-lock-variable-name-face)

;;     ;; warn about '$' immediately after ->
;;     '("\\$\\(?:\\sw\\|\\s_\\)+->\\s-*\\(\\$\\)\\(\\(?:\\sw\\|\\s_\\)+\\)"
;;       (1 font-lock-warning-face) (2 default))
    
;;     ;; warn about $word.word -- it could be a valid concatenation,
;;     ;; but without any spaces we'll assume $word->word was meant.
;;     '("\\$\\(?:\\sw\\|\\s_\\)+\\(\\.\\)\\sw"
;;       1 font-lock-warning-face)
    
;;     ;; Warn about ==> instead of =>
;;     '("==+>" . font-lock-warning-face)
    
;;     ;; exclude casts from bare-word treatment (may contain spaces)
;;     `(,(concat "(\\s-*\\(" php-types "\\)\\s-*)")
;;       1 font-lock-type-face)
    
;;     ;; PHP5: function declarations may contain classes as parameters type
;;     `(,(concat "[(,]\\s-*\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-+\\$\\(?:\\sw\\|\\s_\\)+\\>")
;;       1 font-lock-type-face)
    
;;     ;; Fontify variables and function calls
;;     '("\\$\\(this\\|that\\)\\W" (1 font-lock-constant-face nil nil))
;;     `(,(concat "\\$\\(" php-superglobals "\\)\\W")
;;       (1 font-lock-constant-face nil nil)) ; $_GET & co
;;     '("\\$\\(\\(?:\\sw\\|\\s_\\)+\\)" (1 font-lock-variable-name-face)) ; $variable
;;     '("->\\(\\(?:\\sw\\|\\s_\\)+\\)" (1 font-lock-variable-name-face t t)) ; ->variable
;;     '("->\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(" . (1 default t t)) ; ->function_call
;;     '("\\(?:\\sw\\|\\s_\\)+::\\(?:\\sw\\|\\s_\\)+\\s-*(" . default) ; class::method call
;;     '("\\<\\(?:\\sw\\|\\s_\\)+\\s-*[[(]" . default)	; word( or word[
;;     '("\\<[0-9]+" . default)		; number (also matches word)

;;     ;; Warn on any words not already fontified
;;     '("\\<\\(?:\\sw\\|\\s_\\)+\\>" . font-lock-warning-face)
;;     ))
;;   "Gauchy level highlighting for PC mode.")

(defconst pc-font-lock-syntactic-keywords
  (if xemacsp nil
    (list
     '("\\(\--\\)"
       (1 (11 . nil)))
     '("--.*\\([\n]\\)"
       (1 (12 . nil)))
     )))

;; Define the imenu-generic-expression for PHP mode.
;; To use, execute M-x imenu, then click on Functions or Classes,
;; then select given function/class name to go to its definition.
;; [Contributed by Gerrit Riessen]
;; (defvar pc-imenu-generic-expression
;;  '(
;;    ("Functions"
;;     "^\\s-*function\\s-+\\([a-zA-Z0-9_]+\\)\\s-*(" 1)
;;    ;;    ("Classes"
;;    ;;     "^\\s-*class\\s-+\\([a-zA-Z0-9_]+\\)\\s-*" 1)
;;    )
;;  "Imenu generic expression for PC Mode. See `imenu-generic-expression'."
;;  )

;; Add "foreach" to conditional introducing keywords
;; (defconst pc-conditional-key nil)
;; (let ((all-kws "for\\|if\\|do\\|else\\|while\\|switch\\|foreach\\|elseif\\|try\\|finally\\|try\\|catch all")
;;       (front   "\\<\\(")
;;       (back    "\\)\\>[^_]"))
;;   (setq pc-conditional-key (concat front all-kws back)))

;; (defconst pc-class-kwds "class\\|interface")

;; (defconst pc-class-key
;;   (concat
;;    "\\(" pc-class-kwds "\\)\\s +"
;;    c-symbol-key				      ;name of the class
;;    "\\(\\s *extends\\s *" c-symbol-key "\\)?" ;maybe followed by superclass
;;    "\\(\\s *implements *[^{]+{\\)?"	      ;maybe the adopted protocols list
;;    ))

;; Create "default" symbol for GNU Emacs so that both XEmacs and GNU
;; emacs can refer to the default face by a variable named "default".
(unless (boundp 'default)
  (defvar default 'default))

;; Create faces for XEmacs
(unless (boundp 'font-lock-keyword-face)
  (copy-face 'bold 'font-lock-keyword-face))
(unless (boundp 'font-lock-constant-face)
  (copy-face 'font-lock-keyword-face 'font-lock-constant-face))

(provide 'pc-mode)

;;; pc-mode.el ends here
