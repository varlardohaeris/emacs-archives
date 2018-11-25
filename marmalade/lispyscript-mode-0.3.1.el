;;; lispyscript-mode.el --- Major mode for LispyScript code.

;; Copyright 2013 Kris Jenkins
;; Author: Kris Jenkins
;; URL: https://github.com/krisajenkins/lispyscript-mode
;; Version: 0.3.1

;;; Commentary:
;;
;; A major mode for LispyScript http://lispyscript.com/

(require 'lisp-mode)
(require 'font-lock)

;;;###autoload
(defvar lispyscript-font-lock-defaults
  `((,(rx "("
		  (group "macro")
		  (one-or-more whitespace) (group (one-or-more word)))
  	 (1 font-lock-keyword-face)
  	 (2 font-lock-function-name-face))
	(,(rx "("
		  (group (or "var" "template" "doMonad" "monad" "withMonad" "testGroup" "testRunner"))
		  (one-or-more whitespace) (group (one-or-more word)))
  	 (1 font-lock-keyword-face)
  	 (2 font-lock-variable-name-face))
  	(,(rx "("
		  (group (or "->"
					 "array" "arrayInit" "arrayInit2d" "object" "new" "javascript"
					 "if" "cond" "when" "unless" "do" "function" "try" "catch" "throw" "loop"
					 "each" "each2d" "eachKey" "reduce" "map" "for"
					 "template-repeat" "template-repeat-key"
					 "include"
					 "true" "false"
					 "assert")))
  	 (1 font-lock-keyword-face))
	(,(rx bow (group (or "true" "false")))
	 (1 font-lock-keyword-face))
	(,(rx bow (group "~" (opt ?@) (one-or-more word) (opt "...")))
	 (1 font-lock-type-face))
	(,(rx bow (group "___" (one-or-more word)))
	 (1 font-lock-type-face))))

;;;###autoload
(defmacro lispyscript-mode/add-word-chars (&rest chars)
  (cons 'progn
		(mapcar (lambda (char)
				  `(modify-syntax-entry ,char "w" lispyscript-mode-syntax-table))
				chars)))

;;;###autoload
(define-derived-mode lispyscript-mode lisp-mode "LispyScript"
  "Major mode for LispyScript"
  (lispyscript-mode/add-word-chars ?_ ?~ ?. ?- ?> ?< ?! ??)
  (setq font-lock-defaults '((lispyscript-font-lock-defaults))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ls$" . lispyscript-mode))

(provide 'lispyscript-mode)
;;; lispyscript-mode.el ends here
