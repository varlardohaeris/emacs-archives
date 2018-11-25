;;; egison-mode.el --- Egison editing mode

;; Copyright 2011-2013 Satoshi Egi

;;; Author: Satoshi Egi <egisatoshi@gmail.com>
;;; URL: https://github.com/egisatoshi/egison3/blob/master/elisp/egison-mode.el
;;; Version: 0.1.4

;; Code goes here

(defconst egison-font-lock-keywords-1
  (eval-when-compile
    (list
     "\\<define\\>"
     "\\<test\\>"
     "\\<execute\\>"
     
     "\\<lambda\\>"
     "\\<macro\\>"
     "\\<do\\>"
     "\\<if\\>"
     "\\<let\\>"
     "\\<letrec\\>"
     "\\<matcher\\>"
     "\\<algebraic-data-matcher\\>"
     "\\<loop\\>"
     "\\<function\\>"
     "\\<match\\>"
     "\\<match-all\\>"
     "\\<index-loop\\>"
     "\\<pattern-constructor\\>"

     "\\<undefined\\>"
     "\\<something\\>"
     
     "\\\."
     "\\\,"
     "\\\!"
     "`"
     "\\\~"
     "\\\#"
     "|"
     "%"
     "\\\&"
     "@"
     "\\<_\\>"
     ))
  "Subdued expressions to highlight in Egison modes.")

(defconst egison-font-lock-keywords-2
  (append egison-font-lock-keywords-1
   (eval-when-compile
     (list
      (cons "\\\$\\\w*" font-lock-variable-name-face)
      )))
  "Gaudy expressions to highlight in Egison modes.")

(defvar egison-font-lock-keywords egison-font-lock-keywords-1
  "Default expressions to highlight in Egison modes.")


(defun open-paren-p ()
  (let ((c (string-to-char (thing-at-point 'char))))
    (or (eq c 40) (eq c 60) (eq c 91) (eq c 123))))

(defun close-paren-p ()
  (let ((c (string-to-char (thing-at-point 'char))))
    (or (eq c 41) (eq c 62) (eq c 93) (eq c 125))))

(defun last-unclosed-paren ()
  (save-excursion
    (let ((pc 0))
      (while (<= pc 0)
        (if (bobp)
            (setq pc 2)
          (backward-char)
          (if (open-paren-p)
              (progn
                (setq pc (+ pc 1))
                (if (and (= pc 0) (= (current-column) 0))
                    (setq pc 2)))
            (if (close-paren-p)
                (setq pc (- pc 1))))))
      (if (= pc 2)
          nil
        (point)))))

(defun indent-point ()
  (save-excursion
    (beginning-of-line)
    (let ((p (last-unclosed-paren)))
      (if p
          (progn
            (goto-char (last-unclosed-paren))
            (let ((cp (current-column)))
              (cond ((eq (string-to-char (thing-at-point 'char)) 40)
                     (forward-char)
                     (let* ((op (current-word))
                            (ip (keyword-indent-point op)))
                       (if ip
                           (+ ip cp)
                         (progn (forward-sexp)
                                (+ 1 (current-column))))))
                    ((eq (string-to-char (thing-at-point 'char)) 60)
                     (forward-char)
                     (let ((op (current-word)))
                       (+ 2 (length op) cp)))
                    ((eq (string-to-char (thing-at-point 'char)) 91)
                     (forward-char)
                     (if (eq (string-to-char (thing-at-point 'char)) 124)
                         (+ 2 cp)
                       (+ 1 cp)))
                    (t (+ 1 cp)))))
        0))))

(defun keyword-indent-point (name)
  (cond ((equal "define" name) 2)
        ((equal "lambda" name) 2)
        ((equal "function" name) 2)
        ((equal "macro" name) 2)
        ((equal "let" name) 2)
        ((equal "letrec" name) 2)
        ((equal "do" name) 2)
        ((equal "match" name) 2)
        ((equal "match-all" name) 2)
        ((equal "matcher" name) 2)
        ((equal "algebraic-data-matcher" name) 2)
        ((equal "generate-array" name) 2)
        ((equal "index-loop" name) 2)
        ((equal "pattern-constructor" name) 2)
        ))

(defun egison-indent-line ()
  "indent current line as Egison code."
  (interactive)
  (indent-line-to (indent-point)))


(defvar egison-mode-map
  (let ((smap (make-sparse-keymap)))
    (define-key smap "\C-j" 'newline-and-indent)
    smap)
  "Keymap for Egison mode.")


(defvar egison-mode-syntax-table
  (let ((egison-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry 60 "(" egison-mode-syntax-table)
    (modify-syntax-entry 62 ")" egison-mode-syntax-table)
    (modify-syntax-entry 59 "<" egison-mode-syntax-table)
    (modify-syntax-entry 10 ">" egison-mode-syntax-table)
    (modify-syntax-entry 63 "w" egison-mode-syntax-table)
    egison-mode-syntax-table)
  ;; (copy-syntax-table lisp-mode-syntax-table)
  "Syntax table for Egison mode")

(defun egison-mode-variables ()
  (set-syntax-table egison-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults)
       '((egison-font-lock-keywords
          egison-font-lock-keywords-1 egison-font-lock-keywords-2)
         nil t (("+-*/=?%:_" . "w") ("<" . "(") (">" . ")"))
         ))
  (set (make-local-variable 'indent-line-function) 'egison-indent-line)
  (set (make-local-variable 'comment-start) ";")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) ";+ *")
  (set (make-local-variable 'comment-end-skip) nil)
  )


(defun egison-mode ()
  "Major mode for editing Egison code.

Commands:
\\{egison-mode-map}
Entry to this mode calls the value of `egison-mode-hook'
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (setq indent-tabs-mode nil)
  (use-local-map egison-mode-map)
  (setq major-mode 'egison-mode)
  (setq mode-name "Egison")
  (egison-mode-variables)
  (run-mode-hooks 'egison-mode-hook))


(defgroup egison nil
  "Editing Egison code."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'lisp)

(defcustom egison-mode-hook nil
  "Normal hook run when entering `egison-mode'.
See `run-hooks'."
  :type 'hook
  :group 'egison)

;;; egison-mode.el ends here
