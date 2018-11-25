;;; gp-inline.el --- evaluate Pari/GP expressions inline in a document

;; Copyright 2015, 2016, 2017 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 5
;; Keywords: processes, tex, tools
;; URL: http://user42.tuxfamily.org/pari-gp-inline/index.html
;; EmacsWiki: PariGP

;; This file is part of gp-inline.
;;
;; gp-inline is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; gp-inline is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with gp-inline.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; `M-x gp-inline-defines' runs Pari/GP code in GP-DEFINE lines of the
;; current buffer.  `M-x gp-inline-eval' runs a single line of Pari/GP code,
;; with or without such a prefix.  Both send the code to the GP sub-process
;; of PariEmacs pari.el (`M-x gp').  See the docstrings for more.

;;; Emacsen:

;; PariEmacs might need a moderately recent GNU Emacs.  It works with Emacs
;; 23 up at least.

;;; Install:

;; Put gp-inline.el in one of your `load-path' directories and add to your
;; .emacs
;;
;;     (autoload 'gp-inline-defines "gp-inline" nil t)
;;     (autoload 'gp-inline-eval    "gp-inline" nil t)
;;
;; The suggested keybinding for `gp-inline-eval' is C-M-x in `tex-mode' and
;; anywhere else you might have Pari/GP code.  C-M-x is normally elisp
;; `eval-defun' but if you have GP code rather than Lisp code then it's
;; helpful to make it GP eval.
;;
;;     (eval-after-load "tex"
;;       '(define-key tex-mode-map "\C-\M-x" 'gp-inline-eval))
;;
;; There's autoload cookies for the functions if you install via
;; `M-x package-install' or know how to use `update-file-autoloads'.

;;; History:

;; Version 1 - the first version
;; Version 2 - correction to URL shown
;; Version 4 - compilation-mode pattern
;; Version 5 - fix for compilation-mode pattern

;;; Code:

(eval-when-compile
  (unless (and (fboundp 'declare)
               (fboundp 'ignore-errors))
    (require 'cl))) ;; for ignore-errors macro

(defvar gp-process)  ;; in pariemacs pari.el

;;-----------------------------------------------------------------------------
;; `make-temp-file' new in emacs21, not in xemacs21

(cond ((or (eval-when-compile (fboundp 'make-temp-file))
           (fboundp 'make-temp-file))
       ;; emacs21 up, noticed at compile time or run time
       (eval-and-compile
         (defalias 'gp-inline--make-temp-file 'make-temp-file)))

      ((locate-library "mm-util") ;; from gnus
       ;; xemacs21
       (autoload 'mm-make-temp-file "mm-util")
       (defalias 'gp-inline--make-temp-file 'mm-make-temp-file))

      ((locate-library "poe") ;; from APEL
       ;; emacs20 with poe.el add-on
       (require 'poe)
       (defalias 'gp-inline--make-temp-file 'make-temp-file))

      (t
       ;; umm, dunno, hope the user can define it
       (message "gp-inline.el: don't know where to get `make-temp-file'")
       (defalias 'gp-inline--make-temp-file 'make-temp-file)))

;;-----------------------------------------------------------------------------
;; generic

(eval-when-compile
  (defmacro gp-inline--with-errorfile (&rest body)
    "An internal part of gp-inline.el.
This macro does not exist when running byte compiled.

Create a temporary file for use by the BODY forms.  Variable
`errorfile' is bound to the filename.  An `unwind-protect'
ensures it is removed no matter what BODY does."

    (declare (debug t))  ;; emacs22,xemacs21, or 'cl
    `(let ((errorfile (gp-inline--make-temp-file "gp-inline-el-")))
       (unwind-protect
           (progn ,@body)
         (delete-file errorfile)))))

;;-----------------------------------------------------------------------------

(defun gp-inline-prefix ()
  "An internal part of gp-inline.el.
Return a string which is the prefix of the current GP line,
without leading or trailing whitespace.  See `gp-inline-eval' on
the prefixes allowed."

  ;; Ensure `comment-start-skip' if the mode only defined `comment-start'.
  ;; But `comment-normalize-vars' not in xemacs21.
  (ignore-errors (comment-normalize-vars t))

  ;; always capital GP, no case-fold-search here in case comment-start-skip
  ;; is something alphabetical like in m4
  (let ((regexp (concat
                 "\\([ \t]*\\)\
\\(" comment-start-skip
"\\|[#%]+\\|//+\\|/\\*\\|\\\\\\\\\\
\\)?\
\\([ \t]*=for\\([ \t]\\|$\\)\\)?\
\\([ \t]*\\(\\*[ \t]*\\)?GP-[^ \t\r\n]*\\)?")))
    (save-excursion
      (beginning-of-line)
      (or (looking-at regexp)
          (error "Oops, gp-inline-prefix no line match"))
      ;; comment-start-skip usually matches following whitespace, strip that
      (goto-char (match-end 0))
      (skip-chars-backward " \t" (match-end 1))
      (buffer-substring (match-end 1) (point)))))

(defun gp-inline-expression-at-point ()
  "An internal part of gp-inline.el.
Return a Pari/GP expression at point."

  ;; Optional prefix is comment-start-skip,
  ;; or gp-inline $comment_prefix_re and GP-Something
  ;; same prefix as current line, and possible final backslash
  (let ((regexp (concat "[ \t]*"
                        (regexp-quote (gp-inline-prefix))
                        "\\([^\r\n]*?\\(\\\\\\)?\\)[ \t]*$"))
        (str ""))
    (save-excursion
      ;; preceding backslashed lines
      (while (and (zerop (forward-line -1))
                  (looking-at regexp)
                  (match-beginning 2)) ;; with backslash
        (setq str (concat (match-string 1) "\n" str))))
    (save-excursion
      ;; current and following backslashed lines
      (beginning-of-line)
      (while (and (looking-at regexp)
                  (setq str (concat str (match-string 1) "\n"))
                  (match-beginning 2)        ;; continue while backslashed
                  (zerop (forward-line 1)))) ;; and while more lines
      (substring str 0 -1)))) ;; sans final \n

;;;###autoload
(defun gp-inline-eval ()
  "Evaluate the Pari/GP expression at point.
The expression is passed to the GP subprocess (`gp' from
PariEmacs).

An optional comment prefix (`comment-start-skip' of the current
mode) is skipped, as is any \"GP-Test\" or similar of gp-inline.

    120*240
    ;; 2*6+13*7
    % GP-Test  2+2==4
    =for GP-Test  2*2==4
    # GP-DEFINE  f(n) = 5*n!;

An expression can cross multiple lines with backslashes.  Point
can be on any of these lines.

    2*6 \\=\\
    + 13*7

Braces for multi-line expressions are not currently recognised.
If using them in GP-DEFINE then consider `gp-inline-defines' to
evaluate all definitions in the buffer."

  (interactive)
  (require 'pari)
  (let ((str (gp-inline-expression-at-point)))
    (save-selected-window
      ;; ENHANCE-ME: could like something in pariemacs taking a string to run
      (gp)
      (gp-cleans-last-line)
      (insert str)
      (set-marker (process-mark gp-process) (point))
      (gp-send-input))))

;;;###autoload
(defun gp-inline-defines ()
  "Evaluate GP-DEFINE code in the current buffer.
Any \"GP-DEFINE\", \"GP-CONSTANT\" etc definition lines in the
current buffer are extracted and passed to the GP
subprocess (`gp' from PariEmacs).

Any error from gp-inline is shown in a buffer.  This is a little
rough.  The definitions are in a buffer \" *gp-inline-defines*\"
\(name starting with a space) if something seems to go badly
wrong.  (It's passed to GP with `gp-run-in-region' which writes
it to a temporary file.)"
  (interactive)
  (require 'pari)
  (gp-inline--with-errorfile
   (let ((dir    default-directory)
         (buffer (get-buffer-create " *gp-inline-defines*")))
     (with-current-buffer buffer
       (erase-buffer)
       ;; directory from source document in case read("foo.gp") etc
       (setq default-directory dir))

     ;; Separating stdout and stderr is a bit tedious, but perl has a bad
     ;; habit of making warning messages to spam all users about problems
     ;; only the author can address.  Don't want that mixed into stdout.
     (if (equal 0 (let ((process-connection-type nil)) ;; pipe
                    (call-process-region
                     (point-min) (point-max)
                     "gp-inline"
                     nil               ;; no delete original
                     (list buffer      ;; stdout to buffer
                           errorfile)  ;; stderr to file
                     nil               ;; no display
                     "--stdin" "--defines")))
         (save-selected-window
           (with-current-buffer buffer
             (gp-run-in-region (point-min) (point-max))))

       (switch-to-buffer buffer)
       (goto-char (point-max))
       (insert-file-contents errorfile)
       (goto-char (point-max))
       (error "Error from gp-inline")))))


;;------------------------------------------------------------------------------
;; compilation-mode match of gp-inline message
;;
;;    *** at top-level: ...inline("foo.tex:153",(()->bar())())
;;
;; Would like to get a file:line: output from gp or from the way gp-inline
;; runs it, but this helps at least with the current form.

(eval-after-load "compile"
  '(let ((symbol  'gp-inline)
         (pattern '("^ +\\*\\*\\* +at top.*\"\\([^:\"]+\\):\\([0-9]+\\)\"" 1 2)))
     (eval-when-compile (require 'compile))
     (cond ((eval-when-compile (boundp 'compilation-error-regexp-systems-list))
            ;; xemacs21
            (add-to-list 'compilation-error-regexp-alist-alist
                         (list symbol pattern))
            (compilation-build-compilation-error-regexp-alist))
           ((eval-when-compile (boundp 'compilation-error-regexp-alist-alist))
            ;; emacs22 up
            (add-to-list 'compilation-error-regexp-alist symbol)
            (add-to-list 'compilation-error-regexp-alist-alist
                         (cons symbol pattern)))
           (t
            ;; emacs21
            (add-to-list 'compilation-error-regexp-alist pattern)))))


;;------------------------------------------------------------------------------

(provide 'gp-inline)

;;  LocalWords:  Pari pari PariEmacs eval docstrings

;;; gp-inline.el ends here
