;;; cl-loop-aplist.el --- cl loop macro alist and plist support

;; Copyright 2010, 2011, 2012, 2013, 2014, 2015 Kevin Ryde
;;
;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 5
;; Keywords: extensions, cl
;; URL: http://user42.tuxfamily.org/cl-loop-aplist/index.html
;; EmacsWiki: CommonLispForEmacs

;; cl-loop-aplist.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; cl-loop-aplist.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with cl-loop-aplist.el; see the file COPYING.  Failing that, go to
;; <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; This is a few lines extending the cl.el `loop' macro with alist and plist
;; forms iterator forms.  They're similar to the builtin `hash-keys' and
;; `hash-values'.
;;
;;     for VAR being the alist-keys of ALIST
;;     for VAR being the alist-values of ALIST
;;
;;     for VAR being the plist-keys of PLIST
;;     for VAR being the plist-values of PLIST
;;
;; `of-ref' instead of `of' makes VAR is a `setf'-able target modifying the
;; ALIST or PLIST.
;;
;;     for VAR being the plist-keys of-ref PLIST
;;     ...
;;     do (incf VAR)  ;; modifies PLIST
;;
;; `using' can give both keys and values together.  The loop can be either
;; keys or values first and then the other one in the `using'.
;;
;;     for VAR being the alist-keys of ALIST
;;         using (alist-values VAR2)
;;
;;     for VAR being the plist-values of PLIST
;;         using (plist-keys VAR2)
;;
;; `using-ref' makes VAR2 a `setf'-able target modifying the ALIST or PLIST.
;; It can be used with either `of' or `of-ref' to have one or both variables
;; `setf'-able as desired.
;;
;;     for VAR being the alist-keys of ALIST
;;         using-ref (alist-values VAR2)
;;
;;     for VAR being the plist-values of-ref PLIST
;;         using-ref (plist-keys VAR2)
;;
;;
;; Examples
;; --------
;;
;; Since this is all done with macros it can be `eval-when-compile', no need
;; to load cl-loop-aplist.el at runtime when running byte compiled,
;;
;;     (eval-when-compile
;;       (require 'cl)
;;       (require 'cl-loop-aplist))
;;
;; Then for example
;;
;;     (loop for x being the alist-values of '(("red"   . 6)
;;                                             ("green" . 8))
;;           sum x)
;;     => 14
;;
;; Or using `of-ref' on a plist to increment values
;;
;;     (defvar my-plist
;;       (list "red" 6 "green" 8))
;;     (loop for v being the plist-values of my-plist
;;           (incf v))
;;     =>
;;     my-plist is now ("red" 7 "green" 9))
;; 
;; Or the `using' clause,
;;
;;     (let ((my-list '((1 . 123)
;;                      (2 . 456))))
;;       (loop for v being the alist-values of my-list
;;             using (alist-keys k)
;;             sum (* v k)))
;;     => 1035
;;
;; The symbol properties from `symbol-plist' are a common use for plists.
;; The following for example `insert's a dump of properties from a symbol
;; into the current buffer
;;
;;     (put 'foo 'prop1 123)
;;     (put 'foo 'bar   99999)
;;     (put 'foo 'quux  "hello")
;;
;;     (let* ((sym 'foo)
;;            (width (loop for p being the plist-keys of (symbol-plist sym)
;;                         maximize (length (symbol-name p))))
;;            (fmt   (concat "%-" (number-to-string width) "s %S\n")))
;;       (loop for p being the plist-keys of (symbol-plist sym)
;;             using (plist-values v)
;;             do (insert (format fmt p v))))
;;     =>
;;     prop1 123
;;     bar   99999
;;     quux  1
;;
;;
;; Destructuring
;; -------------
;;
;; An alist can also be iterated easily with the builtin loop destructing as
;; follows.  This is much more compact than `of' and `using'.
;;
;;     for (x . y) in ALIST
;;
;; Similarly for a plist but must include a `by cddr', and note that it's
;; "on PLIST" not "in PLIST".
;;
;;     for (x y) on PLIST by 'cddr
;;
;; These destructurings don't allow an `in-ref' or `on-ref' to make the
;; variables `setf'-able (not as of Emacs 23.2 at least).  Otherwise it's a
;; matter of personal preference between these forms and a `being' form.  If
;; wanting only the keys or only the values then the `being' is one less
;; variable.
;;
;;
;; Unloading
;; ---------
;;
;; In Emacs 22 up an cl-loop-aplist.el an unload removes the removes the
;; `alist-keys' etc handlers from `loop'.
;;
;;     (unload-feature 'cl-loop-aplist)
;;
;; In Emacs 21 and earlier this unload-feature will remove the code but
;; leave behind the handler hooks, causing a no-such-function error if
;; they're used.


;;; Install:

;; Put cl-loop-aplist.el in one of your `load-path' directories and then use
;; it from .el code with
;;
;;     (eval-when-compile
;;       (require 'cl)
;;       (require 'cl-loop-aplist))
;;

;;; History:

;; Version 1 - the first version
;; Version 2 - emacs24.2 incompatible change `args' -> `loop-args'
;; Version 3 - emacs24.3 incompatible change cl--loop etc
;; Version 4 - quieten byte compile a bit
;; Version 5 - more quieten the byte compile


;;; Code:

(eval-when-compile
  (require 'cl)) ;; (pop (symbol-value)

;; dynamic variables from `loop', not defvar-ed by cl itself in emacs24.2
(defvar loop-body)
(defvar loop-symbol-macs)
(defvar loop-bindings)
(defvar loop-for-sets)
(defvar loop-for-steps)
(defvar loop-body)

(defvar cl--loop-args)
(defvar cl--loop-steps)
(defvar cl--loop-body)
(defvar cl--loop-symbol-macs)
(defvar cl--loop-bindings)

(defun cl-loop-aplist (var opposite stepper accessor opposite-accessor)
  "Shared parts of `alist-keys',`plist-values',etc for cl `loop' macro.
This is an internal part of cl-loop-aplist.el.
For now the arguments are as follows.

VAR is a symbol, a variable which is to be created and iterate
over the keys or values of the alist or plist being handled.

OPPOSITE is a symbol, the opposite keys/values to what's being
handled.  For example if handling `alist-keys' then OPPOSITE is
`alist-values'.

STEPPER is a symbol, a function to call as (STEPPER listvar) to
step over the first element of the alist or plist in listvar.
STEPPER is `cdr' for alist or `cddr' for plist.

ACCESSOR is a symbol, a function to call as (ACCESSOR listvar) to
return the first element of the list in listvar.  So when
handling `alist-keys' ACCESSOR is `caar', or when `alist-values'
then `cdar'.  Similarly for a plist `car' or `cadr'.

OPPOSITE-ACCESSOR is a symbol, a function to call
as (OPPOSITE-ACCESSOR listvar) step to return the first element
in listvar but the opposite keys/values, and so corresponding to
OPPOSITE.  For example when handling `alist-keys' OPPOSITE is
`alist-values' and OPPOSITE-ACCESSOR is `cdar' to access the
value part of the first alist element."

  (if (boundp 'cl--loop-args)
      ;; emacs24.3
      (let* ((of (pop cl--loop-args)))
        (unless (memq of '(of of-ref))
          (error "Expected `of' or `of-ref'"))

        (let ((tempvar (make-symbol "--cl-loop-aplist--var--"))
              sets)

          (if (eq of 'of-ref)
              (push (list var (list accessor tempvar)) cl--loop-symbol-macs)
            (push (list var)  cl--loop-bindings)
            (push (list accessor tempvar) sets)
            (push var sets))

          (push (list (list tempvar (pop cl--loop-args))) cl--loop-bindings)
          (push `(setq ,tempvar (,stepper ,tempvar)) cl--loop-steps) ;; step

          ;; possible `using' or `using-ref'
          (let ((using (car cl--loop-args)))
            (when (memq using '(using using-ref))
              (pop cl--loop-args) ;; drop the `using' or `using-ref'
              (let* ((form  (pop cl--loop-args)) ;; list
                     (word2 (car-safe form))
                     (var2  (car-safe (cdr-safe form))))
                (unless (and (= 2 (safe-length form))
                             (eq word2 opposite))  ;; opposite of values/keys
                  (error "Bad `using' clause"))
                (if (eq using 'using-ref)
                    (push (list var2 (list opposite-accessor tempvar))
                          cl--loop-symbol-macs)
                  (push (list var2) cl--loop-bindings)
                  (push (list opposite-accessor tempvar) sets)
                  (push var2 sets)))))

          ;; accessor `setq's
          (if sets
              (push `(progn
                       (setq ,@sets)
                       t)
                    cl--loop-body))

          ;; loop continuation condition, `consp' same as `in LIST'
          (push (list 'consp tempvar) cl--loop-body)))

    ;; emacs24.2 and earlier
    (let* ((argsvar (if (boundp 'loop-args) 'loop-args ;; emacs24.2
                      'args))  ;; emacs23,xemacs21
           (of      (pop (symbol-value argsvar)))) ;; `of' or `of-ref'
      (unless (memq of '(of of-ref))
        (error "Expected `of' or `of-ref'"))

      (let ((tempvar (make-symbol "--cl-loop-aplist--var--")))
        (if (eq of 'of-ref)
            (push (list var (list accessor tempvar)) loop-symbol-macs)
          (push (list var)  loop-bindings)
          (push (list var (list accessor tempvar)) loop-for-sets))

        (push (list (list tempvar (pop (symbol-value argsvar)))) loop-bindings)
        (push (list 'consp tempvar) loop-body) ;; `consp' same as `in LIST'
        (push (list tempvar (list stepper tempvar)) loop-for-steps) ;; step

        ;; possible `using' or `using-ref'
        (let ((using (car (symbol-value argsvar))))
          (when (memq using '(using using-ref))
            (set argsvar  ;; drop the `using' or `using-ref'
                 (cdr (symbol-value argsvar)))
            (let* ((form  (pop (symbol-value argsvar))) ;; list
                   (word2 (car-safe form))
                   (var2  (car-safe (cdr-safe form))))
              (unless (and (= 2 (safe-length form))
                           (eq word2 opposite))  ;; opposite of values/keys
                (error "Bad `using' clause"))
              (if (eq using 'using-ref)
                  (push (list var2 (list opposite-accessor tempvar))
                        loop-symbol-macs)
                (push (list var2) loop-bindings)
                (push (list var2 (list opposite-accessor tempvar))
                      loop-for-sets)))))))))

(defun cl-loop-aplist-alist-keys (var)
  "Support for `alist-keys' in cl `loop' macro."
  ;; checkdoc-params: (var)
  (cl-loop-aplist var 'alist-values 'cdr 'caar 'cdar))

(defun cl-loop-aplist-alist-values (var)
  "Support for `alist-values' in cl `loop' macro."
  ;; checkdoc-params: (var)
  (cl-loop-aplist var 'alist-keys   'cdr 'cdar 'caar))

(defun cl-loop-aplist-plist-keys (var)
  "Support for `plist-keys' in cl `loop' macro."
  ;; checkdoc-params: (var)
  (cl-loop-aplist var 'plist-values 'cddr 'car 'cadr))

(defun cl-loop-aplist-plist-values (var)
  "Support for `plist-values' in cl `loop' macro."
  ;; checkdoc-params: (var)
  (cl-loop-aplist var 'plist-keys   'cddr 'cadr 'car))

(defconst cl-loop-aplist-handler-list
  '((alist-keys   . cl-loop-aplist-alist-keys)
    (alist-values . cl-loop-aplist-alist-values)
    (plist-keys   . cl-loop-aplist-plist-keys)
    (plist-values . cl-loop-aplist-plist-values)))

(defun cl-loop-aplist-unload-function ()
  "Remove `loop' handlers set for `alist-keys' etc.
This is called by `unload-feature'."
  ;; only remove our settings, not anything which might have overridden
  (dolist (elem cl-loop-aplist-handler-list)
    (if (get (car elem) 'cl-loop-for-handler)
        (put (car elem) 'cl-loop-for-handler nil)))
  nil) ;; and do normal unload-feature actions too

(dolist (elem cl-loop-aplist-handler-list)
  (put (car elem) 'cl-loop-for-handler (cdr elem)))

;;-----------------------------------------------------------------------------

;; LocalWords: cl destructuring plist plists accessor aplist ie el builtin
;; LocalWords: runtime fmt quux destructurings maximize listvar

(provide 'cl-loop-aplist)

;;; cl-loop-aplist.el ends here
