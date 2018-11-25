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


