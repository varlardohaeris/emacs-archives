;;; elisp-docstring-preview.el --- preview Emacs Lisp docstring

;; Copyright 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2019 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 7
;; Keywords: lisp, docs
;; URL: http://user42.tuxfamily.org/elisp-docstring-preview/index.html

;; elisp-docstring-preview.el is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; elisp-docstring-preview.el is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; `M-x elisp-docstring-preview' displays the docstring of a `defun',
;; `defvar' or similar toplevel form at point by `eval-defun' then
;; displaying it with the corresponding `describe-function',
;; `describe-variable', etc.  See the `elisp-docstring-preview' docstring
;; for more.

;;; Emacsen:
;;
;; Designed for Emacs 20 up, works in XEmacs 21.

;;; Install:

;; Put elisp-docstring-preview.el in one of your `load-path' directories and
;; to make `M-x elisp-docstring-preview' available add to your .emacs
;;
;;     (autoload 'elisp-docstring-preview' "elisp-docstring-preview" nil t)
;;
;; Bind it to a key of your choice with for example
;;
;;     (define-key emacs-lisp-mode-map [f8] 'elisp-docstring-preview)
;;
;; `emacs-lisp-mode' is used by the *scratch* buffer so it's pre-loaded
;; ready to have the keymap extended.
;;
;; There's an autoload cookie for the function if you know how to use
;; `update-file-autoloads' and friends, then just make the keybinding.

;;; History:

;; Version 1 - the first version
;; Version 2 - better window-point saving
;; Version 3 - allow surrounding eval-and-compile or similar
;; Version 4 - allow surrounding `if' etc conditional
;;           - add static-defconst
;; Version 5 - cl macros only for emacs20
;; Version 6 - don't use second,third macros
;; Version 7 - docstrings of what's supposed to be internal-use

;;; Code:

(eval-when-compile
  (unless (and (fboundp 'dolist)
               (fboundp 'push))
    (require 'cl))) ;; emacs20 `dolist',`push'

(defvar help-xref-stack-item) ;; in help-mode.el

;;-----------------------------------------------------------------------------
;; window-point save/restore

(eval-when-compile
  (put 'elisp-docstring-preview--linecol 'side-effect-free t))
(defun elisp-docstring-preview--linecol (point)
  "An internal part of elisp-docstring-preview.el.
Return a pair (LINE . COLUMN) for POINT in the current buffer.
LINE counts from 0 for the first line.
COLUMN counts from 0 for the first column."
  (save-excursion
    (goto-char point)
    (cons (count-lines (point-min) (line-beginning-position))
          (current-column))))

(defun elisp-docstring-preview--goto-linecol (linecol)
  "An internal part of elisp-docstring-preview.el.
Move point to LINECOL in the current buffer.
LINECOL is a pair (LINE . COLUMN), starting from (0 . 0) for the
first line and first column."
  (goto-char (point-min))
  (forward-line (car linecol)) ;; line
  (move-to-column (cdr linecol)))

(eval-when-compile
  (put 'elisp-docstring-preview--wpoints-get 'side-effect-free t))
(defun elisp-docstring-preview--wpoints-get (buffer)
  "An internal part of elisp-docstring-preview.el.
Return a list of point and start positions for BUFFER.
Currently the return is

    (((WINDOW (START-LINE . START-COL) (POINT-LINE . POINT-COL))
      (WINDOW (START-LINE . START-COL) (POINT-LINE . POINT-COL))
      ...)
     .
     (LINE . COL))

The list is `window-start' and `window-point' positions for each
window currently displaying BUFFER, followed by the plain `point'
line/column.  As usual if one of the windows is the selected
window in the selected frame then plain `point' is the same as
the `window-point' there, otherwise plain `point' is separate."

  (and buffer
       (get-buffer buffer)
       (with-current-buffer buffer
         (cons (mapcar (lambda (window)
                         (list window
                               (elisp-docstring-preview--linecol
                                (window-start window))
                               (elisp-docstring-preview--linecol
                                (window-point window))))
                       (get-buffer-window-list buffer
                                               t   ;; include minibuffer
                                               t)) ;; all frames
               (elisp-docstring-preview--linecol (point))))))

(defun elisp-docstring-preview--wpoints-set (buffer wpoints)
  "An internal part of elisp-docstring-preview.el.
Set a window point and start positions of BUFFER from WPOINTS.
WPOINTS should be as returned by
`elisp-docstring-preview--wpoints-get'."

  (when (and wpoints
             (buffer-live-p buffer))
    (with-current-buffer buffer
      (dolist (window (get-buffer-window-list buffer
                                              t   ;; include minibuffer
                                              t)) ;; all frames
        (let ((elem (assoc window (car wpoints))))
          (if elem
              (progn
                (elisp-docstring-preview--goto-linecol (nth 1 elem))
                ;; don't let window-start be the very end of the buffer, since
                ;; that would leave it completely blank
                (if (= (point) (point-max))
                    (forward-line -1))
                (set-window-start window (point))

                (elisp-docstring-preview--goto-linecol (nth 2 elem))
                (set-window-point window (point)))

            ;; new windows are "restored" to the the buffer point and centre
            ;; of the screen
            (elisp-docstring-preview--goto-linecol (cdr wpoints))
            (set-window-point window (point))
            (save-selected-window
              (select-window window)
              (recenter nil)))))

      (elisp-docstring-preview--goto-linecol (cdr wpoints)))))


;;-----------------------------------------------------------------------------

(defun elisp-docstring-form-at-point ()
  "An internal part of elisp-docstring-preview.el.
Return the defining form at point.

Wrapping macros like `eval-when-compile' or `if' etc are
stripped, so the return is (or should be) the `defun' part
contained."

  (save-excursion
    (end-of-defun)
    (beginning-of-defun)

    (let ((form (read (current-buffer))))

      (while (cond
              ;; descend through conditionals like
              ;; (if COND (defun name (args) ...))
              ((and (memq (car-safe form)
                          '(if when unless
                             static-if static-when static-unless))
                    (consp (cdr form)))
               (setq form (car-safe (cdr-safe (cdr-safe form))))
               t) ;; keep looping

              ;; descend through a macro wrapping a defun like
              ;; (eval-and-compile (defun name (args) ...))
              ((and (symbolp (car-safe form))          ;; wrapping name
                    (listp (car-safe (cdr-safe form))) ;; second
                    (not (eq 'quote (car-safe (car-safe (cdr-safe form))))))
               (setq form (car-safe (cdr-safe form)))
               t) ;; keep looping

              (t nil))) ;; stop loop
      form)))

(defun elisp-docstring-preview--help-buffer (method symbol)
  "An internal part of elisp-docstring-preview.el.
Return the name of a help buffer for METHOD and SYMBOL.
METHOD is a symbol like `describe-function'.  The return is a
string buffer name, or nil if no buffer is describing SYMBOL in
this way."

  (or (let ((type (symbol-name method)))
        ;; xemacs21 help.el buffer named for the method and symbol
        (if (string-match "\\`describe-" type)
            (setq type (substring type (match-end 0))))
        (let ((buffer-name (format "*Help: %s `%s'*" type symbol)))
          (and (get-buffer buffer-name)
               buffer-name)))

      ;; emacs21 help-mode.el single *Help* buffer with "stack item"
      (let ((buffer (get-buffer "*Help*")))
        (and buffer
             (with-current-buffer buffer
               (and (boundp 'help-xref-stack-item)
                    (equal method
                           (car-safe help-xref-stack-item))
                    (equal symbol
                           (car-safe (cdr-safe help-xref-stack-item)))
                    buffer))))))

;;;###autoload
(defun elisp-docstring-preview ()
  "Preview an Emacs Lisp docstring at point.
A `defun', `defvar', or similar form at point is evaluated with
`eval-defun' and then displayed with a corresponding
`describe-function', `describe-variable', etc.

    (defun foo ()
      \"Docstring.\"
      123)

This preview is designed for use while writing code.  The
`eval-defun' means it's not safe on untrusted code.

The following definition forms are recognised

      definition form          description method

    defun, etc               describe-function
    defvar, etc              describe-variable
    defface                  describe-face
    define-coding-system     describe-coding-system
    define-charset           describe-character-set
    defclass                 describe-class (eieio.el)
    defmath                  describe-function, with prefix
    define-ibuffer-filter    \\=\\
    define-ibuffer-op        | describe-function, with prefix
    define-ibuffer-sorter    |
    define-ibuffer-column    /

If a *Help* buffer is already showing the function etc then the
window start and current point are preserved, to stay at the same
part if scrolled down in a long docstring.

Docstrings in the source are usually readable enough but a
`describe-function' etc is good to check what the user will see
and be sure escapes like keymaps expand as desired, or that URLs
and info links buttonize, as per

    Info node `(elisp)Keys in Documentation'
    Info node `(elisp)Documentation Tips'

If `eval-defun' fails then it generally enters the debugger (see
its docstring for details).

Quoted names like (defalias 'new 'old) are recognised.  Often
these don't have their own docstrings but the preview will at
least show how `new' is described by the help system.

Unknown \"def***\" forms are assumed to create functions, since
that's most common.  If there's no function binding but there is
a variable binding then show that.

If `describe-function' is applied to a non-function (because
elisp-docstring-preview mis-identified what the form created)
then the error is

    Symbol's function definition is void: my-something

There's some secret settings to map \"defxxx\" to a
\"describe-xxx\" which shows it, but the way this is done might
change.

Prior to Emacs 24 `eval-defun' on a `defface' didn't set a new
docstring for the face so the preview won't update there.

A surrounding conditional or macro is allowed.

    (eval-and-compile
      (defun foo ()
        \"Docstring.\"
        123))

    (my-wrapping-macro
      (defun foo () 123))

    (when something
      (defun foo () 456))

For a two-leg `if' the defining form is taken from the first leg.

    (if COND
        (defun foo () 123)   ;; <--- form used
      (defun foo () 456))

It's presumed that something like this will define the same
function or variable in one of two ways.  Perhaps `eval-defun'
could be watched somehow to see which leg the COND gave, but
that's a little difficult.

`static-if', `static-when' and `static-unless' from APEL
static.el are recognised too.

----
The elisp-docstring-preview.el home page is
URL `http://user42.tuxfamily.org/elisp-docstring-preview/index.html'"

  (interactive)
  (eval-defun nil)

  (let ((form (elisp-docstring-form-at-point)))

    (let* ((type (car-safe form))               ;; symbol `defun',`defvar',etc
           (name (or (car-safe (cdr-safe form)) ;; symbol `foo' which is def'ed
                     "Unrecognised toplevel form")))

      ;; name form `(quote foo)' -> symbol `foo'
      (if (and (consp name)
               (eq (car name) 'quote))
          (setq name (car-safe (cdr name))))

      (unless (symbolp type)
        (error "Unrecognised define: %S" type))
      (unless (symbolp name)
        (error "Unrecognised name form: %S %S" type name))

      (let ((prefix (get type 'elisp-docstring-preview-name-prefix)))
        (if prefix
            (setq name (intern (concat prefix (symbol-name name))))))

      (let ((method (get type 'elisp-docstring-preview-name)))
        (if method
            (setq name (funcall method name))))

      (let* ((method (or (get type 'elisp-docstring-preview-describe)

                         ;; do these by regexp to pick up variants like poe,
                         ;; ccmode, static.el, without having to list all
                         ;; individually
                         ;;   defvar
                         ;;   defvaralias
                         ;;   defvar-maybe -- in poe pym.el
                         ;;   defconst
                         ;;   defconst-maybe -- in poe pym.el
                         ;;   static-defconst -- in apel static.el
                         ;;   defcustom
                         ;;   defcustom-c-stylevar -- in ccmode.el
                         ;;   defimage -- makes a variable
                         ;;   define-obsolete-variable-alias
                         ;;   define-compatible-variable-alias (xemacs21)
                         (and (string-match
                               "\\`\\(static-\\)?def\\(var\\|const\\|custom\\|image\\|ine.*-variable\\)"
                               (symbol-name type))
                              'describe-variable)

                         ;; if no func but have variable then describe-variable
                         (and (boundp name)
                              (not (fboundp name))
                              'describe-variable)

                         ;; default is describe-function as it's probably the
                         ;; most likely, or at least made by the most toplevels
                         ;;   defun
                         ;;   defun* -- from cl.el
                         ;;   defun-maybe-cond -- in poe pym.el
                         ;;   defun-when-void (xemacs21)
                         ;;   defadvice
                         ;;   defalias
                         ;;   defalias-maybe -- in poe pym.el
                         ;;   defsubst
                         ;;   defsubst* -- in cl.el
                         ;;   defsubst-maybe -- in poe pym.el
                         ;;   defsubst-maybe-cond -- in poe pym.el
                         ;;   defmacro
                         ;;   defmacro* -- in cl.el
                         ;;   defmacro-maybe -- in poe pym.el
                         ;;   defmacro-maybe-cond -- in poe pym.el
                         ;;   define-modify-macro -- in cl.el, makes a macro
                         ;;   define-compilation-mode
                         ;;   define-compiler-macro
                         ;;   define-derived-mode
                         ;;   define-generic-mode
                         ;;   define-global-minor-mode
                         ;;   define-globalized-minor-mode
                         ;;   define-minor-mode
                         ;;   define-obsolete-function-alias
                         ;;   define-compatible-function-alias (xemacs21)
                         ;;   define-device-method (xemacs21), is a defun
                         ;;   define-device-method* (xemacs21), is a defun*
                         ;;   define-function (xemacs21)
                         ;;   define-function-when-void (xemacs21)
                         ;;
                         'describe-function))

             (buffer  (elisp-docstring-preview--help-buffer method name))
             (wpoints (elisp-docstring-preview--wpoints-get buffer)))
        (save-selected-window ;; don't switch window in xemacs21
          (funcall method name))

        (elisp-docstring-preview--wpoints-set buffer wpoints)))))


;;-----------------------------------------------------------------------------
;; various forms and what describe-*** to call

(put 'define-coding-system 'elisp-docstring-preview-describe
     'describe-coding-system)
(put 'define-coding-system-alias 'elisp-docstring-preview-describe
     'describe-coding-system)

(put 'define-charset 'elisp-docstring-preview-describe
     'describe-character-set)
(put 'define-charset-alias 'elisp-docstring-preview-describe
     'describe-character-set)

(put 'defface 'elisp-docstring-preview-describe
     'describe-face)

;; ibuf-ext.el
(put 'define-ibuffer-filter 'elisp-docstring-preview-name-prefix
     "ibuffer-filter-by-")
(put 'define-ibuffer-sorter 'elisp-docstring-preview-name-prefix
     "ibuffer-do-sort-by-")
(put 'define-ibuffer-column 'elisp-docstring-preview-name-prefix
     "ibuffer-make-column-")
(put 'define-ibuffer-sorter 'elisp-docstring-preview-name-func
     (lambda (name)
       ;; As per define-ibuffer-op itself, add `ibuffer-do-' if not already
       ;; present.  Some `define-ibuffer-op' in ibuffer.el already have the
       ;; prefix, others elsewhere not.
       (if (string-match "\\`ibuffer-do" (symbol-name name))
           (intern (concat "ibuffer-do-" (symbol-name name)))
         name)))

;; calc.el
(put 'defmath 'elisp-docstring-preview-name-prefix "calcFunc-")

;; eieio.el
;; if enough loaded to eval defclass then describe-class is available
(put 'defclass 'elisp-docstring-preview-describe 'describe-class)

;; abbrev.el

;; ENHANCE-ME: `define-abbrev-table' makes a defvar, but its value is better
;; displayed by `insert-abbrev-table-description' or some such.  Is there a
;; describe func for it?
(put 'define-abbrev-table 'elisp-docstring-preview-describe
     'describe-variable)

;; widget.el
;; Experimental opening of `widget-browse'.
;; But the buffer isn't *Help* and has the widget name is in the buffer
;; name, and opens full screen, not other-window style.  So not very good
;; for the window-point saving stuff above.
(put 'define-widget 'elisp-docstring-preview-describe 'widget-browse)

;; widget.el
;; Experimental opening of customize to show a group.
;; But the buffer isn't *Help* and has the widget name is in the buffer
;; name, and opens full screen, not other-window style.  So not quite right
;; for the window-point saving stuff above.
(put 'defgroup 'elisp-docstring-preview-describe 'customize-group)

;; Experimental fun on a toplevel `put'.
;; `elisp-docstring-preview' is only meant for things with docstrings really
(if (eval-when-compile (fboundp 'declare-function))
    (declare-function 'apropos-describe-plist "apropos"))
(put 'put 'elisp-docstring-preview-describe
     (lambda (name)
       (require 'apropos)
       (apropos-describe-plist name)))

;; Other def forms not yet handled:
;;
;; cl setf methods make a `setf-documentation' property, but does anything
;; display it?
;;   defsetf
;;   define-setf-expander
;;   define-setf-method
;;
;; define-category -- describe-categories only looks at current buffer
;; define-ccl-program -- no describe?
;; define-char-code-property -- nothing to describe char table?
;; deftheme -- maybe custom-theme-visit-theme
;; defstruct -- no docstring
;;
;; deftype -- no docstring in cl "type"s?  Sets a `cl-deftype-handler' to a
;; lambda, if that held a docstring which could be picked out.
;;
;; define-egg-environment (xemacs21) -- sets `egg-environ-doc-string'
;; property, but does anything display/describe it?
;;
;; define-error (xemacs21) -- sets `error-message' property, but does
;; anything display/describe that, or is it short and unformatted anyway?
;;
;; help-fns+.el `describe-keymap' is a nicely formatted keymap display.
;; Maybe could notice a defvar with a `keymapp' value and invoke
;; describe-keymap, when available.  Alas help-fns+.el also changes some
;; help-mode-map key bindings which is not good.  Might prefer
;; describe-variable itself to better format various types of values like
;; keymaps, hash tables, etc.


;; LocalWords: docstring docstrings el toplevel buttonize defxxx keymaps
;; LocalWords: defclass foo def xxx

(provide 'elisp-docstring-preview)

;;; elisp-docstring-preview.el ends here
