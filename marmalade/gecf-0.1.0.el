;;; gecf.el --- Göktu's Emacs configuration framework.

;; Copyright (C) 2014 Göktu Kayaalp <self@gkayaalp.com>


;; Author: Göktu Kayaalp <self@gkayaalp.com>
;; URL: http://github.com/cadadr/gecf
;; Version: 0.1.0

;; This file is not part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; GECF is the teeny tiny configuration framework to ease up
;; configuring the monstrously configurable, hornily extendable LISP
;; environment, Emacs.  It provides a thin layer of abstraction over
;; package initialisation, and some auxiliary functions for setting up
;; the Emacs environment.

;; See the README.markdown file [1] for more information.
;;
;; [1] https://raw.github.com/cadadr/gecf/master/README.markdown

;;; Code:

(require 'cl)
(require 'package)

(defgroup :gecf
  '((gecf/init-directory text))
  "Göktu's Emacs configuration framework."
  :prefix "gecf/"
  :group  'convenience)

(defcustom gecf/init-directory
  (expand-file-name "~/.emacs.d/init")
  "The directory to contain initialisation files."
  :group 'gecf)

(defmacro gecf/mode-setup (mode &rest body)
  "Add a hook to the given MODE.

MODE is the name of the mode except the `-mode' part.

BODY is code to execute when that mode is selected.

Example usage:

  (gecf/mode-setup emacs-lisp
    (setq indent-tabs-mode nil))"
  (progn (let ((hook-sym (intern (concat (symbol-name mode) "-mode-hook"))))
	   (eval `(setq ,hook-sym nil))
	   (eval `(add-to-list (quote ,hook-sym)
		    (lambda () ,@body))))
   nil))

(defun gecf/use-repos (repos)
  "Set which ELPA repos to use.

REPOS is a alist of repo names mapped to repo urls.

Example usage:

  (gecf/use-repos '((\"gnu\" . \"http://elpa.gnu.org/packages/\")
                     (\"marmalade\" . \"http://marmalade-repo.org/packages/\")
                     (\"melpa\" . \"http://melpa.milkbox.net/packages/\")))
"
  (setq package-archives nil)
  (dolist (repo repos)
    (add-to-list 'package-archives repo)))

(defun gecf/init ()
  "Initialise GECF.

This function will trigger GECF initialisation process, which
will load all the files under the ``gecf/init-directory''
directory."
  ;; Refresh the archive contents if not so already.
  (when (not package-archive-contents)
    (package-refresh-contents))
  ;; Load the files from ``gecf/init-directory''.
  (let ((files (mapcar #'car (directory-files-and-attributes gecf/init-directory t "\\.el$"))))
    (dolist (file files)
      (load file))))

(defun gecf/ensure-packages (package-list)
  "Install packages in PACKAGE-LIST if not so already.

PACKAGE-LIST is a list of symbols, each symbol being a package name."
  (dolist (package package-list)
    (when (not (package-installed-p package))
      (package-install package))))

(defun gecf/apply-to-colon-seperated-list (f cls)
  (let* ((ls (split-string cls ":" t)))
    (mapconcat 'identity (funcall f ls) ":")))

(defun gecf/apply-to-path (f)
    (let ((newpath (gecf/apply-to-colon-seperated-list
                    f (getenv "PATH"))))
    (setenv "PATH" newpath)
    newpath))

(defun gecf/unshift-to-path (dir)
  "Insert DIR to the front of the $PATH environment variable."
  (gecf/apply-to-path (lambda (path) (cons (expand-file-name dir) path))))

(defun gecf/push-to-path (dir)
  "Push DIR to the back of $PATH environment variable."
  (gecf/apply-to-path (lambda (path) `(,@path ,(expand-file-name dir)))))

(defun gecf/filter-path (p)
  "Filter the PATH environment variable with the predicate P of arity 1."
  (gecf/apply-to-path (lambda (path) (remove-if p path))))

;; Initialise the configuration and load ``gecf/init-directory'' directory.
(package-initialize)

(provide 'gecf)
;;; gecf.el ends here.
