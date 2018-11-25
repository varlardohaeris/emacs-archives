;;; checkdoc-url-buttonize.el --- check URL `' linking in docstrings

;; Copyright 2010, 2015 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 2
;; Keywords: lisp, maint, checkdoc
;; URL: http://user42.tuxfamily.org/checkdoc-url-buttonize/index.html

;; checkdoc-url-buttonize.el is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; checkdoc-url-buttonize.el is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This spot of code looks for http://... and ftp://... urls in docstrings
;; that can be written URL `http://...' to make buttonized links in the
;; `help-mode' display.  See the `checkdoc-url-buttonize-docstring'
;; docstring below for more.

;;; Emacsen:

;; Designed for Emacs 21 and up, works in XEmacs 21 too.

;;; Install:

;; Put checkdoc-url-buttonize.el in one of your `load-path' directories, and
;; in your .emacs add
;;
;;     (autoload 'checkdoc-url-buttonize-docstring "checkdoc-url-buttonize")
;;     (add-hook 'checkdoc-style-hooks 'checkdoc-url-buttonize-docstring)
;;
;; There's an autoload cookie below for the function, if you know how to use
;; `update-file-autoloads' and friends, then add or customize the hook.

;;; History:

;; Version 1 - the first version
;; Version 2 - new email

;;; Code:

(require 'checkdoc)
(when (locate-library "help-mode") ;; emacs22 up
  (require 'help-mode)) ;; for `help-xref-url-regexp'

(defvar help-xref-url-regexp) ;; quieten the byte compiler in emacs21

;;;###autoload
(defun checkdoc-url-buttonize-docstring (defuninfo endpoint)
  ;; checkdoc-params: (defuninfo endpoint)
  "Check that URLs in docstrings are URL `' linked.
This function is designed for use in `checkdoc-style-hooks'.

A URL written as for example URL `http://gnu.org' is buttonized
in `help-mode' so Ret etc can open it, with `browse-url'.  This
check function looks for http://..., ftp://... etc in a docstring
which might be buttonized that way.

This buttonizing is new in Emacs 22, but does no harm in earlier
versions.  In all likelihood once you know it's available you'll
use it and not need `checkdoc-url-buttonize-docstring', but the
check may help when revisiting old code.  `help-xref-url-regexp'
is the pattern for a buttonized form.

The checking loop here is structured like the builtin `checkdoc'
checks (as of Emacs 23.1), so if you answer \"n\" it means next
docstring, not next URL or more in the current docstring.  This
is not very good as it means you can't pick and choose among
things to correct.

The checkdoc-url-buttonize.el home page is
URL `http://user42.tuxfamily.org/checkdoc-url-buttonize/index.html'

See also `nobreak-fade-emacs-url-p' for nobreak-fade.el if you
want to keep the \"URL\" bit on the same line as the target."

  (let ((regexp (concat "\\(" (if (boundp 'help-xref-url-regexp)
                                  ;; emacs22 up
                                  help-xref-url-regexp
                                ;; emacs21,xemacs21 copy of value from 23.1
                                "\\<[Uu][Rr][Ll][ \t\n]+`\\([^']+\\)'")
                        "\\)\\|\\(s?ftp\\|https?\\)://[^. \"\t\r\n\\][^ \"\t\r\n\\]+[^.,\"' \t\r\n\\]"))
        ret)
    (while (and (not ret)
                (re-search-forward regexp endpoint t))
      (unless (match-beginning 1) ;; the help-xref-url-regexp

        (if (checkdoc-autofix-ask-replace
             (copy-marker (match-beginning 0))
             (copy-marker (match-end 0))
             "Use URL `...' to link? "
             (concat "URL `" (match-string 0) "'")
             t) ;; complex, meaning ask the user
            ;; fix applied, keep looping
            nil
          ;; not applied, stop like builtin checks do
          (setq ret (checkdoc-create-error
                     "URLs in docstrings should use URL `...'"
                     (match-beginning 0)
                     (match-end 0))))))
    ret))

;;;###autoload
(custom-add-option 'checkdoc-style-hooks 'checkdoc-url-buttonize-docstring)

;; LocalWords: http urls docstring docstrings buttonized

(provide 'checkdoc-url-buttonize)

;;; checkdoc-url-buttonize.el ends here
