;;; copyright-both-ends.el --- update copyright at start and end of file

;; Copyright 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 5
;; Keywords: tools, copyright
;; URL: http://user42.tuxfamily.org/copyright-both-ends/index.html
;; EmacsWiki: CopyrightUpdate

;; copyright-both-ends.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; copyright-both-ends.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; `copyright-both-ends-update' updates a copyright notice at both start and
;; end of a file using `copyright-update' with `copyright-at-end-flag' both
;; true and false.  See the `copyright-both-ends-update' docstring for
;; details.

;;; Install:

;; Put copyright-both-ends.el in one of your `load-path' directories and the
;; following in your .emacs
;;
;;     (autoload 'copyright-both-ends-update "copyright-both-ends" nil t)
;;
;; This makes M-x copyright-both-ends-update available, but see the
;; docstring for suggested `before-save-hook'.
;; 
;; There's an autoload cookie for the function and custom option if you
;; install via `M-x package-install' or know `update-file-autoloads'.

;;; Emacsen:

;; Designed for Emacs 23 up, does a single update in Emacs 21 and 22.
;; Doesn't work in XEmacs 21 (doesn't have copyright.el).

;;; History:

;; Version 1 - the first version
;; Version 2 - one end when copyright-at-end-flag not available
;;           - message if base copyright-update not available at all
;; Version 3 - defvars to quieten the byte compiler
;; Version 4 - in the sample code conditionalize make-local-hook
;; Version 5 - new email

;;; Code:

;; in copyright.el, but only when that package is available, so defvar here
(defvar copyright-update)
(defvar copyright-at-end-flag)

;;;###autoload
(defun copyright-both-ends-update ()
  "Update copyright years at both start and end of the buffer.
This function calls `copyright-update' with
`copyright-at-end-flag' set to true then to false, so if you have
a copyright in comments at the start then also inline in
documentation at the end of the file then both are updated.

Like function `copyright-update', an update is only done if the
variable `copyright-update' says it hasn't already been done.
Don't run the plain `copyright-update' before this both-ends one,
or it'll look like the update has been done.

`copyright-both-ends-update' can be added to `before-save-hook'
to have an automatic update when saving any file

    (add-hook 'before-save-hook 'copyright-both-ends-update)

Chances are you'll want it only for your own work, in which case
try conditionalizing with say the following for only files under
your home directory

    (defun my-enable-copyright-update ()
      (when (string-match (concat \"\\=\\`\" (regexp-quote
                                         (expand-file-name \"~/\")))
                          (buffer-file-name))
        (if (fboundp 'make-local-hook)
            (make-local-hook 'before-save-hook)) ;; for xemacs
        (add-hook 'before-save-hook 'copyright-both-ends-update
                  nil
                  t))) ;; buffer-local
    (add-hook 'find-file-hooks 'my-enable-copyright-update)

In Emacs 22 and earlier there's no `copyright-at-end-flag' and
`copyright-both-ends-update' does just a single update.  This
won't do what's intended, but at least a common
`before-save-hook' setup won't error out."

  (interactive)
  (if (not (fboundp 'copyright-update))
      (message "`copyright-update' not available")

    (when (or (not (boundp 'copyright-update)) ;; not yet loaded
              copyright-update)  ;; or update pending for this buffer
      (copyright-update)
      (when (boundp 'copyright-at-end-flag) ;; new in emacs23
        (let ((copyright-at-end-flag (not copyright-at-end-flag)))
          (setq copyright-update t)
          (copyright-update))))))

;;;###autoload
(custom-add-option 'before-save-hook 'copyright-both-ends-update)

;; LocalWords: conditionalizing docstring

(provide 'copyright-both-ends)

;;; copyright-both-ends.el ends here
