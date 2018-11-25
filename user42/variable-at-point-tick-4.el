;;; variable-at-point-tick.el --- variable-at-point on a 'foo (for Emacs 23)

;; Copyright 2011, 2013, 2014, 2015 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 4
;; Keywords: convenience, lisp
;; URL: http://user42.tuxfamily.org/variable-at-point-tick/index.html

;; variable-at-point-tick.el is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; variable-at-point-tick.el is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a couple of lines (literally!) to have `variable-at-point'
;; recognise a quoted variable 'foo when point is on the '
;;
;;     (set 'fill-column 70)
;;          ^--- point here
;;
;; This is incorporated in Emacs 24 up and variable-at-point-tick.el is not
;; needed and does nothing there.
;;
;; In Emacs 23.2 for example `variable-at-point' will actually go to the
;; previous symbol, provided it is bound.  For example in the following it
;; gives the preceding `emacs-version',
;;
;;     (list 'emacs-version 'baud-rate)
;;                          ^--- point here

;;; Emacsen:

;; Designed for Emacs 20 and up.  Works in XEmacs 21.
;; Already builtin in Emacs 24 and does nothing there.

;;; Install:

;; Put variable-at-point-tick.el in one of your `load-path' directories, and
;; in your .emacs add
;;
;;     (require 'variable-at-point-tick)
;;
;; It only acts on `variable-at-point' so if desired could require only when
;; that function is loaded, which means eval-after-load of "help.el"
;; (emacs21,xemacs21) or "help-fns.el" (emacs22 up).
;;
;;     (eval-after-load (if (locate-library "help-fns") "help-fns" "help")
;;       '(require 'variable-at-point-tick))

;;; History:

;; Version 1 - the first version
;; Version 2 - check it works on adjacent variables
;; Version 3 - comments that not needed in Emacs 24 up
;; Version 4 - do the already-working check at byte compile time

;;; Code:

;; Check whether `variable-at-point' already works as desired, which is so
;; in emacs24 up.  Note `variable-at-point' optional ANY-SYMBOL arg is new
;; in emacs22, so test it with no args on a bound variable.
(unless (eval-when-compile
          (with-temp-buffer
            (insert "'emacs-major-version\n")
            (save-excursion
              (insert "'emacs-minor-version\n"))
            (eq 'emacs-minor-version (variable-at-point))))

  ;; Explicit dependency on advice.el since
  ;; `variable-at-point-tick-unload-function' needs `ad-find-advice' macro
  ;; when running not byte compiled, and that macro is not autoloaded.
  (require 'advice)

  (defadvice variable-at-point (around variable-at-point-tick activate)
    "Fix for point on the ' of 'foo etc."
    (save-excursion
      (skip-chars-forward "'")
      ad-do-it))

  ;; this cleans up in emacs22 up, but since the advice is only enabled while
  ;; checkdoc-batch executes it doesn't matter if it's left behind in
  ;; emacs21/xemacs21
  ;;
  (defun variable-at-point-tick-unload-function ()
    "Remove advice from `variable-at-point'.
This is called by `unload-feature'."
    (when (ad-find-advice 'variable-at-point 'around 'checkdoc-batch)
      (ad-remove-advice   'variable-at-point 'around 'checkdoc-batch)
      (ad-activate        'variable-at-point))
    nil) ;; and do normal unload-feature actions too

  (unless (with-temp-buffer
            (insert "'emacs-major-version\n")
            (save-excursion
              (insert "'emacs-minor-version\n"))
            (eq 'emacs-minor-version (variable-at-point)))
    (message "variable-at-point-tick.el: Oops, the fix applied doesn't seem to work")))

;; LocalWords: arg

(provide 'variable-at-point-tick)

;;; variable-at-point-tick.el ends here
