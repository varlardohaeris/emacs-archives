;;; hippie-expand-haskell.el --- Hippie expand try function using ghc's completion function.
;;
;; Filename: hippie-expand-haskell.el
;; Description: Hippie expand try function using ghc's completion function.
;;              Inspire by: https://github.com/purcell/hippie-expand-slime
;;              and much from it.
;; Author: Jiajian Huang
;; Created: Sun Dec 23 15:42:05 2012 (+0800)
;; Version: 0.0.1
;; URL: https://github.com/ispinfx/hippie-expand-haskell.el
;; Keywords: hippie-expand ghc haskell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Usage:
;;
;;  (add-hook 'haskell-mode-hook 'set-up-haskell-hippie-expand)
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'ghc)

;;;###autoload
(defun try-expand-haskell (old)
  "Try function for `hippie-expand' using ghc's completion function."
  (if (not old)
      (progn
        (he-init-string (ghc-completion-start-point) (point))
        (if (not (equal he-search-string ""))
            (setq he-expand-list
                  (sort
                   (all-completions he-search-string (ghc-select-completion-symbol))
                   'string<))
          (setq he-expand-list '()))))
  (while (and he-expand-list
              (he-string-member (car he-expand-list) he-tried-table))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn
        (if old (he-reset-string))
        nil)
    (progn
      (he-substitute-string (car he-expand-list))
      (setq he-tried-table (cons (car he-expand-list) (cdr he-tried-table)))
      (setq he-expand-list (cdr he-expand-list))
      t)))

;;;###autoload
(defun set-up-haskell-hippie-expand ()
  "Use `ghc-select-completion-symbol' as a hippie expand try function.
Will add `try-expand-haskell' to the front of `hippie-expand-try-functions-list'"
  (interactive)
  (set (make-local-variable 'hippie-expand-try-functions-list) hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-expand-haskell))

(provide 'hippie-expand-haskell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hippie-expand-haskell.el ends here
