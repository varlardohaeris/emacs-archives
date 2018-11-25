;;; citrus-mode.el --- Major mode for editing Citrus files

;; Copyright (C) 2010 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.0.2
;; Keywords: citrus, peg
;; URL: http://github.com/rejeep/citrus-mode

;; This file is NOT part of GNU Emacs.


;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:

;; citrus-mode is a major mode for editing Citrus
;; (http://mjijackson.com/citrus/index.html) files.

;; To use citrus-mode mode, make sure that this file is in Emacs load-path:
;;   (add-to-list 'load-path "/path/to/directory/or/file")
;;
;; Then require citrus-mode:
;;   (require 'citrus-mode)


;;; Code:

(defvar citrus-mode-map (make-sparse-keymap)
  "Keymap for `citrus-mode'.")

(defvar citrus-mode-syntax-table (make-syntax-table)
  "Syntax map for `citrus-mode'.")

(defvar citrus-mode-hook nil
  "Mode hook for `citrus-mode'")

(defconst citrus-mode-grammar-regex
  "^\\s-*grammar"
  "Regex matching grammar line.")

(defconst citrus-mode-rule-regex
  "^\\s-*rule"
  "Regex matching rule line.")

(defconst citrus-mode-end-regex
  "^\\s-*end"
  "Regex matching end line.")

(defconst citrus-mode-include-regex
  "^\\s-*include"
  "Regex matching include line.")

(defconst citrus-mode-blank-line-regex
  "^\\s-*$\\|$"
  "Regex matching blank line.")

(defconst citrus-mode-font-lock-keywords
  '(("\\<rule\\>" . font-lock-keyword-face)
    ("\\<rule\\> \\([a-z_]+\\)" 1 font-lock-function-name-face)
    ("\\<grammar\\>" . font-lock-keyword-face)
    ("\\<grammar\\> \\([A-Z][A-Za-z]*\\)" 1 font-lock-type-face)
    ("\\<include\\> \\([A-Z][A-Za-z]*\\)" 1 font-lock-type-face)
    ("\\<end\\>" . font-lock-keyword-face))
  "Font lock keywords for `citrus-mode'.")


(defun citrus-mode-indent-line ()
  "Indentation of the current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at citrus-mode-grammar-regex)
        (indent-line-to 0)
      (if (looking-at citrus-mode-rule-regex)
          (indent-line-to 2)
        (if (looking-at citrus-mode-include-regex)
            (indent-line-to 2)
          (if (looking-at citrus-mode-end-regex)
              (if (save-excursion
                    (forward-line -1)
                    (while (looking-at citrus-mode-blank-line-regex)
                      (forward-line -1))
                    (looking-at citrus-mode-end-regex))
                  (indent-line-to 0)
                (indent-line-to 2))
            (indent-line-to 4))))))
  (if (looking-back "^\\s-*")
      (back-to-indentation)))

;;;###autoload
(defun citrus-mode ()
  "Major mode for Citrus files."
  (interactive)

  (kill-all-local-variables)
  (use-local-map citrus-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(citrus-mode-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'citrus-mode-indent-line)

  (set-syntax-table citrus-mode-syntax-table)
  (modify-syntax-entry ?_ "_" citrus-mode-syntax-table)
  (modify-syntax-entry ?# "<" citrus-mode-syntax-table)
  (modify-syntax-entry ?\n ">" citrus-mode-syntax-table)
  (modify-syntax-entry ?' "\"" citrus-mode-syntax-table)

  (setq comment-start "#")
  (setq major-mode 'citrus-mode)
  (setq mode-name "Citrus")
  (run-hooks 'citrus-mode-hook))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.citrus$" . citrus-mode))


(provide 'citrus-mode)

;;; citrus-mode.el ends here