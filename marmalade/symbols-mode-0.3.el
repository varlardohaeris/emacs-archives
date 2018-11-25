;;; symbols-mode.el --- List symbols of object files

;; Copyright (C) 2012  Matthias Meulien

;; Author: Matthias Meulien <orontee@gmail.com>
;; Keywords: tools, data
;; URL: https://gitorious.org/symbols-mode-el
;; Version: 0.3

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See `list-symbols' docstring for usage information.

;; Todos/Ideas:

;; - Add argument to `list-symbols' to edit nm command
;; 
;; - Tool tips in type column
;; 
;; - Command to toggle display of file names
;; 
;; - Check existence of file (default to a.out)
;;
;; - Check that nm is in path
;; 
;; - Integration to `compilation-mode'

;;; Code:

(defgroup symbols nil
  "List symbols from object files."
  :group 'tools
  :version "24.0")

(defvar symbols-object-file nil)

(defvar symbols-buffer-name "*Symbol List*")

(defun symbols-set-and-refresh (symbol value)
  (set-default symbol value)
  (condition-case nil
      (with-current-buffer symbols-buffer-name
	(revert-buffer))
    (error nil)))

(defun symbols-set-and-list (symbol value)
  (set-default symbol value)
  (condition-case nil
      (with-current-buffer symbols-buffer-name
	(list-symbols symbols-object-file))
    (error nil)))

;;;###autoload
(defcustom symbols-values-type 16
  "Symbol values are printed on 16 or 8 columns depending on the
the type of the targeted os, 64 bits or 32 bits os."
  :type '(radio (const :tag "64 bits" 16)
		(const :tag "32 bits" 8))
  :set 'symbols-set-and-list
  :group 'symbols)

;;;###autoload
(defcustom symbols-values-radix "x"
  "Radix to use for printing the symbol values."
  :type '(radio (const :tag "Decimal" "d")
		(const :tag "Octal" "o")
		(const :tag "Hexadecimal" "x"))
  :set 'symbols-set-and-refresh
  :group 'symbols)

;;;###autoload
(defcustom symbols-demangle-names t
  "Non-nil means decode low-level symbol names into user-level names."
  :type 'boolean
  :set 'symbols-set-and-refresh
  :group 'symbols)

;;;###autoload
(defcustom symbols-undefined-symbols-only nil
  "Non-nil means display only undefined symbols."
  :type 'boolean
  :set 'symbols-set-and-refresh
  :group 'symbols)

(defvar symbols-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "i" 'symbols-display-manual)
    (define-key map "t" 'symbols-toggle-demangling)
    (define-key map "u" 'symbols-toggle-undefined-only)
    map)
  "Keymap used for programming modes.")

(define-derived-mode symbols-mode tabulated-list-mode "Symbols"
  "Major mode for listing the symbols from an object file.

The list of symbols is obtained from the nm tool."
  (setq tabulated-list-format
	`[("Value" ,symbols-values-type t)
	 ("T" 1 t)
	 ("Name" 60 t)
	 ("File" 0 t)])
  (make-local-variable 'symbols-object-file)
  (setq tabulated-list-sort-key (cons "Name" nil))
  (add-hook 'tabulated-list-revert-hook 'symbols-list-symbols--refresh nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun list-symbols (file)
  "Display the list of symbols in FILE.

The return is always nil."
  (interactive "fFile name: ")
  (setq buffer (get-buffer-create symbols-buffer-name))
  (with-current-buffer buffer
    (symbols-mode)
    (setq symbols-object-file file)
    (symbols-list-symbols--refresh)
    (tabulated-list-print))
  (display-buffer buffer)
  nil)

(defun symbols-list-symbols--refresh ()
  "Recompute the list of symbols from `symbols-object-file'."
  (setq tabulated-list-entries nil)
  (if symbols-object-file
      (let ((file symbols-object-file)
	    (entries nil))
	(progn
	  (with-temp-buffer
	    (let ((args (concat "-l"
				(if symbols-demangle-names "C")
				(if symbols-undefined-symbols-only "u")
				"t" symbols-values-radix)))
	      (call-process "nm" nil (current-buffer) nil args (expand-file-name file))
	      (goto-char (point-min))
	      (while
		  (re-search-forward
		   "\\(\[0-9a-f \]+\\) \\(\[AbBCDdGgiNpRrSsTtUuVvWw-?\]\\) \\(.*\\)" nil t)
		(let* ((value (match-string 1))
		       (type (match-string 2))
		       (others (match-string 3))
		       (name (car (split-string others "\t")))
		       (location (or (cadr (split-string others "\t")) ""))
		       (file (car (split-string location ":")))
		       (line (cadr (split-string location ":")))
		       (label
			(if (and (stringp file) (file-exists-p file) line)
			    `(,name
			      face link
			      help-echo ,(concat "Jump to line " line " of file `" file "'")
			      follow-link t
			      file ,file
			      line ,(string-to-number line)
			      action symbols-find-file)
			  name)))
		  (push (list (line-number-at-pos) (vector value type label location))
			entries)))))
	  (setq tabulated-list-entries entries)))
    (message "No object file associated to buffer")))

(defun symbols-find-file (button)
  (let ((line (button-get button 'line))
	(buff (find-file (button-get button 'file))))
    (when (numberp line)
      (with-current-buffer buff
	(goto-char (point-min)) (forward-line (1- line))))))

(require 'info)
(defun symbols-display-manual ()
  "Go to Info buffer that displays nm manual, creating it if none
already exists."
  (interactive)
  (let ((blist (buffer-list))
	(manual-re (concat "\\(/\\|\\`\\)" "binutils" "\\(\\.\\|\\'\\)"))
	(case-fold-search t)
	found)
    (dolist (buffer blist)
      (with-current-buffer buffer
	(if (and (eq major-mode 'Info-mode)
		 (stringp Info-current-file)
		 (string-match manual-re Info-current-file))
	    (setq found buffer
		  blist nil))))
    (if found
	(switch-to-buffer found)
      (info-initialize)
      (info (Info-find-file "binutils")))
    (Info-goto-node "(binutils)nm")))

(defun symbols-toggle-demangling ()
  "Toggle whether to decode low-level symbol names into
user-level names."
  (interactive)
  (setq symbols-demangle-names (not symbols-demangle-names))
  (symbols-list-symbols--refresh)
  (tabulated-list-print))

(defun symbols-toggle-undefined-only ()
  "Toggle whether to display only undefined symbols or not."
  (interactive)
  (setq symbols-undefined-symbols-only (not symbols-undefined-symbols-only))
  (symbols-list-symbols--refresh)
  (tabulated-list-print))

(provide 'list-symbols)
(provide 'symbols-mode)

;;; symbols-mode.el ends here
