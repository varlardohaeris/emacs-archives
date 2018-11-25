;;; visible-color-code.el --- color code strings in current buffer, this elisp show you one as real color.

;; Copyright (C) 2012  podhmo

;; Author: podhmo  
;; Version: 0.0.1
;; Keywords: convenience, color, visible

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

;; 

;;; Code:

;;; color code format example
;;
;; #aaa
;; #aabbcc
;; #000000

(defconst +vcc:color-code-regexp+ "#\\(?:[0-9a-fA-F]\\{6\\}\\|[0-9a-fA-F]\\{3\\}\\)")

;; todo: use defcustom.
(defvar vcc:facemenu-add-new-face-p nil) ;;; not implemented yet
(defvar vcc:facemenu-add-new-color-p nil) ;;; not implemented yet

(defvar vcc:default-face 'bold)
(defvar vcc:default-face-prefix "vcc:")
(defvar vcc:face-customize-function 'vcc:face-customize-function-default)
(defvar vcc:enable-auto-timer-start t)
(defvar vcc:auto-timer nil)
(defvar vcc:auto-timer-interval 0.5)

;; timer
(defun vcc:auto-timer-start () (interactive)
  (unless vcc:auto-timer
    (setq vcc:auto-timer
          (run-with-idle-timer
           vcc:auto-timer-interval
           t
           'visible-color-code))))

(defun vcc:auto-timer-end () (interactive)
  (when vcc:auto-timer
    (cancel-timer vcc:auto-timer)
    (setq vcc:auto-timer nil)))

;; restrictionn
(defmacro vcc:with-visible-area (&rest body)
  (declare (indent 1) (debug ((&rest body))))
  `(save-excursion
    (save-restriction
      (narrow-to-region (window-start) (window-end))
       ,@body)))
 
;; make font

(defun vcc:face-customize-function-default (face color &optional frame)
  (copy-face vcc:default-face face frame)
  (set-face-foreground face color frame))

(defun vcc:make-color-code-face (color)
  ;; memo: (equal (make-face 'foo) (make-face 'foo))
  (when (string-match-p +vcc:color-code-regexp+ color)
    (let* ((name (intern (format "%s%s" vcc:default-face-prefix color)))
           (face (make-face name)))
      (funcall vcc:face-customize-function face color)
      face)))

;; find
(defun vcc:find-color-code-forward (&optional n) 
  (re-search-forward +vcc:color-code-regexp+ nil t (or n 1)))
(defun vcc:find-color-code-backword (&optional n)
  (re-search-backward +vcc:color-code-regexp+ nil t (or n 1)))

(defun vcc:find-color-code (&optional direction)
  (let ((d (or direction 1)))
    (if (>= direction 0)
        (vcc:find-color-code-forward direction)
        (vcc:find-color-code-backword direction))))

;; visible-color
(defun visible-color-code () (interactive)
  (vcc:with-visible-area
   (goto-char (point-min))
   (while (vcc:find-color-code-forward 1)
     (let ((beg (match-beginning 0))
           (end (match-end 0))
           (color (match-string-no-properties 0)))
       (let ((face (vcc:make-color-code-face color)))
         (cond (face (facemenu-set-face face beg end))
               (t (message "%s color is not found. sorry" color))))))))

(define-minor-mode visible-color-code-mode
  "color code format string(e.g. #ffaabb) in current buffer, this elisp show you one as real color."
  nil
  "color code"
  nil
  (when font-lock-mode
    (message "visible color code mode conflicts with `font-lock-mode'!!")
    (sit-for 2)))

(defun visible-color-code-on-hook ()
  (message "visible color code mode ON")
  (when vcc:enable-auto-timer-start
    (vcc:auto-timer-start)))

(defun visible-color-code-off-hook ()
  (message "visible color code mode OFF")
    (vcc:auto-timer-end))

(defvar visible-color-code-mode-on-hook nil)
(add-hook 'visible-color-code-mode-on-hook 
          'visible-color-code-on-hook)
(defvar visible-color-code-mode-off-hook nil)
(add-hook 'visible-color-code-mode-off-hook 
          'visible-color-code-off-hook)


(provide 'visible-color-code)
;;; visible-color-code.el ends here
