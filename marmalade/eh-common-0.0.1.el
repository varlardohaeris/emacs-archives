;;; eh-common.el --- Tumashu's  emacs functions

;; Copyright (c) 2011, Feng Shu

;; Author: Feng Shu <tumashu@gmail.com>
;; URL: https://github.com/tumashu/tumashu.github.com
;; Version: 0.0.1

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  �*��/tumashu*�(�emacs�pemacs-�(7���

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
;; lq(��p  
(defun eh-toggle-line-spacing ()
  "tؤL݄�p,�1� 05� ,"
  "e�: [N@Q](http://xahlee.org/emacs/emacs_make_modern.html)"
  (interactive)
  (if (eq line-spacing 1)
      (setq-default line-spacing 5)
    (setq-default line-spacing 1))
  )

(defun eh-reload-config ()
  "ΰ�}Mn��"
  (interactive)
  (load "$HOME/.emacs"))


(defun eh-increase-font-size ()
  ">'WS��p"
  "e�: [sacha chua](http://sachachua.com/wp/2006/09/15/emacs-changing-the-font-size-on-the-fly/)"
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (ceiling (* 1.10
                                  (face-attribute 'default :height)))))
(defun eh-decrease-font-size ()
  ")WS��p"
  "e�: [sacha chua](http://sachachua.com/wp/2006/09/15/emacs-changing-the-font-size-on-the-fly/)"
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (floor (* 0.9
                                (face-attribute 'default :height)))))

(defun eh-kill-ring-save (&optional n)
  "�peh-kill-ring-save,S`�		- *:߄�"
  "eh-kill-ring-save6I@(L"
  "�I�Mn(��,SX(	-:��,L:�kill-ring-save 7" 
  (interactive "p")
  (if mark-active
      (kill-ring-save (region-beginning) (region-end))
    (if (> n 0)
        (kill-ring-save (line-beginning-position)
                        (line-end-position n))
      (kill-ring-save (line-beginning-position n)
                      (line-end-position)))))

(defun eh-kill-region (&optional n)
  "�peh-kill-regin,S`�		- *:߄�"
  "eh-kill-reginjI@(L"
  "�I�Mn(��,SX(	-:��,L:�kill-region 7" 
  (interactive "p")
  (if mark-active
      (kill-region (region-beginning) (region-end))
    (if (> n 0)
        (kill-region (line-beginning-position)
                     (line-end-position n))
      (kill-region (line-beginning-position n)
                   (line-end-position)))))

(defun eh-copy-line (&optional arg)
  "�SML��p"
  (interactive "P")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (copy-region-as-kill beg end))
  )

(defun eh-copy-word (&optional arg)
  "�SMǗ�p"
  (interactive "P")
  (let ((beg (progn (if (looking-back "[a-zA-Z0-9]" 1) (backward-word 1)) (point)))
        (end (progn (forward-word arg) (point))))
    (copy-region-as-kill beg end))
  )

(defun eh-copy-paragraph (&optional arg)
  "�SM�=��p"
  (interactive "P")
  (let ((beg (progn (backward-paragraph 1) (point)))
        (end (progn (forward-paragraph arg) (point))))
    (copy-region-as-kill beg end))
  )

(defun eh-dos2unix () 
  "dosbL�lb:unix�bL�,(��d^M"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

(defun eh-unix2dos ()
  "unixbL�lb:dos�bL�"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))

;;  '��
(defun eh-maximized ()
  (interactive)
  (x-send-client-message
   nil 0 nil "_NET_WM_STATE" 32
   '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
  (x-send-client-message
   nil 0 nil "_NET_WM_STATE" 32
   '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  )

;; hO
(defun eh-fullscreen ()
  (interactive)
  (x-send-client-message
   nil 0 nil "_NET_WM_STATE" 32
   '(2 "_NET_WM_STATE_FULLSCREEN" 0))
  )

;;;###autoload(require 'eh-common)

(provide 'eh-common)
;;; eh-common.el ends here
