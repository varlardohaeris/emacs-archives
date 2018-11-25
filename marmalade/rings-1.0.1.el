;;; rings.el --- Buffer rings. Like tabs, but better.

;; Copyright 2013 Konrad Scorciapino

;; Author: Konrad Scorciapino
;; Keywords: utilities, productivity
;; URL: http://github.com/konr/rings
;; Version: 1.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;; Code goes here
(require 'cl)


(defmacro rings->> (x &optional form &rest more)
  "Like clojure's ->>"
  (if (null form) x
    (if (null more)
        (if (sequencep form)
            `(,(car form) ,@(cdr form) ,x)
          (list form x))
      `(rings->> (rings->> ,x ,form) ,@more))))

(defun rings-toggle-buffer (key)
  (let ((variable-name (intern (format "rings-%s" key))))
    (if (boundp variable-name)
        (progn (kill-local-variable variable-name)
               (message "Removed!"))
      (progn (set (make-local-variable variable-name) t)
             (message "Added!")))))

(defun rings-buffers (key)
  (remove-if-not
   (lambda (x) (assoc (intern (format "rings-%s" key)) (buffer-local-variables x)))
   (buffer-list)))

(defun rings-cycle (key)
  (let ((buffers (sort (mapcar #'buffer-name (rings-buffers key)) #'string<))
        (current (buffer-name (current-buffer))))
    (if (not buffers) (message "Empty group!")
      (loop for all = (append buffers buffers) then (cdr all)
            until (or (not all) (equal current (car all)))
            finally
            (let ((new (or (cadr all) (car buffers))))
              (switch-to-buffer (get-buffer new))
              (rings->> buffers (mapcar (lambda (b)
                                     (if (equal b new) (format "((%s))" b) b
                                        ; <taylanub> konr: I'm not sure if that's a good idea; the message-area is
                                        ;  supposed to print the object in an unambiguous way ...
                                        ;
                                        ;(propertize current
                                        ;'font-lock-face '(:weight
                                        ;bold :foreground
                                        ;"#ff0000")) b
                                         )))
                   (mapcar (lambda (x) (concat x " ")))
                   (apply #'concat) message))))))

(defmacro rings-generate-setter (key)
  `(lambda () (interactive) (rings-toggle-buffer ,key)))

(defmacro rings-generate-cycler (key)
  `(lambda () (interactive) (rings-cycle ,key)))


(provide 'rings)
;;; rings.el ends here
