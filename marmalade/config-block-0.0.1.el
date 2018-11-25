;;; config-block.el --- config-block is utility for individual settings (e.g. .emacs).

;; Copyright (C) 2012  podhmo

;; Author: podhmo  
;; Keywords: convenience, config, dot-emacs
;; Version: 0.0.1

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

;; ;; how to use
;; dependencies: core:utilities -> foo:utilities -> {foo:addon-A, foo:addon-B, foo:addon-C}
;;
;; (config-block!
;;  nil
;;  (defun core:utilities ()
;;    (defun useful-function0 () nil)
;;    (defun useful-function1 () nil)
;;    (defun useful-function2 () nil)
;;    (defun useful-function3 () nil)))

;; (config-block!
;;  '(core:utilities)
;;  (defun foo:utilities ()
;;    (defun more-useful () nil)
;;    (setq foo-auto-transform-for-convinience t)))

;; (config-block!
;;  '(foo:utilities)
;;  (defun foo:addon-A ()
;;    (message "this is greatest addon")))

;; (config-block!
;;  '(foo:utilities)
;;  (defun foo:addon-B ()
;;    (message "this is greatest addon. yeah!")))

;; (config-block-setup!
;;  '(foo:addon-A
;;    foo:addon-B
;;    ;foo:addon-C
;;    ))


(setq config-block-table (make-hash-table :test 'eq))
(setq config-block-applied-table (make-hash-table :test 'eq)) ;; hash-set

(defun config-block-clean! ()
  (setq config-block-table (make-hash-table :test 'eq))
  (setq config-block-applied-table (make-hash-table :test 'eq)) ;; hash-set
  )

(defun config-block! (required action &optional action-table applied-table)
  (let ((action-table (or action-table config-block-table))
        (applied-table  (or applied-table config-block-applied-table)))
    (remhash action applied-table)
    (%config-block! required action action-table)))

(defun %config-block! (required action action-table)
    (let ((value (gethash action action-table nil)))
      (if value
          (puthash action (union value required) action-table)
        (puthash action required action-table))))
  
(defun config-block-refresh! ()
  (setq config-block-applied-table 
        (make-hash-table :test 'eq)))

(defun config-block-execute-one! (action action-table applied-table)
  (let ((required (gethash action action-table nil)))
    (dolist (prev-action required)
      (config-block-execute-one! prev-action action-table applied-table))
    (unless (gethash action applied-table nil)
      (funcall action)
      (puthash action t applied-table))))

(defun config-block-execute! (target-action-sym &optional action-table applied-table)
  (let ((action-table (or action-table config-block-table))
        (applied-table  (or applied-table config-block-applied-table)))
    (config-block-execute-one! target-action-sym action-table applied-table)))

(defun %config-block-execute-all! (action-table applied-table)
  (loop for k being the hash-keys in action-table
        do (config-block-execute! k action-table applied-table)))

(defun config-block-execute-all! (&optional action-table applied-table)
  (let ((action-table (or action-table config-block-table))
        (applied-table  (or applied-table config-block-applied-table)))
    (destructuring-bind (status failed)
        (config-block-typo-check-full action-table)
      (cond (status
             (loop for k being the hash-keys in action-table
                   do (config-block-execute! k action-table applied-table)))
            (t (error "these symbols are typo? %s" failed))))))

(defun %config-block-default-error-treat-fn (v)
  (message "%s is not found in config-block. (typo?)" v))

(defun %config-block-typo-check (iterator actioin-table error-treat-fn)
  (let ((check-status 'ok)
        (failed (list)))
    (loop for v in iterator
          when (eq (gethash v action-table 'fail) 'fail)
          do (progn (funcall error-treat-fn v)
                    (push v failed)
                    (setq check-status 'ng)))
    (values check-status (delete-duplicates failed))))

(defun config-block-typo-check-full (&optional action-table error-treat-fn)
  (let ((action-table (or action-table config-block-table))
        (error-treat-fn #'%config-block-default-error-treat-fn))
    (let ((iterator (loop for vs being the hash-values in action-table
                          nconc vs)))
      (%config-block-typo-check iterator action-table error-treat-fn))))

(defun config-block-setup! (required &optional action-table applied-table error-treat-fn)
  (let ((action-table (or action-table config-block-table))
        (applied-table  (or applied-table config-block-applied-table))
        (error-treat-fn #'%config-block-default-error-treat-fn))
    (destructuring-bind (status failed)
        (%config-block-typo-check required action-table error-treat-fn)
      (cond (status
             (loop for r in required 
                   do (config-block-execute! r action-table applied-table)))))))

(provide 'config-block)
;;; config-block.el ends here
