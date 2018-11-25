;;; pickup.el --- pickup file. 

;; Copyright (C) 2013  podhmo

;; Author: podhmo  
;; Keywords: convenience, file-search, 
;; Version: 0.0.3

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

;;; Example 

;; cwd = /home/<user>/projects/<project>/foo/bar/boo/bee.c

;; get your current project directory
;; (pickup-parent ".git") ;; => /home/<user>/projects/<project>/

;; get your toplevel Makefile in current project
;; (pickup-file "Makefile")  ;; => /home/<user>/projects/<project>/Makefile

;;; Code:


(require 'cl)

(defsubst* %pickup:string-strip-left (string &optional (pat-arg "[ \t]"))
  (replace-regexp-in-string (format "^%s+" pat-arg) "" string))

(defsubst* %pickup:string-strip-right (string &optional (pat-arg "[ \t]"))
  (replace-regexp-in-string (format "%s+$" pat-arg) "" string))

(defsubst %pickup:concat-filename (x y)
  (concat (%pickup:string-strip-right x "/") 
          "/"
          (%pickup:string-strip-left y "/")))


(defsubst %pickup:current-directory ()
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))

(defsubst %pickup:force-directory-name (path) ;xxx
  (or (file-name-directory path) path))

(defsubst %pickup:normalize-filename (dir fname)
  (cond ((file-name-absolute-p fname) fname)
        (t (let ((default-directory (%pickup:force-directory-name dir)))
             (file-truename fname)))))


(defsubst %pickup:normalize-dirname (dirname)
  (concat (%pickup:string-strip-right dirname "/") "/"))

(defsubst %pickup:file-exists-p (target path)
  (file-exists-p (concat path "/" target)))

;;;; config-file
;; config format is one s-expression. (e.g.)
;; (("Makefile" . "./foo/bar/boo/Makefile") 
;;  ("command" . "./foo/bar/boo/bin/command"))

(defun pickup:read-config-file (config-path) ;;TODO:
  (unless (file-exists-p config-path)
    (error "%s is not found" config-path))
  (with-current-buffer (find-file-noselect config-path)
    (save-excursion
      (goto-char (point-min))
      (read (current-buffer)))))

(defun pickup:parse-config (config file)
  (or (assoc-default file config)
      (error "%s is not found in %s" file config)))

;; produce, consume
(defun* pickup:produce-parent-canidates (path &key (delimiter "/"))
  "a/b/c -> (a/b/c a/b a)"
  (destructuring-bind (head . rest) (split-string path delimiter)
    (loop for x in rest
          with acc = head
          unless (string-equal "" x)
          do (setq acc (concat acc "/" x))
          and collect acc into result
          finally return (nreverse (cons head result)))))

(defun pickup:consume-collect-matched-files (candidates target)
  (let ((target (%pickup:string-strip-left target "/"))
        (check 
         (lexical-let ((target target))
           (lambda (path) (%pickup:file-exists-p target path)))))
    (remove-if-not check candidates)))

(defun pickup:consume-find-matched-file (candidates target)
  (let ((target (%pickup:string-strip-left target "/")))
    (find target candidates :test '%pickup:file-exists-p)))


;; toplevel
(defun pickup:get-candidates (produce path)
  (let* ((path (or path (%pickup:current-directory)))
         (produce (or produce pickup:produce-candidates-function)))
    (funcall produce path)))

(defun* pickup:pickup-parent (file &key path produce candidates)
  (let ((candidates (or candidates (pickup:get-candidates produce path))))
    (pickup:consume-find-matched-file candidates file)))

(defun* pickup:pickup-file (file &key path produce candidates)
  (let ((parent (pickup:pickup-parent file :path path :produce produce :candidates candidates)))
    (and parent 
         (concat parent "/" (%pickup:string-strip-left file "/")))))

(defun* pickup:pickup-parent-with-config-file (file &key path produce candidates config)
  (let* ((config-name (or config pickup:reference-path-config-filename))
        (config-path (pickup:pickup-file 
                      config-name :path path :produce produce :candidates candidates)))
    (and config-path 
         (funcall pickup:load-config-function config-path file 'parent))))

(defun* pickup:pickup-file-with-config-file (file &key path produce candidates config)
  (let* ((config-name (or config pickup:reference-path-config-filename))
        (config-path (pickup:pickup-file 
                      config-name :path path :produce produce :candidates candidates)))
    (and config-path 
         (funcall pickup:load-config-function config-path file 'file))))

(defun pickup:load-config (config-path file usecase)
  (let* ((config (pickup:read-config-file config-path))
         (result (pickup:parse-config config file)))
    (case usecase
      (parent 
       (cond ((string-equal file (file-name-nondirectory result))
              (file-name-directory
               (%pickup:normalize-filename config-path result)))
             (t 
              (%pickup:normalize-dirname
               (%pickup:normalize-filename config-path result)))))
      (otherwise
       (cond ((file-directory-p result)
              (%pickup:concat-filename 
               (%pickup:normalize-filename config-path result)
               file))
             (t 
              (%pickup:normalize-filename config-path result)))))))

(defvar pickup:produce-candidates-function 'pickup:produce-parent-canidates)
(defvar pickup:load-config-function 'pickup:load-config)
(defvar pickup:reference-path-config-filename ".pickup")

;; toplevel
(defun* pickup-parent (file &key path produce)
  (let ((candidates (pickup:get-candidates produce path)))
    (or (pickup:pickup-parent
         file :path path :produce produce :candidates candidates)
        (pickup:pickup-parent-with-config-file
         file :path path :produce produce :candidates candidates))))

(defun* pickup-file (file &key path produce)
  (let ((candidates (pickup:get-candidates produce path)))
    (or (pickup:pickup-file
         file :path path :produce produce :candidates candidates)
        (pickup:pickup-file-with-config-file
         file :path path :produce produce :candidates candidates))))

(defun pickup-file-clipboard (file) (interactive "spickup-file: ")
  (let* ((dir (%pickup:current-directory))
         (result (pickup-file file)))
    (cond (result (message "%s is found" result)
                  (kill-new result))
          (t (error "%s is not found (in %s)" file dir)))))

(provide 'pickup)
;;; pickup.el ends here
