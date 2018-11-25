;;; mactag.el --- Mode for automatically handle multiple tags files with Mactag rubygem

;; Copyright (C) 2010 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.0.1
;; Keywords: speed, convenience
;; URL: http://github.com/rejeep/mactag.el

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

;; If your web application is running Rails 3, you can use the Mactag
;; gem with FSSM to automatically update tags files. mactag.el makes
;; sure Emacs tags files list is up to date with those tags files.

;; To use it. Make sure this file is in your Emacs `load-path'. Then:
;;   (require 'mactag)

;; If your Rails project has a directory named `mactag-tags-dir-name'
;; in it's root. The find-tag command will automatically load all
;; tags files and look for tags in them.

;;; Code:

(defvar mactag-tags-dir-name ".tags"
  "Name of TAGS-files directory.")

(defvar mactag-last-mtimes
  (make-hash-table :test 'equal)
  "Cache for project last modification times.")

(defvar mactag-tags-table-lists
  (make-hash-table :test 'equal)
  "Cache for project TAGS-files.")

(defvar mactag-fallback-tags tags-table-list
  "Fallback TAGS-files when Mactag should not be used.")

(defconst mactag-root-regex
  "\\(^[[:alpha:]]:/$\\|^/$\\)"
  "Regular expression matching a file system root.")

(defconst mactag-except-files-regex
  "^.*/\\(\\.\\|\\.\\.\\|#.*#\\)$"
  "Regular expression matching files that should not be included as TAGS-files.")


(defun mactag-last-mtime ()
  "Returns last modification time for current project."
  (gethash (mactag-tags-dir) mactag-last-mtimes))

(defun mactag-set-last-mtime (mtime)
  "Set last modification time for current project."
  (puthash (mactag-tags-dir) mtime mactag-last-mtimes))

(defun mactag-tags-files ()
  "Returns TAGS-files for current project."
  (gethash (mactag-tags-dir) mactag-tags-table-lists))

(defun mactag-set-tags-files (tags-files)
  "Set TAGS-files for current project."
  (puthash (mactag-tags-dir) tags-files mactag-tags-table-lists))

(defun mactag-scan-tags-files ()
  "Scans project tags directory for TAGS-files."
  (remove-if
   (lambda (file)
     (string-match-p mactag-except-files-regex file))
   (directory-files (mactag-tags-dir) t)))

(defun mactag-rebuild (mtime)
  "Rebuilds TAGS-files and updates last modification time."
  (mactag-set-last-mtime mtime)
  (let ((tags-files (mactag-scan-tags-files)))
    (mactag-set-tags-files tags-files)))

(defun mactag-rebuild-if-needed ()
  "Rebuilds TAGS-files if first time or if tags directory have been modified."
  (let ((mtime (nth 5 (file-attributes (mactag-tags-dir)))))
    (if (mactag-initialized-p)
        (if (mactag-modified-p mtime)
            (mactag-rebuild mtime))
      (mactag-rebuild mtime))))

(defun mactag-tag-files ()
  "Returns a list of TAGS-files for project."
  (cond ((mactag-active-p)
         (mactag-rebuild-if-needed)
         (mactag-tags-files))
        (t mactag-fallback-tags)))

(defun mactag-initialized-p ()
  "Checks if project tags have been initialized."
  (not (not (mactag-last-mtime))))

(defun mactag-modified-p (mtime)
  "Checks if project tags have been modified."
  (when mtime
    (let ((time (mactag-last-mtime)))
      (not
       (and
        (= (car mtime) (car time))
        (= (cadr mtime) (cadr time)))))))

(defun mactag-active-p ()
  "Checks if project has any tags directory."
  (file-exists-p (mactag-tags-dir)))

(defun mactag-tags-dir ()
  "Current project tags directory."
  (expand-file-name mactag-tags-dir-name (mactag-root)))

(defun mactag-root (&optional dir)
  "Returns path to Rails root."
  (or dir (setq dir default-directory))
  (if (file-exists-p (expand-file-name "environment.rb" (expand-file-name "config" dir)))
      dir
    (let ((new-dir (expand-file-name (file-name-as-directory "..") dir)))
      (unless (string-match mactag-root-regex dir)
        (mactag-root new-dir)))))

;;;###autoload
(defadvice find-tag (around find-tag-around)
  "Before calling `find-tag', set correct TAGS-files."
  (tags-reset-tags-tables)
  (let ((tags-table-list (mactag-tag-files)) (tags-file-name))
    ad-do-it))
(ad-activate 'find-tag)

(provide 'mactag)

;;; mactag.el ends here
