;;; package-store.el --- a package cache

;; Copyright (C) 2012  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Maintainer: Nic Ferrier <nferrier@ferrier.me.uk>
;; Created: 17th July 2012
;; Version: 0.3
;; Keywords: lisp, http, hypermedia

;; This file is NOT part of GNU Emacs.

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
;; A simple adaption for Emacs' package system, this stores downloaded
;; packages directly in a cache.  This promotes easier transfer to
;; other machines or disconnected (from the Internet) testing.

;;; Source code
;;
;; package-store's code can be found here:
;;   http://github.com/nicferrier/package-store

;;; Style note
;;
;; This codes uses the Emacs style of:
;;
;;    package-store--private-function
;;
;; for private functions.


;;; Code:

(require 'cl)
(require 'ert)

;;;###autoload
(defcustom package-store-cache-dir
  (concat user-emacs-directory "package-cache")
  "The directory to store downloaded packages."
  :type 'directory
  :group 'package)

(defvar package-store-cache-package-name
  nil
  "Special variable declaration for the package name.

This is used to communicate the package name to the name
production function for the url cache:
`package-store-url-cache-create-filename-package'")

(defvar package-store-cache-package-version
  nil
  "Special variable declaration for the package version.

This is used to communicate the package version to the name
production function for the url cache:
`package-store-url-cache-create-filename-package'")

;;;###autoload
(defvar package-store-disconnected
  nil
  "Is the network disconnected?")

;;;###autoload
(defun toggle-package-store-connected (&optional arg)
  "Toggle package network downloads on or off."
  (interactive "P")
  (setq package-store-disconnected
        (if (null arg)
            (not package-store-disconnected)
            (> (prefix-numeric-value arg) 0))))

;;;###autoload
(defun package-store-url-cache-create-filename-package (url)
  "A url cache file namer.

This depends on the special variables
`package-store-cache-package-name' and
`package-store-cache-package-version'."
  (if url
      (let* ((package-name "test")
             (package-version "0.9.9")
             (urlobj (url-generic-parse-url url))
             (url-file-name (url-filename urlobj))
             (protocol (url-type urlobj))
             (hostname (url-host urlobj))
             (host-components
              (cons (or protocol "file")
                    (nreverse
                     (delq nil
                           (split-string (or hostname "localhost")
                                         "\\.")))))
             (dname (file-name-directory url-file-name))
             (ext (file-name-extension url-file-name)))
        (message "package-store url: %s" url)
        (and dname
             (expand-file-name
              (concat
               (mapconcat 'identity host-components "/")
               (file-name-as-directory dname)
               (file-name-as-directory
                (if (symbolp package-store-cache-package-name)
                    (symbol-name package-store-cache-package-name)
                    package-store-cache-package-name))
               package-store-cache-package-version
               "." ext)
              url-cache-directory)))))

(ert-deftest package-store-url-cache-create-filename-package ()
  "Test the cache naming function."
  (let* ((package-store-cache-dir "/home/packagestore/.emacs.d/packagecache")
         (package-store-cache-package-name "eldoc")
         (package-store-cache-package-version "0.9.9")
         (url-cache-directory package-store-cache-dir))
    (should
     (equal
      (package-store-url-cache-create-filename-package
       "http://marmalade-repo.org/package/eldoc-0.9.9.tar")
      (concat "/home/packagestore/.emacs.d"
              "/packagecache/http/org/marmalade-repo"
              "/package/eldoc/0.9.9.tar")))))

;;;###autoload
(defadvice package-download-tar (around
                                 package-store-do-cache-tar
                                 activate)
  "Turn on caching around tar downloads.

Downloads are cached to `package-store-cache-dir'."
  ;; the normal api is (package-download-tar NAME VERSION)
  (if (and
       package-store-cache-dir
       (file-exists-p package-store-cache-dir)
       (file-directory-p package-store-cache-dir))
      (let* ((url-automatic-caching t)
             (url-cache-directory package-store-cache-dir)
             (url-cache-creation-function
              'package-store-url-cache-create-filename-package)
             (package-store-cache-package-name name)
             (package-store-cache-package-version version))
        (if package-store-disconnected
            (flet ((url-retrieve-synchronously (url)
                     ;; STUPID STUPID URL - this is just to fix the
                     ;; bad cache handling
                     (with-current-buffer (url-fetch-from-cache url)
                       (goto-char (point-min))
                       (re-search-forward "\n\n" nil t)
                       (setq url-http-end-of-headers (point))
                       (current-buffer))))
              ad-do-it)
            ;; Else
            ad-do-it))
      ad-do-it))

;;;###autoload
(defadvice package-download-single (around
                                 package-store-do-cache-file
                                 activate)
  "Turn on caching around tar downloads.

Downloads are cached to `package-store-cache-dir'."
  ;; the normal API is (package-download-single NAME VERSION DESC REQUIRES)
  (if (and
       package-store-cache-dir
       (file-exists-p package-store-cache-dir)
       (file-directory-p package-store-cache-dir))
      (let* ((url-automatic-caching t)
             (url-cache-directory package-store-cache-dir)
             (url-cache-creation-function
              'package-store-url-cache-create-filename-package)
             (package-store-cache-package-name name)
             (package-store-cache-package-version version))
        (if package-store-disconnected
            (flet ((url-retrieve-synchronously (url)
                     ;; STUPID STUPID URL - this is just to fix the
                     ;; bad cache handling
                     (with-current-buffer (url-fetch-from-cache url)
                       (goto-char (point-min))
                       (re-search-forward "\n\n" nil t)
                       (setq url-http-end-of-headers (point))
                       (current-buffer))))
              ad-do-it)
            ;; Else
            ad-do-it))
      ad-do-it))

(defun map-regex (buffer regex fn)
  "Map the REGEX over the BUFFER executing FN.

FN is called with the match-data of the regex.

Returns the results of the FN as a list."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let (res)
        (save-match-data
          (while (re-search-forward regex nil t)
            (let ((f (match-data)))
              (setq res
                    (append res
                            (list
                             (save-match-data
                               (funcall fn f))))))))
        res))))

;;;###autoload
(defun package-load-all (filename)
  "Load all the files in a package."
  (interactive "Gfile list:")
  (map-regex
   (find-file-noselect
    (cond
      ((and filename
            (file-exists-p filename)
            (file-directory-p filename))
       (concat (file-name-as-directory filename) "build-parts.txt"))
      ((file-exists-p filename)
       filename)))
   "^\\(.*.el\\(\\.gz\\)*\\)$"
   (lambda (md)
     (let ((filename (match-string 0)))
       (when (file-exists-p filename)
         (load-file filename))))))

(provide 'package-store)

;;; package-store.el ends here
