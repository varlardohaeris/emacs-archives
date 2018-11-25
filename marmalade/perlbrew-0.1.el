;;; perlbrew.el --- basic support for perlbrew environments

;; -*- coding: utf-8 -*-

;; Author: Fabrice Gabolde <fabrice.gabolde@gmail.com>
;; Maintainer: Fabrice Gabolde <fabrice.gabolde@gmail.com>
;; Created: 20 Jun 2012
;; Version: 0.1
;; Keywords: perl, perlbrew
;; Package-Requires: ((cl "0"))

;; This file is not part of GNU Emacs.

;; perlbrew.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; perlbrew.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with perlbrew.el. If not, see <http://www.gnu.org/licenses/>.

;;; Requirements:

;; + Emacs 23 (probably works in 22)
;;
;; + The `cl' package for a couple list operations
;;
;; + At least one perl brew installed (perlbrew itself is not
;;   required)

;;; Installation:

;; Copy perlbrew.el to your LOAD-PATH and customize:
;;
;; + `perlbrew-perls-dir'
;;
;; The default value is OK for default perlbrew installations
;; (~/perl5/perlbrew/perls).  It's probably a bad idea to customize
;; this variable in the middle of a session.  You should do
;; (perlbrew-off) before customizing and turn it back on afterwards.

;;; Usage:

;; To switch to a perlbrew-managed perl, just do
;;
;; M-x perlbrew-use BREW-NAME RET
;;
;; This modifies your `exec-path', PATH environment variable, and
;; `woman-path' to include the specified brew's directories.  WoMan's
;; index will be rebuilt the next time `woman' is called.  Now, if you
;; run
;;
;; M-! perl -v
;;
;; you'll notice it reports your brewed perl's version, and WoMan can
;; display man pages from the modules you've installed in the brewed
;; "lib" directory.  Binaries from the brewed "bin" are also
;; available.
;;
;; If you wish to return to the system perl, do
;;
;; M-x perlbrew-off

;;; Code:

(require 'cl)

(defcustom perlbrew-perls-dir
  (concat (getenv "HOME") "/perl5/perlbrew/perls/")
  "Path where perlbrew's perls directory can be found.")

(defun perlbrew-use (brew)
  (interactive
   (list
    (completing-read "Brew name (leave empty to turn perlbrew off): "
                     (perlbrew-list-installed-brews))))
  (if brew
      (if (perlbrew-set-current-brew brew)
          (progn
            (perlbrew-set-current-perl-path)
            (perlbrew-set-current-exec-path)
            (perlbrew-set-current-woman-path))
        (message (concat "Could not load " brew " perl version.")))
    (perlbrew-off)))

(defun perlbrew-off ()
  "Deactivate perlbrew until next call to `perlbrew-use'."
  (interactive)
  (setq perlbrew-current-brew nil)
  (perlbrew-set-current-perl-path)
  (perlbrew-set-current-exec-path)
  (perlbrew-set-current-woman-path))

(defun perlbrew-brew-exists (brew)
  "Test if a brew looks like it's installed."
  (let ((brewdir (concat perlbrew-perls-dir brew)))
    (and (file-exists-p brewdir) (file-directory-p brewdir))))

(defun perlbrew-list-installed-brews ()
  "List installed brews."
  (remove-if-not (lambda (filename) (file-directory-p (concat (file-name-directory perlbrew-perls-dir) filename)))
             (directory-files perlbrew-perls-dir nil "\[^.\]")))

(defun perlbrew-set-current-exec-path ()
  "Set current EXEC to include our brew's /bin directory."
  (setq exec-path
        (remove-if (lambda (path) (string-match (concat "^" perlbrew-perls-dir) path)) exec-path))
  (if perlbrew-current-brew
      (add-to-list 'exec-path (concat (file-name-directory perlbrew-perls-dir) perlbrew-current-brew
                                      "/bin")))
  (setenv "PATH"
          (reduce
           (lambda (a b) (concatenate 'string a ":" b))
           exec-path)))

(defun perlbrew-set-current-perl-path ()
  "Set the path to the current brew's perl interpreter."
  (setq perlbrew-current-perl-path
        (if perlbrew-current-brew
            (concat (file-name-directory perlbrew-perls-dir) perlbrew-current-brew "/bin/perl")
          "/usr/bin/perl")))

(defun perlbrew-set-current-woman-path ()
  "Set the path to the current brew's /man directory."
  (eval-after-load "woman"
    (progn
      (setq woman-path
            (remove-if (lambda (path) (string-match (concat "^" perlbrew-perls-dir) path)) woman-path))
      (if perlbrew-current-brew
          (add-to-list 'woman-path
                       (concat (file-name-directory perlbrew-perls-dir) perlbrew-current-brew
                               "/man/.*")))
      (setq woman-cached-data nil))))

(defun perlbrew-set-current-brew (brew)
  (if (perlbrew-brew-exists brew)
      (setq perlbrew-current-brew brew)
    nil))

(provide 'perlbrew)
;;; perlbrew.el ends here
