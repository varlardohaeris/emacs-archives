;;; ffap-rfc-directories.el --- look for RFCs in local directories too (for Emacs 22)

;; Copyright 2007, 2008, 2009, 2010, 2015 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 9
;; Keywords: files, ffap, rfc
;; URL: http://user42.tuxfamily.org/ffap-rfc-directories/index.html
;; EmacsWiki: FindFileAtPoint

;; ffap-rfc-directories.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; ffap-rfc-directories.el is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This spot of code makes M-x ffap look in some local directories for RFC
;; files before offering the ftp download in `ffap-rfc-path'.  It's good if
;; you keep copies of some RFCs locally, including perhaps from a packaged
;; distribution etc.
;;
;; This feature has been merged into Emacs 23.  The code notices that and
;; does nothing there.  But there's no default value in
;; `ffap-rfc-directories' in emacs23, so if you were relying on
;; "/usr/share/doc/RFC/links" from the code here then add that back from
;; your .emacs etc.
;;
;; If you keep local RFCs as compressed .gz etc you can enable jka-compr
;; with `(auto-compression-mode 1)' in the usual way to read them.  ffap
;; will fallback on the `ffap-rfc-path' download if there's a compressed
;; file but jka-compr is not enabled.
;;
;; There's a lot of Emacs RFC download/cache/search packages.  Several are
;; listed at
;;
;;     http://www.emacswiki.org/cgi-bin/wiki/RFC
;;
;; ffap-rfc-directories.el is intentionally minimal, just saving you from
;; hitting ftp.rfc-editor.org every time.

;;; Emacsen:

;; Designed for Emacs 21 and 22, works in XEmacs 21.
;; Already builtin in Emacs 23 so not needed there.

;;; Install:

;; Put ffap-rfc-directories.el in one of your `load-path' directories and
;; the following in your .emacs
;;
;;     (eval-after-load "ffap" '(require 'ffap-rfc-directories))
;;
;; There's an autoload cookie below for this, if you're brave enough to use
;; `update-file-autoloads' and friends.

;;; History:
;; 
;; Version 1 - the first version
;; Version 2 - GPLv3
;; Version 3 - notice builtin in emacs23
;; Version 4 - eval-after-load in .emacs so no need for it here too
;; Version 5 - eval-when-compile for smaller .elc on emacs23
;; Version 6 - defcustom same as emacs23
;; Version 7 - undo defadvice on unload-feature
;; Version 8 - allow for advice unloaded before us too
;; Version 9 - new email

;;; Code:

;;;###autoload (eval-after-load "ffap" '(require 'ffap-rfc-directories))

(require 'ffap)

(unless (eval-when-compile
          (boundp 'ffap-rfc-directories)) ;; already builtin in emacs23

  ;; this is a defcustom the same as in emacs23
  ;; the corresponding ffap-rfc-path is only a defvar until 23.2 or some such
  (defcustom ffap-rfc-directories
    '("/usr/share/doc/RFC/links")
    "A list of directories to look for RFC files.
If a file is not in any of these directories then `ffap-rfc-path'
is offered."
    :type '(repeat directory)
    :group 'ffap)

  (defadvice ffap-rfc (around ffap-rfc-directories first (name) activate)
    "Look first for RFCs in `ffap-rfc-directories'."
    (or (setq ad-return-value
              (save-match-data ;; clobbered by ffap-locate-file
                (ffap-locate-file (format "rfc%s.txt" (match-string 1 name))
                                  t ;; nosuffix
                                  ffap-rfc-directories)))
        ad-do-it))

  (add-hook 'ffap-rfc-directories-unload-hook
            (lambda ()
              ;; ad-find-advice not autoloaded, so require 'advice in case
              ;; it was removed by `unload-feature'
              (require 'advice)
              (when (ad-find-advice 'ffap-rfc 'around 'ffap-rfc-directories)
                (ad-remove-advice   'ffap-rfc 'around 'ffap-rfc-directories)
                (ad-activate        'ffap-rfc)))))

;; LocalWords: usr gz compr http www emacswiki org el fallback

(provide 'ffap-rfc-directories)

;;; ffap-rfc-directories.el ends here
