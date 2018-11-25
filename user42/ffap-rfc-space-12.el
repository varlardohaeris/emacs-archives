;;; ffap-rfc-space.el --- recognise RFC with a space, like "RFC 1234"

;; Copyright 2007, 2008, 2009, 2010, 2015 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 12
;; Keywords: files, ffap, rfc
;; URL: http://user42.tuxfamily.org/ffap-rfc-space/index.html
;; EmacsWiki: FindFileAtPoint

;; ffap-rfc-space.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; ffap-rfc-space.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; M-x ffap doesn't recognise a space in an RFC like "RFC 1234", only say
;; "RFC-1234".  The entry in `ffap-alist' in Emacs 22 and earlier allows it,
;; but the guessing in `ffap-string-at-point' never matched it.  The
;; following spot of code extends both to do that, adding a (thing-at-point
;; 'rfc) as a bonus.
;;
;; Perhaps changing the `file' regexp in ffap-string-at-point-mode-alist
;; would be cleaner.  Or perhaps M-x ffap could tie in with thing-at-point a
;; bit more, and look for things that have "find" handlers.

;;; Install:

;; Put ffap-rfc-space.el in one of your `load-path' directories and the
;; following in your .emacs
;;
;;     (eval-after-load "ffap" '(require 'ffap-rfc-space))
;;
;; There's an autoload cookie for this if you install by
;; `M-x package-install' or know `update-file-autoloads' and friends.

;;; History:
;; 
;; Version 1 - the first version
;; Version 2 - GPLv3
;; Version 3 - don't drag in thingatpt.el until actually doing the ffap
;; Version 4 - set region for ffap-highlight
;; Version 5 - pattern with a space in preparation for emacs 23
;; Version 6 - eval-after-load in .emacs so no need for it here too
;; Version 7 - require word boundary
;; Version 8 - set ffap-string-at-point variable
;; Version 9 - undo defadvice on unload-feature
;; Version 10 - speedup for big buffers
;; Version 11 - new email
;; Version 12 - quieten the byte compiler on `thing-at-point-looking-at'

;;; Code:

;;;###autoload (eval-after-load "ffap" '(require 'ffap-rfc-space))

(require 'ffap)

;; Explicit dependency on advice.el since `ffap-rfc-space-unload-function'
;; needs `ad-find-advice' macro when running not byte compiled, and that
;; macro is not autoloaded.
(require 'advice)

;; Have `thing-at-point-looking-at' to quieten the byte compiler.
;; At runtime the `bounds-of-thing-at-point' handler is called by
;; thingatpt.el so it exists then.
(eval-when-compile
  (require 'thingatpt))

;;-----------------------------------------------------------------------------

;; emacs23 dropped the space from the rfc pattern, add it back with this
(add-to-list 'ffap-alist '("^[Rr][Ff][Cc] \\([0-9]+\\)" . ffap-rfc))

(put 'rfc 'bounds-of-thing-at-point
     (lambda ()
       ;; This regexp is the same as in `ffap-alist'.
       ;;
       ;; Narrowing to the current line is a speedup for big buffers.  It
       ;; limits the amount of searching forward and back that
       ;; thing-at-point-looking-at does when it works-around the way
       ;; re-search-backward doesn't match across point.
       ;;
       (and (save-restriction
              (narrow-to-region (line-beginning-position) (line-end-position))
              (thing-at-point-looking-at "\\b[Rr][Ff][Cc][- #]?\\([0-9]+\\)"))
            (cons (match-beginning 0) (match-end 0)))))

(defadvice ffap-string-at-point (around ffap-rfc-space activate)
  "Recognise RFCs with a space, like \"RFC 1234\"."
  (unless (let ((bounds (bounds-of-thing-at-point 'rfc)))
            (when bounds
              (setq ffap-string-at-point-region (list (car bounds)
                                                      (cdr bounds)))
              (setq ad-return-value
                    (setq ffap-string-at-point
                          (buffer-substring-no-properties (car bounds)
                                                          (cdr bounds))))))
    ad-do-it))

(defun ffap-rfc-space-unload-function ()
  "Remove defadvice from `ffap-string-at-point'.
This is called by `unload-feature'."
  (when (ad-find-advice 'ffap-string-at-point 'around 'ffap-rfc-space)
    (ad-remove-advice   'ffap-string-at-point 'around 'ffap-rfc-space)
    (ad-activate        'ffap-string-at-point))
  nil) ;; and do normal unload-feature actions too

(provide 'ffap-rfc-space)

;;; ffap-rfc-space.el ends here
