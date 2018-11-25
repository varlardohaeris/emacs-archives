;;; xml-coding.el --- coding system from tag in xml files (for Emacs 21)

;; Copyright 2006, 2007, 2009, 2010, 2015 Kevin Ryde
;;
;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 9
;; Keywords: i18n, xml
;; URL: http://user42.tuxfamily.org/xml-coding/index.html
;; EmacsWiki: CategoryXML

;; xml-coding.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; xml-coding.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This is a spot of code for Emacs 21 to get the coding system from a
;; <?xml> tag when visiting a .xml file.  Emacs 22 has this feature already,
;; so the code does nothing there.

;;; Install:

;; Put xml-coding.el in one of your `load-path' directories, and in your
;; .emacs add
;;
;;     (unless (fboundp 'sgml-xml-auto-coding-function)
;;       (require 'xml-coding))
;;
;; There's an autoload cookie for this below, if you use
;; `update-file-autoloads' and friends.

;;; History:

;; Version 1 - the first version.
;; Version 2 - suggest fboundp test within .emacs
;; Version 3 - eval-when-compile in the cookie, helping bytecomp'ed loaddefs
;; Version 4 - just \\s- instead of [:space:], for xemacs
;; Version 5 - fix defadvice ad-return-value test
;; Version 6 - undo defadvice on unload-feature
;; Version 7 - also zap xml-auto-coding-function on unload
;; Version 8 - allow for advice unloaded before us too
;; Version 9 - new email

;;; Code:

;;;###autoload (unless (eval-when-compile (fboundp 'sgml-xml-auto-coding-function)) (require 'xml-coding))

;; already done in emacs 22
(unless (fboundp 'sgml-xml-auto-coding-function)

  (defun xml-auto-coding-function (size)
    "Return the coding system for an xml file, based on the <?xml> tag.
This function is designed for use under `set-auto-coding' of
Emacs 21.  (Emacs 22 has this feature already in
`sgml-xml-auto-coding-function'.)

At point there should be SIZE many bytes from an
`insert-file-contents-literally' of the first part of a file.  If
it has an \"<?xml\" marker with an \"encoding\" parameter then
return an Emacs coding system, otherwise return nil.

The xml-coding.el home page is
URL `http://user42.tuxfamily.org/xml-coding/index.html'"

    (save-restriction
      ;; only look at SIZE being inserted
      (narrow-to-region (point) (+ (point) size))

      ;; According to http://www.w3.org/TR/REC-xml/#sec-prolog-dtd there
      ;; shouldn't be a <!-- comment before the <?xml.  Something like
      ;; "\\(\\s-*<!--\\(.\\|\n\\)*?--\\s-*>\\)*" could be added if
      ;; that was needed.
      ;;
      (and (looking-at "\\s-*<\\?xml\\s-[^>]*encoding=[\"']\\([^\"'>]+\\)")

           ;; According to http://www.w3.org/TR/REC-xml/#NT-EncodingDecl the
           ;; charset is supposed to be an iana registered name, as per
           ;; http://www.iana.org/assignments/character-sets.  Dunno how
           ;; much that means in practice, let mm-util try to figure it out.
           ;; Most are utf-8 which will certainly work.
           ;;
           (let ((charset (match-string 1)))
             (or (progn
                   (eval-and-compile ;; quieten byte compiler
                     (require 'mm-util))
                   (let ((coding (mm-charset-to-coding-system charset)))

                     ;; `mm-charset-to-coding-system' returns `ascii' for
                     ;; "US-ASCII", but that's not actually a coding system.
                     ;; Gnus copes with that in various places (usually
                     ;; treating ascii as meaning no conversion), go
                     ;; undecided here.
                     (if (and (eq coding 'ascii)
                              (not (coding-system-p coding)))
                         (setq coding 'undecided))

                     coding))
                 (progn
                   ;; note no `display-warning' in emacs21, though
                   ;; unfortunately a plain `message' tends to be
                   ;; overwritten in many cases
                   (message "Unrecognised XML charset: %s" charset)
                   nil))))))

  (defadvice set-auto-coding (around xml-coding activate)
    "Find the coding system for an XML file based on its <?xml> tag.
See `xml-auto-coding-function' for details."

    ;; `set-auto-coding' moves point to the end of the buffer.  Don't use
    ;; save-excursion in case something depends on it working that way, but do
    ;; remember where we're supposed to start looking from.
    ;;
    (let ((xml-coding--saved-point (point)))
      ad-do-it
      (unless ad-return-value
        (save-excursion
          (goto-char xml-coding--saved-point)
          (setq ad-return-value (xml-auto-coding-function size))))))

  (add-hook 'xml-coding-unload-hook
            (lambda ()
              ;; ad-find-advice not autoloaded, so require 'advice in case
              ;; it was removed by `unload-feature'
              (require 'advice)
              (when (ad-find-advice 'set-auto-coding 'around 'xml-coding)
                (ad-remove-advice   'set-auto-coding 'around 'xml-coding)
                (ad-activate        'set-auto-coding))
              ;; normal unload-feature actions don't run when an unload-hook
              (fmakunbound 'xml-auto-coding-function))))

;; LocalWords: xml

(provide 'xml-coding)

;;; xml-coding.el ends here
