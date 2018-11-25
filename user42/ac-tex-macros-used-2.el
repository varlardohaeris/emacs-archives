;;; ac-tex-macros-used.el --- auto-complete for tex-mode \foo macros used

;; Copyright 2016, 2017 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 2
;; Keywords: wp, TeX, LaTeX, auto-complete
;; URL: http://user42.tuxfamily.org/ac-tex-macros-used/index.html

;; ac-tex-macros-used.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; ac-tex-macros-used.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; This is an auto-complete.el add-on with a completion source for TeX macro
;; names which exist in the current buffer.
;;
;; TeX and add-on packages have a lot of macros.  It's likely only
;; relatively few will be wanted in a given document.  The completion here
;; offers those which appear already in the current buffer.  This is simple,
;; and quite effective as far as it goes.

;;; Install:
;;
;; Put ac-tex-macros-used.el in one of your `load-path' directories and put
;; `ac-source-tex-macros-used' into `ac-sources' with something like
;;
;;     (add-hook 'tex-mode-hook
;;               (lambda ()
;;                 (require 'ac-tex-macros-used)
;;                 (make-local-variable 'ac-sources)
;;                 (add-to-list 'ac-sources 'ac-source-tex-macros-used)))
;;
;; `ac-source-tex-macros-used' is rather specific to TeX backslashing and so
;; generally wouldn't be wanted globally.
;;
;; auto-complete.el (as of its version 1.3.1) does not enable itself in
;; `tex-mode' by default.  M-x auto-complete-mode can start it to try.
;; To have it permanently, add to `ac-modes' as described in the
;; auto-complete manual.txt under "enable auto-complete-mode automatically
;; for specific modes".

;;; History:
;;
;; Version 1 - the first version
;; Version 2 - ac-tex-macros-ignore

;;; Code:

;; auto-complete:
;; /usr/share/doc/auto-complete-el/doc/manual.txt
;; /usr/share/emacs/site-lisp/auto-complete/auto-complete.el

(eval-when-compile
  (unless (fboundp 'push)
    (require 'cl))) ;; for push macro

;; autoload for the benefit of safe-local-variable
;;;###autoload
(defun ac-tex-macros-list-of-strings-p (obj)
  "Return non-nil if OBJ is a list of strings."
  (and (listp obj)
       (not (memq nil (mapcar 'stringp obj)))))

(defcustom ac-tex-macros-ignore
  '(
    ;; LaTeX \documentclass{foo} normally appears once only in a document,
    ;; don't want it as a completion candidate
    "documentclass"

    ;; article.sty macros normally appearing once in the header
    "author" "date")

  "List of macro names (strings) ignored for completion.
This is experimental, but intended as macros which usually occur
only once in a document so are not of interest for subsequent
completion.

The current value is some LaTeX macros used once in a document
header (though possibly depending on the class), for example
\"documentclass\".  The intention would be some more of the most
common macros of this kind."

  :type  '(repeat string)
  :group 'auto-complete)  ;; no separate group for ac-tex-macros.el yet
;;;###autoload
(put 'ac-tex-macros-ignore
     'safe-local-variable 'ac-tex-macros-list-of-strings-p)

(defun ac-tex-macros-used-list ()
  "An internal part of ac-tex-macros-used.el.
Return a list of all macro names \"\\=\\foo\" used in the current
buffer.

A macro name at point is not included in the return, since that
will be an incomplete name which auto-complete is about to offer
to extend.

Any macros in % comments are ignored.  Currently any
\\=\\begin{comment} or \\=\\begin{verbatim} is not ignored, but
maybe it ought to be."

  (let ((orig-point (point))
        ret)
    (save-excursion
      (goto-char (point-min))

      ;; match any %, or >=1 backslashes then alpha or %
      ;; 1 = backslashes
      ;; 3 = macro name (without backslashes)
      (while (re-search-forward "%\\|\\(\\\\+\\)\\(%\\|\\([a-zA-Z]+\\)\\)"
                                nil t)
        (cond ((not (match-beginning 1))
               ;; % comment, no backslashes
               (end-of-line))

              ((= 0 (mod (- (match-end 1) (match-beginning 1)) 2))
               ;; even backslashes
               (if (not (match-beginning 3))
                   ;; % comment, even backslashes
                   (end-of-line)))

              ;; odd backslashes
              ((match-beginning 3)
               ;; \foo macro name
               ;; not the macro currently being completed
               (unless (and (>= orig-point (match-beginning 3))
                            (<= orig-point (match-end 3)))
                 (let ((str (buffer-substring (1- (match-beginning 3))
                                              (match-end 3))))
                   ;; not duplicate or to be ignored
                   (unless (or (member str ret)
                               (member (match-string 3) ac-tex-macros-ignore))
                     (push str ret))))))))

    (nreverse ret)))

(defun ac-tex-macros-used-beginning-of-macro-position ()
  "An internal part of ac-tex-macros-used.el.
Return the position of the start of a macro name at point.

The return is the location of the backslash, or nil if point is
not in a macro name.

    \\=\\foo
    ^---return value

At an odd number of backslashes the last introduces the macro and
is returned.  At an even number of backslashes the return is nil
since that does not introduce a macro."

  (save-excursion
    (skip-chars-backward "a-zA-Z")
    (let ((p (point)))
      (skip-chars-backward "\\\\")
      (and (= 1 (mod (- p (point)) 2))  ;; if an odd number of backslashes
           (1- p)))))

(defvar ac-source-tex-macros-used
  '((candidates . ac-tex-macros-used-list)
    (prefix     . ac-tex-macros-used-beginning-of-macro-position)
    (requires   . 3))
  "auto-complete.el source for TeX macro names used in the buffer.
This source completes the names of \\=\\foo macros which appear
in the buffer.

TeX and add-on packages have a lot of macros and only relatively
few will be of interest in a given document.  The idea here is
those already used are likely to be used again.

The \"requires\" field is 3 so completion is offered after two
letters \\=\\xx.  This is designed to offer completions quickly.

----
The auto-complete.el home page is
URL `http://cx4a.org/software/auto-complete/'

The ac-tex-macros-used.el home page is
URL `http://user42.tuxfamily.org/ac-tex-macros-used/index.html'")


;;-----------------------------------------------------------------------------

;;  LocalWords:  documentclass foo

(provide 'ac-tex-macros-used)

;;; ac-tex-macros-used.el ends here
