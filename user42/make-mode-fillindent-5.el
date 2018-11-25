;;; make-mode-fillindent.el --- filling indented makefile comments

;; Copyright 2008, 2009, 2010, 2015 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 5
;; Keywords: files, make
;; URL: http://user42.tuxfamily.org/make-mode-fillindent/index.html
;; EmacsWiki: MakefileMode

;; make-mode-fillindent.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; make-mode-fillindent.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This spot of code helps makefile-mode in Emacs 21 and 22 have
;; fill-paragraph (M-q) work on indented comments like
;;
;;	foo:
;;		# this is
;;		# some comment
;;		echo hi
;;
;; `makefile-mode' in Emacs 21 and 22 has a special makefile-fill-paragraph
;; for filling.  It recognises "#" comments at the start of a line, but does
;; nothing on indented comments.  Doing nothing is particularly
;; disconcerting if you use filladapt.el, because filladapt-debug shows a
;; correct prefix+paragraph analysis yet M-q has no effect.
;;
;; Whether indented comments is a good idea is another matter.  "make" runs
;; them with the shell, and it ignores them.  The happy side-effect though
;; is to get an echo from make, so you see the comments as the rule
;; proceeds, whereas unindented makefile comments are consumed by make.
;;
;; Paragraph identification in Emacs 21 and XEmacs 21 makefile-mode isn't
;; really setup for indented comments, and no attempt is made to do anything
;; about that here.  The suggestion is to use filladapt which gets it right
;; (or is easier to configure if it doesn't).
;;
;; If you find you don't like what this code does then to undo try
;;
;;     M-x unload-feature
;;     make-mode-fillindent
;;

;;; Emacsen:

;; Designed for Emacs 21 and 22.  Not needed and does nothing in Emacs 23.

;;; Install:

;; Put make-mode-fillindent.el in one of your `load-path' directories and in
;; your .emacs add
;;
;;     (eval-after-load "make-mode" '(require 'make-mode-fillindent))
;;
;; There's an autoload cookie below for this, if you're brave enough to use
;; `update-file-autoloads' and friends.

;;; History:
;;
;; Version 1 - the first version
;; Version 2 - eval-when-compile to exclude in emacs23 bytecomp
;; Version 3 - undo defadvice on unload-feature
;; Version 4 - allow for advice unloaded before us too
;; Version 5 - new email


;;; Code:

;;;###autoload (eval-after-load "make-mode" '(require 'make-mode-fillindent))

(require 'make-mode)

(when (eval-when-compile
        (with-temp-buffer
          (insert "\t# foo\n\t# bar\n")
          (goto-char (point-min))
          ;; if makefile-fill-paragraph claims to have acted, but leaves two
          ;; lines instead of one, then it's done the wrong thing and should
          ;; apply defadvice
          (and (makefile-fill-paragraph nil)
               (= 2 (count-lines (point-min) (point-max))))))

  (defadvice makefile-fill-paragraph (around make-mode-fillindent activate)
    "Let indented comments go to normal filling.
Unindented comments work fine, this patch lets them work indented too."
    (if (save-excursion
          (beginning-of-line)
          (looking-at "^[ \t]+#+\\s-*"))

        ;; Use ordinary filling.
        ;;
        ;; The usual way is to return nil to ask for ordinary filling, but
        ;; the following is what makefile-fill-paragraph does, so for
        ;; consistency we'll do the same.  Both ways seem fine with
        ;; filladapt.el, perhaps there's a difference with adaptive fill
        ;; or something.  (Emacs pre-23 has taken to returning nil, but
        ;; maybe it can rely on improved comment detection and whatnot.)
        ;;
        (let ((fill-prefix (buffer-substring-no-properties
                            (match-beginning 0) (match-end 0)))
              (fill-paragraph-function nil))
          (fill-paragraph nil)
          (setq ad-return-value t))

      ;; rest of makefile-fill-paragraph
      ad-do-it))

  (add-hook 'make-mode-fillindent-unload-hook
            (lambda ()
              ;; ad-find-advice not autoloaded, so require 'advice in case
              ;; it was removed by `unload-feature'
              (require 'advice)
              (when (ad-find-advice 'makefile-fill-paragraph 'around 'make-mode-fillindent)
                (ad-remove-advice   'makefile-fill-paragraph 'around 'make-mode-fillindent)
                (ad-activate        'makefile-fill-paragraph)))))

;;------------------------------------------------------------------------------

;; LocalWords: filladapt el unindented makefile fillindent foo

(provide 'make-mode-fillindent)

;;; make-mode-fillindent.el ends here
