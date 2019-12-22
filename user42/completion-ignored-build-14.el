;;; completion-ignored-build.el --- some built completion-ignored-extensions

;; Copyright 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2017, 2019 Kevin Ryde
;;
;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 14
;; Keywords: convenience, compilation
;; URL: http://user42.tuxfamily.org/completion-ignored-build/index.html
;;
;; completion-ignored-build.el is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; completion-ignored-build.el is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; `M-x completion-ignored-build-mode' makes on-the-fly additions to
;; `completion-ignored-extensions' to ignore some built files.  See its
;; docstring for details.
;;
;; The idea is to give quicker access to source files.  For example typing
;; "Mak" Tab can complete to "Makefile.am", ignoring the "Makefile.in"
;; created by Automake, and ignoring a "Makefile" created by configure if
;; building in the source dir.
;;
;; The effect is best when an extension like say .c is sometimes a source
;; file and sometimes a generated file.  A foo.c file could be generated
;; from Perl foo.xs or yacc foo.y or lex foo.l etc.  You don't want to
;; ignore all .c files since a given directory might have a mix of manual
;; and generated C files.  `completion-ignored-build-mode' sets up to ignore
;; just the built ones.
;;
;; Currently there's no way to customize the built-file vs source-file rules
;; applied, but you can modify `completion-ignored-build-apply' without too
;; much trouble.  One reason not to customize yet is that pressing
;; `completion-ignored-extensions' into service for ignoring is not an ideal
;; mechanism.  What's really wanted is to ignore certain individual files,
;; not whole suffixes.  It works well enough in practice, but better ought
;; to be possible.
;;
;; Bugs:
;;
;; If a directory name is completed together with a filename (partial
;; completion) then the dynamic ignores are not applied because the
;; directory name is not yet expanded.  Pressing Tab again after the
;; directory has expanded should ignore the built files.
;;
;; If, however, at this second Tab, the minibuffer filename exists then it
;; is presented as a completion.  This can happen for an executable and a .c
;; source filename.  completion-ignored-build adds the bare exe to
;; `completion-ignored-extensions', but an exact match of the minibuffer is
;; not subject to those ignores (or believe so).

;;; Emacsen:
;;
;; Designed for Emacs 21 and higher, works in XEmacs 21.

;;; Install:
;;
;; Put completion-ignored-build.el in one of your `load-path' directories,
;; and to make `M-x completion-ignored-build-mode' available add to your
;; .emacs
;;
;;     (autoload 'completion-ignored-build-mode
;;               "completion-ignored-build" nil t)
;;
;; To enable the mode from .emacs the suggestion is the following which
;; loads and enables the first time you use the minibuffer
;;
;;     (autoload 'completion-ignored-build-enable-setup
;;               "completion-ignored-build")
;;     (add-hook 'minibuffer-setup-hook
;;               'completion-ignored-build-enable-setup)
;;
;; There's autoload cookies for the functions if you know how to use
;; M-x package-install or know `update-file-autoloads' and friends.  Then
;; just `M-x' or `add-hook' as desired.
;;
;; (In the past loading completion-ignored-build.el enabled the feature
;; immediately, but that's now not done because it clashed badly with an
;; initial autoloaded `M-x completion-ignored-build-mode' to mean toggle ON
;; -- and was very inconvenient if loading the code here for other
;; purposes.)

;;; History:
;;
;; Version 1 - the first version
;; Version 2 - read directory just one, tighter .c and .info ignoring
;; Version 3 - ignore Makefile.old from perl MakeMaker
;; Version 4 - ignore executables from .c and .cc
;; Version 5 - new completion-ignored-build-enable-setup
;;           - fixes for minibuffer dirname extraction
;; Version 6 - hash table for speed
;;           - remove leftover diagnostic `message's
;;           - undo defadvice on unload-feature
;; Version 7 - arrange as a minor mode
;;           - don't enable just from `require'
;; Version 8 - ignore .c from yacc .y or lex .l
;; Version 9 - ignore .eps from asymptote .asy
;; Version 10 - ignore executable from .cpp
;;            - treat .txi same as .texi
;; Version 11 - ignore .log .out from TeX
;; Version 12 - check executables are actually executable,
;;              to not ignore .gp when gp2c makes .gp.c
;; Version 13 - fix bug in filename read detection
;; Version 14 - more comments about partial completion


;;; Code:

;; Hard dependency on advice.el since it's used for the enabling and
;; disabling.  It also quietens the byte compiler a bit.
;;
;; `ad-find-advice' used in `completion-ignored-build-unload-function' is
;; not autoloaded, so if running uncompiled then don't want advice.el
;; unloaded before us.  It would work to put (require 'advice) in
;; `completion-ignored-build-unload-function', but here is safer and
;; clearer.
;;
(require 'advice)

(eval-when-compile
  (unless (fboundp 'ignore-errors)
    (require 'cl))) ;; for `ignore-errors'


;;-----------------------------------------------------------------------------
;; xemacs21 compatibility

(defun completion-ignored-build--completing-file-name-p ()
  "An internal part of completion-ignored-build.el.
Return non-nil if completing a filename in the minibuffer.

This is variable `minibuffer-completing-file-name' in Emacs, or a
fallback hack in XEmacs 21."

  (if (eval-when-compile (boundp 'minibuffer-completing-file-name))
      minibuffer-completing-file-name   ;; emacs21 up
    ;; xemacs21
    (eq minibuffer-completion-table 'read-directory-name-internal)))

;;-----------------------------------------------------------------------------
;; generic bits

(defun completion-ignored-build--file-executable-p (filename)
  "An internal part of completion-ignored-build.el.
Return true if FILENAME is an executable.
Currently this means it has an execute bit set (any of owner,
group, other) and not a directory.

This function differs from `file-executable-p' in that the file
can be executable by anyone, not just the current user."

  (and (not (file-directory-p filename))
       (string-match "x" (nth 8 (file-attributes filename)))))


;;-----------------------------------------------------------------------------

(defun completion-ignored-build-apply ()
  "An internal part of completion-ignored-build.el.
Make some additions to `completion-ignored-extensions'.
See `completion-ignored-build-enable' for how this works."

  ;; (field-string) gets minibuffer contents.  emacs21 minibuf.c
  ;; minibuffer_complete() uses `(field_string (point-max))', but emacs23
  ;; completion--do-completion uses effectively (field-string) at (point).
  ;; Don't think it should make a difference normally, but do the latter for
  ;; now.
  ;;
  ;; `substitute-in-file-name' is applied to expand any $FOO variables in
  ;; the string.  This ensures the correct dir to look at when the user
  ;; types "/abc/$FOO/def/Ma" etc.  In emacs21 `substitute-in-file-name' is
  ;; also necessary to pick out the operative part of doubled slashes like
  ;; "/foo//bar".  In emacs22 up the overlay and field mangling means
  ;; field-string is just the "/bar" part already.
  ;;
  (let ((minibuf (field-string)))
    (setq minibuf (substitute-in-file-name minibuf))

    (let* ((dir-name  (file-name-directory minibuf))
           (file-list (ignore-errors ;; no error if no such dir
                        (directory-files dir-name))) ;; relative filenames
           (table (make-hash-table :size (* 3 (length file-list))
                                   :test 'equal))
           (default-directory dir-name))
      (dolist (file file-list)
        (puthash file t table))  ;; `table' is the filenames, no directory


      ;; Generated-ness is a pairing like Makefile.am -> Makefile.in.
      ;; Exclusions can be done either
      ;;
      ;;   a) Loop across source files, act to excluded any built files they
      ;;      might create.
      ;;   b) Loop across built files, exclude them if corresponding source
      ;;      file exists.
      ;;
      ;; The current code is mostly (b) type looping across all files and
      ;; checking for a source.  The exception is .c .cc .cpp .C ->
      ;; executable which is done (a) style from the source files to try to
      ;; save some work in a big directory of undotted names such as
      ;; /usr/share/doc or a news spool dir etc.
      ;;
      ;; A single `directory-files' is done so all checks are in-memory.
      ;; A hash table is used to try to speedup multiple checks in a big
      ;; directory.  In a small directory just `member' would suffice, but
      ;; in that case the extra cost of hash table creation is small too.
      ;;
      ;; Repeated `file-exists-p' would make the system scan the directory
      ;; filenames in a similar way.  Chances are there'd be no noticeable
      ;; difference in speed except perhaps under a "remote" directory
      ;; file-handler thingie.

      ;;-----------------
      ;; individual special files

      ;; Makefile - variously generated
      (and (gethash "Makefile" table)
           (or (gethash "Makefile.am" table)  ;; automake
               (gethash "Makefile.in" table)  ;; generic autoconf
               (gethash "Makefile.PL" table)) ;; perl
           (add-to-list 'completion-ignored-extensions "Makefile"))

      ;; Makefile.in - generated by automake from Makefile.am
      (and (gethash "Makefile.in" table)
           (gethash "Makefile.am" table)
           (add-to-list 'completion-ignored-extensions "Makefile.in"))

      ;; Makefile.old -- generated by perl ExtUtils::MakeMaker as a backup
      (and (gethash "Makefile.old" table)
           (gethash "Makefile.PL"  table)
           (add-to-list 'completion-ignored-extensions "Makefile.old"))

      ;; configure -- generated by autoconf from configure.in or configure.ac
      (and (gethash "configure" table)
           (or (gethash "configure.in" table)
               (gethash "configure.ac" table))
           (add-to-list 'completion-ignored-extensions "configure"))

      ;; Build -- generated by perl Module::Build from Build.PL
      (and (gethash "Build"    table)
           (gethash "Build.PL" table)
           (add-to-list 'completion-ignored-extensions "Build"))

      ;;----------------
      ;; file extensions

      (dolist (file file-list)

        ;; foo-N, foo.info-N, foo-N.gz and foo.info-N.gz -- info sub-files
        ;; of base "foo", "foo.info", "foo.gz" or "foo.info.gz".
        ;;
        ;; Could allow other compression suffixes, or even all of
        ;; `Info-suffix-list', but think only .gz is ever used in practice
        ;; and don't particularly want to load info.el.
        ;;
        ;; A few files ending with a number "-N" aren't info files,
        ;; eg. ISO-8859-1 charmaps, a few ChangeLog parts and the like.  In
        ;; practice there normally either isn't also a base like "ISO-8859"
        ;; so this ignore doesn't apply, or the sub-parts aren't usually
        ;; wanted, or can be typed explicitly if wrongly ignored.
        ;;
        ;; ENHANCE-ME: If there's a foo.c or foo.cc per below then might
        ;; want to presume foo is an executable not an info master file and
        ;; on that basis not ignore foo-1 etc.  Is it worth the trouble?
        ;; How often foo.c -> foo and also a foo-1 sourcefile in the dir?
        ;;
        (and (string-match "-[0-9]+\\(\\.gz\\)\\'" file)
             (gethash (concat (substring file 0 (match-beginning 0))
                              (match-string 1 file))
                      table)
             (add-to-list 'completion-ignored-extensions file))

        ;; foo - executable from foo.c, foo.cc, foo.cpp or foo.C program
        ;; Demand >= 3 chars in the name, so say an x.c -> x doesn't ignore
        ;; everything ending "x".  Hope realistic executables have at least
        ;; 3 chars, and that 3 is enough not to match too many unrelated
        ;; suffixes.
        (and (string-match "\\.\\(C\\|c\\(\\|c\\\\|pp\\)\\)\\'" file)
             (>= (match-beginning 0) 3)
             (let ((built (substring file 0 (match-beginning 0))))
               (and (gethash built table)
                    (completion-ignored-build--file-executable-p built)
                    (add-to-list 'completion-ignored-extensions built))))

        ;; foo.c - variously generated
        (and (string-match "\\.c\\'" file)
             (let ((basename (substring file 0 (match-beginning 0))))
               (or (gethash (concat basename ".xs") table)   ;; perl xsubpp
                   (gethash (concat basename ".l")  table)   ;; lex (or flex)
                   (gethash (concat basename ".y")  table))) ;; yacc (or bison)
             (add-to-list 'completion-ignored-extensions file))

        ;; foo.h - generated by yacc (or bison) from foo.y
        (and (string-match "\\.h\\'" file)
             (gethash (concat (substring file 0 (match-beginning 0)) ".y")
                      table)
             (add-to-list 'completion-ignored-extensions file))

        ;; foo.info - generated by makeinfo from foo.texi
        (and (string-match "\\.info\\'" file)
             (let ((basename (substring file 0 (match-beginning 0))))
               ;; .txi same as auto-mode-alist has for texinfo-mode
               (or (gethash (concat basename ".texi") table)
                   (gethash (concat basename ".txi")  table)))
             (add-to-list 'completion-ignored-extensions file))

        ;; foo.eps - generated by asymptote from foo.asy
        ;; "asy -o" can write to a different filename, but foo.eps is usual
        ;; for `M-x asy-compile' which is C-c C-c in `asy-mode'.
        (and (string-match "\\.eps\\'" file)
             (gethash (concat (substring file 0 (match-beginning 0)) ".asy")
                      table)
             (add-to-list 'completion-ignored-extensions file))

        ;; foo.log foo.out - generated from TeX foo.tex or foo.TeX,
        ;; LaTeX foo.ltx, Texinfo foo.texi or foo.txi
        ;;
        ;; TeX generates various further .aux .cp .toc files too.  They're
        ;; in the default completion-ignored-extensions so ignored always.
        ;;
        (and (string-match "\\.\\(log\\|out\\)\\'" file)
             (let ((basename (substring file 0 (match-beginning 0))))
               ;; extensions per auto-mode-alist entries for tex-mode,
               ;; latex-mode, texinfo-mode
               (or (gethash (concat basename ".tex")  table)
                   (gethash (concat basename ".TeX")  table)
                   (gethash (concat basename ".ltx")  table)
                   (gethash (concat basename ".txi")  table)
                   (gethash (concat basename ".texi") table)))
             (add-to-list 'completion-ignored-extensions file))))))

;; In emacs22 `read-file-name' is C code and ignores advice set on
;; `file-name-completion' and `file-name-all-completions', so instead mangle
;; from the command funcs like `minibuffer-complete'.
;;
(defconst completion-ignored-build--advised-functions
  '(minibuffer-complete
    minibuffer-complete-word
    minibuffer-complete-and-exit)
  "An internal part of completion-ignored-build.el.
The list of functions (symbols) which have defadvice applied so
as to ignore built files.")

(dolist (func completion-ignored-build--advised-functions)
  (ad-add-advice
   func
   '(completion-ignored-build
     nil  ;; PROTECT
     nil  ;; not ENABLED
     (advice
      lambda nil
      "Add built files to `completion-ignored-extensions', dynamically."
      (if (completion-ignored-build--completing-file-name-p)
          (let ((completion-ignored-extensions completion-ignored-extensions))
            (completion-ignored-build-apply)
            ad-do-it)
        ad-do-it)))
   'around
   'first))

(defun completion-ignored-build-unload-function ()
  "Remove defadvice from `completion-ignored-build--advised-functions'.
This is called by `unload-feature'."
  (dolist (func completion-ignored-build--advised-functions)
    (when (ad-find-advice func 'around 'completion-ignored-build)
      (ad-remove-advice   func 'around 'completion-ignored-build)
      (ad-activate        func)))
  nil) ;; and do normal unload-feature actions too

;;-----------------------------------------------------------------------------

;;;###autoload
(define-minor-mode completion-ignored-build-mode
  "Ignore built files using `completion-ignored-extensions'.
When entering a filename in the minibuffer certain hairy setups
are applied to exclude built files for tab completion, so as to
give quick access to source files.

Currently the built file recognition is

* Ignore Makefile if there's any of Makefile.am (Automake),
  Makefile.in (raw configury), config.status (Autoconf build
  dir), or Makefile.PL (Perl ExtUtils::MakeMaker).

* Ignore Makefile.in if there's a Makefile.am (Automake).

* Ignore configure if there's a configure.in or
  configure.ac (Autoconf source dir).

* Ignore Build if there's a Build.PL (Perl Module::Build).

* Ignore foo.info if there's a foo.texi or foo.txi (Texinfo).

* Ignore foo.c if there's any of foo.l (lex), foo.y (yacc/bison)
  or foo.xs (Perl xsubs).

* Ignore foo.h if there's a foo.y (yacc).

* Ignore executable foo if there's a foo.c, foo.C, foo.cc or
  foo.cpp (C or C++ compiled program).

* Ignore foo-1, foo-2 etc if there's a foo, including ignore
  foo-1.gz if there's a foo.gz (Info sub-files).

* Ignore foo.log, foo.out, if there's any of foo.tex or
  foo.TeX (TeX), foo.ltx (LaTeX), foo.texi or foo.txi (Texinfo).

* Ignore foo.eps if there's foo.asy (Asymptote).

The ignoring is done with `completion-ignored-extensions', so
Tab (`minibuffer-complete') and friends still offer built files
if there's nothing else, and you still see all files in a `\\<minibuffer-local-filename-completion-map>\\[minibuffer-completion-help]'
help listing.

As usual for `completion-ignored-extensions' you can always type
in a full name explicitly if it's wrongly ignored.

--------
`completion-ignored-extensions' is a rather blunt instrument for
this idea.  If \"foo.c\" is ignored then other files like
barfoo.c ending foo.c are ignored too.  As long as you don't have
a mixture of similarly named generated and manual files then it's
usually fine.

The info foo-N sub-file ignoring is for info file foo without a
.info extension.  Nothing is done for foo.info-N files with .info
extension.  Their extension makes them unambiguous and they can
be ignored always by adding to `completion-ignored-extensions'.
It's necessary to put each of \".info-1\", \".info-2\",
\".info-1.gz\", \".info-2.gz\", etc there.  It's not desirable to
do the same to bare \"-1\" since \"foo-1\" can be various
charsets, debian package dirs, etc.

For Perl xsubs you might like to ignore \".bs\" link bootstrap
files too.  It can be put in `completion-ignored-extensions'
directly to ignore all \".bs\" files, no need to be dynamic.

--------
The completion-ignored-build.el home page is
URL `http://user42.tuxfamily.org/completion-ignored-build/index.html'"

  :type 'boolean
  (remove-hook 'minibuffer-setup-hook
               'completion-ignored-build-enable-setup)
  (let ((action (if completion-ignored-build-mode
                    'ad-enable-advice
                  'ad-disable-advice)))
    (dolist (func completion-ignored-build--advised-functions)
      (funcall action
               func 'around 'completion-ignored-build)
      (ad-activate func))))

;;;###autoload
(defun completion-ignored-build-enable ()
  "Enable `completion-ignored-build-mode'."
  (interactive)
  (completion-ignored-build-mode 1))

;;;###autoload
(defun completion-ignored-build-disable ()
  "Disable `completion-ignored-build-mode'."
  (interactive)
  (completion-ignored-build-mode 0))

;;;###autoload
(defun completion-ignored-build-enable-setup ()
  "Enable `completion-ignored-build-mode' initially.
This function is designed for use from `minibuffer-setup-hook'.

It enables `completion-ignored-build-mode' and then removes
itself from `minibuffer-setup-hook'.  The effect is to load and
enable the mode when you first use the minibuffer.  Waiting for
the first minibuffer rather than loading from .emacs can make
Emacs startup a little faster.

`completion-ignored-build-mode' always removes this setup from
`minibuffer-setup-hook' too, on the basis that if you called
`M-x completion-ignored-build-mode' explicitly then that should
trump any \"enable initially\" setup."
  (completion-ignored-build-enable))

;; No custom-add-option on `minibuffer-setup-hook' since as of Emacs 24.3
;; it's only a defvar, not a defcustom.  If it was a defcustom then an
;; option could be a good way for novice users to enable.
;;
;; ;;;###autoload
;; (custom-add-option 'minibuffer-setup-hook
;;                    'completion-ignored-build-enable-setup)

;; LocalWords: docstring Mak xs customization minibuffer Makefile automake
;; LocalWords: dir filename startup foo yacc lex el fallback

(provide 'completion-ignored-build)

;;; completion-ignored-build.el ends here
