;;; compile-command-default.el --- establish a default for M-x compile

;; Copyright 2008, 2009, 2010, 2012, 2015 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 14
;; Keywords: processes, compilation
;; URL: http://user42.tuxfamily.org/compile-command-default/index.html
;; EmacsWiki: CompilationMode

;; compile-command-default.el is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; compile-command-default.el is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This spot of code helps establish a default `compile-command' for
;; M-x compile etc in particular files.
;;
;; Each function in `compile-command-default-functions' can contemplate the
;; buffer filename, directory, contents, etc, and decide a compile-command
;; which applies.
;;
;; The only included functions as yet setup to run Perl, either just on a
;; script, in a working directory for development, or a test script directly
;; or through the usual MakeMaker test harness.
;;
;; The operative part is really just hacking `hack-local-variables' for a
;; place to establish a default.  The list of functions allows wild tests
;; for what to apply when and to construct a command perhaps with absolute
;; directories etc.
;;
;; See mode-compile.el for a bigger system geared more towards language
;; compiles like gcc etc.
;;
;; Bugs:
;;
;; When a file is renamed with `dired-do-rename' a buffer visiting it
;; follows to the new name but the `compile-command' is not recalculated.
;; This is no good since the old name is left in that string.  Maybe the
;; `compile-command-default-functions' should be re-run in that case, though
;; maybe only if the compile-command was calculated by
;; compile-command-default and hasn't been edited manually later.  Or if
;; always overridden then an edited command would still be in the
;; `compile-history' list.

;;; Emacsen:

;; Designed for Emacs 21 and up.  Works in XEmacs 21 and Emacs 20.

;;; Install:

;; Add to your .emacs
;;
;;     (require 'compile-command-default)
;;
;; By default it does nothing, you add the functions you like the sound of
;; `compile-command-default-functions'.  For example
;;
;;     (setq compile-command-default-functions
;;           '(compile-command-default-perl-pl))
;;
;; or
;;
;;     (add-hook 'compile-command-default-functions
;;               'compile-command-default-perl-pl)

;;; History:

;; Version 1 - the first version
;; Version 2 - allow .t files in subdirectories of /t
;; Version 3 - .t files under /devel too
;; Version 4 - show "Entering directory" with cd's
;;           - run .pl.gz too
;; Version 5 - undo defadvice on unload-feature
;; Version 6 - add -w to raw perl .t
;;           - clean compile-command-default-functions on other package unload
;; Version 7 - express dependency on 'advice
;; Version 8 - use Filter::gunzip or PerlIO::gzip for .pl.gz if available
;; Version 9 - 'cl for emacs20
;; Version 10 - new compile-command-default-perl-interpreter for #!
;;            - t-raw on /examples/ only when under a dist working dir
;; Version 11 - perl xt/ directory as well as t/
;; Version 12 - perl sandbox/ directory as well as devel/
;; Version 13 - cl macros only when needed
;; Version 14 - use && for combination cd and command


;;; Code:

;; for `ad-find-advice' macro when running uncompiled
;; (don't unload 'advice before our -unload-function)
(require 'advice)

(eval-when-compile
  (unless (fboundp 'push)
    (require 'cl))) ;; for macros in emacs20

;; quieten the byte compiler when no auto-mode-interpreter-regexp
(defvar auto-mode-interpreter-regexp)

;;-----------------------------------------------------------------------------

;;;###autoload
(defgroup compile-command-default nil "Compile-Command-Default"
  :prefix "compile-command-default-"
  :group 'compilation
  :link  '(url-link
           :tag "compile-command-default.el home page"
           "http://user42.tuxfamily.org/compile-command-default/index.html"))

(defcustom compile-command-default-functions nil
  "Functions calculating default `compile-command' values."
  :type 'hook
  :group 'compile-command-default)
;; ask `unload-feature' to purge `compile-command-default-functions' when a
;; package func in it is unloaded
(eval-after-load "loadhist"
  '(when (boundp 'unload-feature-special-hooks)
     (add-to-list 'unload-feature-special-hooks
                  'compile-command-default-functions)))
;; automatically `risky-local-variable-p' due to name `-functions'

;; This is a defadvice so it's re-run by M-x normal-mode.  An entry in
;; `find-file-hook' for instance doesn't get that.
;;
(defadvice hack-local-variables (after compile-command-default activate)
  "Possibly set a default `compile-command'."
  (and (not (ad-get-arg 0))  ;; not when doing a "mode-only" local vars crunch
       buffer-file-name      ;; only for file buffers

       ;; leave alone any explicit local variable value in the file;
       ;; must give buffer parameter explicitly for xemacs
       (not (local-variable-p 'compile-command (current-buffer)))

       (let ((result (run-hook-with-args-until-success
                      'compile-command-default-functions)))
         (if result
             (set (make-local-variable 'compile-command) result)))))

;; `-unload-function' only runs in emacs22 up, but that's fine since the
;; advice is harmless when everything else has unloaded, because
;; `run-hook-with-args-until-success' just returns nil if the given hook
;; variable `compile-command-default-functions' has been unbound.
;;
(defun compile-command-default-unload-function ()
  "Remove defadvice from `hack-local-variables'.
This is called by `unload-feature'."
  (when (ad-find-advice 'hack-local-variables 'after 'compile-command-default)
    (ad-remove-advice   'hack-local-variables 'after 'compile-command-default)
    (ad-activate        'hack-local-variables))
  nil) ;; and do normal unload-feature actions too


;;-----------------------------------------------------------------------------
;; #! interpreter recognition

(defun compile-command-default--interpreter ()
  "Return the #! interpreter at the start of the current buffer.
This is an internal part of compile-command-default.el.
For example \"#!/usr/bin/env /bin/bash -e\" in the buffer gives
\"/bin/bash -e\".  If there's no #! line then return nil.

The #! interpreter is recognised with `auto-mode-interpreter-regexp'.
XEmacs 21.4 doesn't have that variable but if you set it then
it's used, otherwise there's a built-in fallback."

  ;; cf linux kernel fs/binfmt_script.c -- it allows multiple tabs or spaces
  ;; after the #!, unlike emacs23 which only allows one

  (save-excursion
    (goto-char (point-min))
    ;; auto-mode-interpreter-regexp existence locked down at compile time,
    ;; otherwise look again at runtime, otherwise the fallback
    (and (looking-at (if (or (eval-when-compile
                               (boundp 'auto-mode-interpreter-regexp))
                             (boundp 'auto-mode-interpreter-regexp))
                         auto-mode-interpreter-regexp
                       ;; adapted from emacs23
                       "#![ \t]*\\([^ \t\n]*/bin/env[ \t]+\\)?\\([^ \t\n]+\\)"))
         (buffer-substring-no-properties (match-beginning 2)
                                         (line-end-position)))))


;;-----------------------------------------------------------------------------
;; perl module availability

(defvar compile-command-default--have-perl-module-alist nil
  "Cached results of `compile-command-default--have-perl-module-p'.
This is an internal part of compile-command-default.el.

Entries like (\"Foo::Bar\" . t) or (\"No::Such\" . nil) according
to availability of the respective module.")

(defun compile-command-default--have-perl-module-p (module)
  "Return non-nil if Perl module MODULE is available.
This is an internal part of `compile-command-default-perl-pl'.
MODULE is a string like \"Foo::Bar\".  The code is not robust
against bad module names."
  (cdr (or (assoc module compile-command-default--have-perl-module-alist)
           (car (push (cons module
                            (eq 0 (call-process
                                   "perl"
                                   nil ;; input /dev/null
                                   nil ;; output /dev/null
                                   nil ;; no redisplay
                                   "-e" (concat "require " module))))
                      compile-command-default--have-perl-module-alist)))))

;;-----------------------------------------------------------------------------
;; perl .pl, .pl.gz, #!/usr/bin/perl
  
(defun compile-command-default-perl-command (&optional perl-command)
  "Return a `compile-command' to run perl on the current buffer file.
This is an internal part of `compile-command-default-perl-pl'.

PERL-COMMAND defaults to \"perl\".  It can be \"perl -w\" or
similar when options are included in a #!."

  ;; Filter distribution 1.37 has a Filter::Decompress, but only as an
  ;; example, not installed, and only reading the Zlib RFC1950 format, not
  ;; gzip, so it doesn't suit for the .gz here.
  ;;
  ;; PerlIO::via::gzip version 0.021 doesn't seem to work.

  (unless perl-command
    (setq perl-command "perl"))

  (cond ((let ((case-fold-search nil))
           (not (string-match "\\.gz\\'" buffer-file-name)))
         ;; perl /some/dir/foo.pl
         (concat perl-command " " (shell-quote-argument buffer-file-name)))

        ((compile-command-default--have-perl-module-p "Filter::gunzip")
         ;; perl -MFilter::gunzip /some/dir/foo.pl.gz
         (concat perl-command " -MFilter::gunzip "
                 (shell-quote-argument buffer-file-name)))

        ((and (compile-command-default--have-perl-module-p "PerlIO::gzip")
              (compile-command-default--have-perl-module-p "open"))
         ;; perl -e '{use open IN=>\":gzip\";require shift}' /dir/foo.pl.gz
         (concat perl-command " -e '{use open IN=>\":gzip\";require shift}' "
                 (shell-quote-argument buffer-file-name)))

        ((compile-command-default--have-perl-module-p "Filter::exec")
         ;; perl -MFilter::exec=zcat /some/dir/foo.pl.gz
         (concat perl-command " -MFilter::exec=zcat "
                 (shell-quote-argument buffer-file-name)))

        (t
         ;; zcat /some/dir/foo.pl.gz | perl -
         (concat "zcat " (shell-quote-argument buffer-file-name)
                 " | " perl-command " -"))))

(defun compile-command-default-perl-pl ()
  "Set `compile-command' to run perl on a .pl or .pl.gz file.
This is designed for use in `compile-command-default-functions'.
The command generated is

    perl /top/dir/foo.pl

or for .pl.gz one of the following, according to what modules are
available (checked on first visiting a .pl.gz),

    perl -MFilter::gunzip /some/dir/foo.pl.gz
    perl -e '{use open IN=>\":gzip\";require shift}' /dir/foo.pl.gz
    perl -MFilter::exec=zcat /some/dir/foo.pl.gz
    zcat /some/dir/foo.pl.gz | perl -

This .pl.gz is handy on example programs which are compressed
under /usr/share/doc etc.

Filter::gunzip or the lexical open :gzip are preferred since
they're a bit better on __DATA__ sections than Filter::exec.  The
zcat to \"-\" stdin form doesn't need any modules but it doesn't
get the filename in error messages the way the others do.

The script filename is absolute so the command can be re-run from
a different directory (a buffer with a different current
directory) if you follow an error to a module, etc.

See `compile-command-default-perl-t-raw' for running development
or example programs with build and working directories included.

The Filter::gunzip home page is
URL `http://user42.tuxfamily.org/filter-gunzip/index.html'.
PerlIO::gzip and Filter::exec are available from CPAN."

  (and (let ((case-fold-search nil)) ;; don't act on Makefile.PL
         (string-match "\\.pl\\(\\.gz\\)?\\'" buffer-file-name))
       (compile-command-default-perl-command)))

(defun compile-command-default-perl-interpreter ()
  "Set `compile-command' to run perl on a #!/usr/bin/perl file.
This is designed for use in `compile-command-default-functions'.
A file starting with a #! like

    #!/usr/bin/perl
    #!/usr/local/bin/perl
    #!perl -wT

is considered a perl script and a command to run it is generated
as per `compile-command-default-perl-pl'.  The #!perl form is
found in scripts to be processed by ExtUtils::MakeMaker (becomes
the configured perl executable).

The particular path like /usr/local/bin is ignored, but options
like -wT are included in the command.  (Perl recognises -w itself
when it runs, but a -T must be on the command line.)

A plain executable script could be run just as ./foo, but picking
out perl explicitly is helpful on .gz files or example files
which don't have execute bits set (per chmod).

The #! line is recognised with `auto-mode-interpreter-regexp', or
with a fallback if that variable is not available (Emacs 20 or
XEmacs 21).

If you use #! lines in .t test script files then arrange
`compile-command-default-perl-t-harness' (or t-raw) ahead of this
interpreter check, so as to prefer the .t working directory
setups."

  (let ((interpreter (compile-command-default--interpreter)))
    (and interpreter
         (string-match "\\`[^ \t]*perl[^/ \t]*\\([ \t]\\|\\'\\)"
                       interpreter)
         (compile-command-default-perl-command
          (concat "perl" (substring interpreter (match-beginning 1)))))))


(custom-add-option 'compile-command-default-functions
                   'compile-command-default-perl-pl)
(custom-add-option 'compile-command-default-functions
                   'compile-command-default-perl-interpreter)

;;-----------------------------------------------------------------------------
;; perl .t files

(defun compile-command-default-perl-t-harness ()
  "Set `compile-command' to run a perl test harness on a t/*.t file.
This is designed for use in `compile-command-default-functions'.

The command is the same sort of thing \"make test\" from
ExtUtils::MakeMaker will run.  For example

    cd /top/dir && \\=\\
    PERL_DL_NONLAZY=1 perl -MExtUtils::Command::MM \\=\\
      -e \"test_harness(0,'blib/lib','blib/arch')\" t/foo.t

It includes a \"cd\" to the top level directory where a normal
\"make test\" runs.  An absolute path there means you can re-run
from elsewhere if you follow an error to a different file.

Test files in subdirectory like t/author/bar.t are run similarly,
and an xt/ directory too."

  (and (let ((case-fold-search nil)) ;; not .T
         (string-match "/\\(x?t/.*\\.t\\)\\'" buffer-file-name))
       (let ((topdir   (substring buffer-file-name 0 (match-beginning 1)))
             (filename (substring buffer-file-name (match-beginning 1))))
         (concat "cd " (shell-quote-argument topdir) " && \\\n"
                 "echo \"Entering directory \\``pwd`'\" && \\\n"
                 "PERL_DL_NONLAZY=1 perl -MExtUtils::Command::MM \\\n"
                 "  -e \"test_harness(0,'blib/lib','blib/arch')\" \\\n"
                 "  " (shell-quote-argument filename)))))

(custom-add-option 'compile-command-default-functions
                   'compile-command-default-perl-t-harness)

(defun compile-command-default-perl-t-raw ()
  "Set `compile-command' to run a perl development or raw test script.
This is designed for use in `compile-command-default-functions'.
It acts on files

    t/*.t            # test scripts
    xt/*.t           # author tests
    devel/*.pl       # development scripts
    sandbox/*.pl
    devel/*.t        # development tests
    sandbox/*.t

The command is like

    cd /home/foo/proj/ && \\=\\
    echo \"Entering directory \\=\\``pwd`'\" && \\=\\
    perl -w -I /home/foo/proj/lib \\=\\
         -I /home/foo/proj/blib/lib \\=\\
         -I /home/foo/proj/blib/arch \\=\\
         t/foo.t

\"cd\" means it runs from the toplevel the same as a \"make
test\" does on a .t file, and it can be restarted from elsewhere
if you follow an error to a different directory.  \"Entering
directory\" tells `compilation-mode' where it's running, to help
relative paths (see `compilation-directory-matcher').

\"-w\" is added to .t files because that's how \"make test\" will
run them.  It's not added for .pl files since there it's best in
the \"#!/usr/bin/perl -w\" line or \"use warnings\" in the usual
way.

\"-I\" paths get latest work-in-progress code from \"lib\", and
\"blib\" for the last \"make\".  \"blib\" is needed if you keep
.xs code in the toplevel instead of under the \"lib\" tree.  The
paths are absolute in case the program does a chdir() elsewhere.

For .t files this is a raw run and you see all the output,
without the the usual Test::Harness crunching.  See
`compile-command-default-perl-t-harness' for a harnessed command.

For reference, the \"-Mblib\" blib.pm module is not used for the
blib directory because it dies if there's no such directory,
which can happen if you're trying some all-perl \"lib\" code
without yet having run Makefile.PL."

  ;; on MacOS blib.pm has "blib/$MacPerl::Architecture" instead of
  ;; "blib/arch", dunno if there's any merit trying to do the same here
  ;; (with a "-e" or something)
  ;;
  ;; FindBin::libs has some novel automated pickup of /lib dirs at or above
  ;; a script's location.  Not sure if you'd want that all the time though.
  ;;
  (and (let ((case-fold-search nil)) ;; not .T
         (or (string-match
              ;; .t file anywhere under /t/, /xt/, /devel/ or /sandbox/,
              ;; .pl under /devel/ or /sandbox/
              "/\\(\\(x?t\\|devel\\|sandbox\\)/.*\\.t\\|\\(devel\\|sandbox\\)/.*\\.pl\\)\\'"
              buffer-file-name)
             ;; .pl under /examples/, if a lib or blib dir suggests it's a
             ;; working directory
             (and (string-match "/\\(examples/.*\\.pl\\)\\'" buffer-file-name)
                  (or (file-exists-p "../lib")
                      (file-exists-p "../blib")))))
       (let* ((topdir   (substring buffer-file-name 0 (match-beginning 1)))
              (filename (substring buffer-file-name (match-beginning 1)))
              ;; -w for .t files, per Test::Harness $Switches
              (warnings (if (match-beginning 2) "-w " ""))
              (libdir   (concat topdir "lib"))
              (blibdir  (concat topdir "blib/lib"))
              (archdir  (concat topdir "blib/arch")))
         (concat "cd " (shell-quote-argument topdir) " && \\\n"
                 "echo \"Entering directory \\``pwd`'\" && \\\n"
                 "perl " warnings "-I " (shell-quote-argument libdir) " \\\n"
                 "     -I " (shell-quote-argument blibdir) " \\\n"
                 "     -I " (shell-quote-argument archdir) " \\\n"
                 "     " (shell-quote-argument filename)))))

(custom-add-option 'compile-command-default-functions
                   'compile-command-default-perl-t-raw)


;;-----------------------------------------------------------------------------

;; LocalWords: gcc gz pl gunzip MFilter zcat PerlIO usr MExtUtils ExtUtils
;; LocalWords: MakeMaker proj blib Mblib xs chdir toplevel perl stdin wT el
;; LocalWords: pm filename env dir gzip subdirectory xt devel Makefile
;; LocalWords: fallback Foo foo

(provide 'compile-command-default)

;;; compile-command-default.el ends here
