;;; compilation-perl.el --- extra error regexps for Perl

;; Copyright 2007, 2008, 2009, 2010, 2011, 2015 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 14
;; Keywords: processes, compilation
;; URL: http://user42.tuxfamily.org/compilation-perl/index.html
;; EmacsWiki: PerlLanguage

;; compilation-perl.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; compilation-perl.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This spot of code adds patterns to `compilation-error-regexp-alist' for
;; the following Perl things
;;
;;     * die() messages with "during global destruction" (for Emacs 22)
;;     * die() messages with "." (for Emacs 20)
;;     * Pod::Checker module and podchecker program (for Emacs 22)
;;     * Pod::Simple module
;;     * Devel::Backtrace module
;;     * Test.pm module
;;     * Test::Builder module, as used by Test::More and others
;;     * xsubpp program
;;
;; And toning down Emacs 22 "gnu" pattern for the benefit of
;;
;;     * Glib::Log module, as used by Gtk2-Perl etc
;;
;; Emacs has a pattern for Perl's normal compile and run errors, but the
;; extra programs and modules are different.  Patterns adopted into Emacs 23
;; are not duplicated.
;;
;; Perl::Critic patterns are available in its perlcritic.el (and some also
;; in a different perlcritic.el which comes with the PDE package).  The
;; Perl::Critic output is configurable, so you're best off just asking it
;; for GNU style "file:line:column:" anyway.

;;; Emacsen:

;; Designed for Emacs 21 and up, works in Emacs 20 and XEmacs 21.

;;; Install:

;; Put compilation-perl.el in one of your `load-path' directories, and in
;; your .emacs add
;;
;;     (eval-after-load "compile" '(require 'compilation-perl))
;;
;; There's an autoload cookie below for this, if you're brave enough to use
;; `update-file-autoloads' and friends.

;;; History:

;; Version 1 - the first version
;; Version 2 - fixes for Test and Test::Harness
;; Version 3 - add xsubpp
;; Version 4 - add "during global destruction"
;; Version 5 - add Test::Builder and a hack for Glib::Log
;; Version 6 - add Pod::Simple "complain_stderr"
;; Version 7 - notice podchecker now in emacs23
;;           - add die pattern for emacs 23.1
;; Version 8 - 23.1 pattern ok in fact
;; Version 9 - no need to anchor global destruction pattern in emacs22
;; Version 10 - better Test, Test::Builder and Test::Harness stuff
;; Version 11 - some bits for emacs20
;; Version 12 - add Test.pm "fail #n"
;; Version 13 - add Devel::Backtrace
;; Version 14 - prefer builtin `dolist' macro


;;; Code:

;;;###autoload (eval-after-load "compile" '(require 'compilation-perl))
(require 'compile)

(eval-when-compile
  (unless (fboundp 'dolist)
    (require 'cl))) ;; for `dolist' macro in emacs20

(let ((elem-list
       '(;; Pod::Simple module messages, if you enable its "complain_stderr"
         ;; setting.  Style per its _complain_warn() function, eg.
         ;;
         ;;     foo.pod around line 9: Unknown directive: =fsdkfsd
         ;;
         ;; The pattern "around line N:" is probably close enough to
         ;; unambiguous.  Not fantastic, but hopefully ok in practice.
         ;; Excluding spaces from the filename keeps a lid on how far the
         ;; pattern goes looking for an "around line".
         ;;
         (compilation-perl--Pod::Simple
          "^\\([^ \t\r\n]+\\) around line \\([0-9]+\\): " 1 2)

         ;; Test.pm and Test::Builder fail messages.
         ;; Test.pm ok() function on a plain boolean test gives,
         ;;
         ;;     # Failed test 1 in foo.t at line 6
         ;;
         ;; And Test::Builder (eg. module version 0.94, and as used for
         ;; instance by Test::More) gives similar in its ok().  The # is
         ;; added by Test::Builder diag().  Eg. with no test name,
         ;;
         ;;     #   Failed test in foo.t at line 5.
         ;;
         ;; Or with a test name,
         ;;
         ;;     #   Failed test 'my name'
         ;;     #   in foo.t at line 5.
         ;;
         ;; Or with a multi-line name,
         ;;
         ;;     #   Failed test 'my name
         ;;     #   blah
         ;;     #   '
         ;;     #   in foo.t at line 5.
         ;;
         ;; Both Test and Test::Harness can be preceded by a progress part
         ;; from Test::Harness, so match anywhere in the line, eg.
         ;;
         ;;     ../devel/d-compilation-perl.t .. 1/1 # Failed test 1 in ../devel/d-compilation-perl.t at line 27
         ;;
         ;; A Test::Builder message "# Failed (TODO) test" is deliberately
         ;; not matched, since a test flagged as TODO isn't an error.  If
         ;; you want to match that you can slip a "\\( (TODO)\\)?" into the
         ;; pattern (perhaps classing it as a warning).
         ;;
         (compilation-perl--Test-failed
          "# +Failed test.*?\\(\n#.*?\\)*? +in \\([^ \t\r\n]+\\) at line \\([0-9]+\\)"
          2 3)

         ;; xsubpp program messages
         ;; Style per the Warn() function in the xsubpp script, eg.
         ;;
         ;;     Warning: #if without #endif in this function in Foo.xs, line 12
         ;;
         ;;     Error: `endif' with no matching `if' in Foo.xs, line 25
         ;;
         ;;     Code is not inside a function (maybe last function was ended by a blank line  followed by a statement on column one?) in Foo.xs, line 19
         ;;
         ;; The same style "in FILENAME, line NNN" is used in a couple of
         ;; other places too.  The "Warning:" or "Error:" bit is not
         ;; always present, treat messages with neither as errors.
         ;;
         ;; The "Warning:" form matches the whole line, but without is just
         ;; the "in" part.  A ".*", or making the "Warning:" optional, would
         ;; make it consistent, but is slow on long lines.
         ;;
         ;; A simple "in X, line Y" might risk matching something unrelated,
         ;; perhaps with X a function name instead of a filename.  Require a
         ;; filename ending ".xs" as a protection against that.
         ;;
         (compilation-perl--xsubpp
          "\\(\\(Warning:\\).*\\)? in \\([^ \t\r\n]+\\.xs\\), line \\([0-9]+\\)"
          3 4 nil (2))


         ;; Devel::Backtrace $backtrace->to_string() form is the "default" in
         ;; Devel::Backtrace::Point %formats, being
         ;;
         ;;   '%s called from %p (%f:%l)' such as
         ;;
         ;;   Devel::Backtrace::new called from main (examples/basic.pl:12)
         ;;
         ;; A .* at the start could get the whole line fontified, but
         ;; matching that is pretty slow on long lines.
         ;;
         ;; For reference, the format from Devel::StackTrace is like Perl's
         ;; plain die(), so it's picked up by the usual patterns, no need
         ;; for anything special.  You can choose whether you like
         ;; Devel::Backtrace or Devel::StackTrace better.
         ;;
         (compilation-perl--Devel::Backtrace-short
          " called from .* (\\([^)]+\\):\\([0-9]+\\)" 1 2)

         ;; Devel::Backtrace $backtrace->to_string_long() form is each field
         ;; from Devel::Backtrace::Point @known_fields, which is made public
         ;; in the FIELDS constant.
         ;;
         ;;     package: main
         ;;     filename: examples/basic.pl
         ;;     line: 38
         ;;
         ;; The first line "package: FOO" is matched to get that included in
         ;; the locus shown by `next-error' etc stepping, though only
         ;; "filename:" and "line:" are used in the pattern.
         ;;
         (compilation-perl--Devel::Backtrace-long
          "^package: .*\nfilename: \\(.*\\)\nline: \\([0-9]+\\)" 1 2))))


  ;; Test.pm message when comparing two values ok($got,$want), as
  ;; printed in its _complain(), builtin in emacs 24 probably
  ;;
  ;;     # Test 2 got: "xx" (d-compilation-perl-2.t at line 10)
  ;;
  ;; Or if an ok() on a given line is called more than once (a loop,
  ;; a func, etc) then with a "#n" count bit for the second and
  ;; subsequent calls.  (ok() func adding $repetition to the
  ;; $context.)
  ;;
  ;;     # Test 6 got: "xx" (d-compilation-perl-2.t at line 33 fail #2)
  ;;
  ;; And under Test::Harness can be preceded by progress stuff so
  ;; allow match anywhere in the line.
  ;;
  ;;     ... NOK 1# Test 1 got: "1234" (t/foo.t at line 46)
  ;;
  (unless (and (eval-when-compile
                 (boundp 'compilation-error-regexp-alist-alist))
               (let ((pattern (cadr (assq 'perl--Test2 compilation-error-regexp-alist-alist))))
                 (equal pattern "^\\(.*NOK.*\\)?# Test [0-9]+ got:.* (\\([^ \t\r\n]+\\) at line \\([0-9]+\\)\\( fail #[0-9]+\\)?)")))

    (add-to-list
     'elem-list
     '(compilation-perl--Test
       "# Test [0-9]+ got:.* (\\([^ \t\r\n]+\\) at line \\([0-9]+\\)\\( fail #[0-9]+\\)?)"
       1 2)))


  ;; global destruction messages, for pre-emacs23 (builtin in 23 up),
  ;;
  ;;     (in cleanup) something bad at foo.pl line 3 during global destruction.
  ;;
  ;; Emacs 21 and 22 have a perl pattern, but it doesn't match with a
  ;; "during global destruction" part.  Here's a program provoking global
  ;; destruction:
  ;;
  ;;     #!/usr/bin/perl
  ;;     use warnings;
  ;;     sub DESTROY { die "something bad"; }
  ;;     my $x; $x = bless {x=>\$x}, 'main';
  ;;     exit 0;
  ;;
  (unless (and (eval-when-compile
                 (boundp 'compilation-error-regexp-alist-alist))
               (let ((pattern (cadr (assq 'perl compilation-error-regexp-alist-alist))))
                 (and (stringp pattern) ;; emacs string, not xemacs list
                      (string-match "global destruction" pattern))))
    (add-to-list
     'elem-list
     '(compilation-perl--global-destruction
       " at \\([^ \n]+\\) line \\([0-9]+\\) during global destruction\\.$"
       1 2)))

  ;; emacs20 pattern ending with "," is too tight, allow "." or eol as per
  ;; later compile.el
  ;;
  (when (assoc ".* at \\([^ ]+\\) line \\([0-9]+\\),"
               compilation-error-regexp-alist)
    (add-to-list 'elem-list
                 '(compilation-perl--emacs20-dot
                   " at \\([^ \n]+\\) line \\([0-9]+\\)\\([.,]\\|$\\)"
                   1 2)))


  ;; Pod::Checker module messages, as from the podchecker program.  Emacs 23
  ;; has incorporated this, under the name `perl--Pod::Checker', so omit
  ;; there.  The style is per the Pod::Checker::poderror() function, eg.
  ;;
  ;;     *** ERROR: Spurious text after =cut at line 193 in file foo.pm
  ;;
  ;; Plus end_pod() can give "at line EOF" instead of a number, so
  ;; for that match "on line N" which is the originating spot, eg.
  ;;
  ;;     *** ERROR: =over on line 37 without closing =back at line EOF in file bar.pm
  ;;
  ;; Plus command() can give both "on line N" and "at line N".  The
  ;; latter is desired and is matched because the .* is greedy.
  ;;
  ;;     *** ERROR: =over on line 1 without closing =back (at head1) at line 3 in file x.pod
  ;;
  (unless (eval-when-compile
            (and (eval-when-compile ;; extra eval to quieten xemacs21,emacs20
                   (boundp 'compilation-error-regexp-alist-alist))
                 (assq 'perl--Pod::Checker
                       compilation-error-regexp-alist-alist)))
    (add-to-list
     'elem-list
     '(compilation-perl--Pod::Checker
       ;;            1          2                  3                  4            5                 6
       "^\\*\\*\\* \\(ERROR\\|\\(WARNING\\)\\).* \\(at\\|on\\) line \\([0-9]+\\) \\(.* \\)?in file \\([^ \t\n]+\\)"
       6 4 nil (2))))


  ;; now actually applying `elem-list'
  ;;
  (dolist (elem elem-list)
    (let ((symbol (nth 0 elem))
          (regexp (nth 1 elem))
          (file   (nth 2 elem))
          (line   (nth 3 elem))
          (column (nth 4 elem))
          (type   (nth 5 elem)))

      (cond ((eval-when-compile
               (boundp 'compilation-error-regexp-systems-list))
             ;; xemacs21
             (unless (string-match "^\\^" regexp) ;; per emacs21 comments below
               (setq regexp (concat "^.*" regexp)))
             (add-to-list 'compilation-error-regexp-alist-alist
                          (list symbol
                                (list regexp file line)))
             (compilation-build-compilation-error-regexp-alist))

            ((eval-when-compile (boundp 'compilation-error-regexp-alist-alist))
             ;; emacs22
             (add-to-list 'compilation-error-regexp-alist symbol)
             (add-to-list 'compilation-error-regexp-alist-alist
                          (list symbol regexp file line column type)))

            (t
             ;; emacs21
             ;; In emacs21 matches are sought only at the start of each
             ;; line, so regexps allowed to be anywhere in the line must
             ;; have a ".*" added to work.  For the patterns established
             ;; here this means anything without a "^" anchoring to the
             ;; start of the line.
             (unless (string-match "^\\^" regexp)
               (setq regexp (concat "^.*" regexp)))
             (add-to-list 'compilation-error-regexp-alist
                          (list regexp file line)))))))


;;-----------------------------------------------------------------------------
;; The following is a nasty hack to the "gnu" pattern in Emacs 22.
;; On a perl Glib::Log message like the following, as per GLog.xs
;; gperl_log_handler(),
;;
;;     GLib-GObject-WARNING **: /build/buildd/glib2.0-2.14.5/gobject/gsignal.c:1741: instance `0x8206790' has no handler with id `1234' at t-compilation-perl-gtk.pl line 3.
;;
;; the "gnu" pattern of Emacs 22 takes the whole of
;;
;;     GLib-GObject-WARNING **: /build/buildd/glib2.0-2.14.5/gobject/gsignal.c
;;
;; as the filename.  Firstly of course that start part "Glib-GObject" bit is
;; not right and secondly it's unhelpful to go to gsignal.c when the perl
;; filename at the end is the interesting bit.  That latter is matched by
;; the ordinary perl patterns, but `next-error' goes to the first on the
;; line, and so asks where the supposed "blah blah gsignal.c" filename is.
;;
;; A new "compilation-perl--munged-gnu" pattern is added to
;; compilation-error-regexp-alist-alist, disallowing "*" in the filename
;; part.  It should be very rare to have "*" in a filename so this won't
;; hurt the things the gnu pattern should normally match.
;;
;; This new pattern is enabled by removing the "gnu" symbol from
;; compilation-error-regexp-alist, replacing it with the new
;; "compilation-perl--munged-gnu".  The actual "gnu" regexp in
;; compilation-error-regexp-alist-alist is unchanged, so if you need the
;; original in some particular mode it's just a matter of the symbols
;; selected in compilation-error-regexp-alist.
;;
;; In Emacs 23, Emacs 21 and XEmacs 21 the gnu entry has a tighter filename
;; pattern and already doesn't match "GLib-GObject-WARNING **:...", so
;; nothing needs to be done there.
;;
(when (eval-when-compile
        (and (boundp 'compilation-error-regexp-alist-alist)
             (memq 'gnu compilation-error-regexp-alist))) ;; only Emacs 22
  (let* ((gnu-elem (assoc 'gnu compilation-error-regexp-alist-alist))
         (regexp   (cadr gnu-elem)))
    (when ;; only for the form present in Emacs 22.1
        (string-match
         (regexp-quote "\\([0-9]*[^0-9\n]\\(?:[^\n ]\\| [^-\n]\\)*?\\):")
         regexp)
      (setq regexp (replace-match
                    "\\([0-9]*[^0-9\n]\\(?:[^*\n ]\\| [^-*\n]\\)*?\\):"
                    t t ;; fixedcase and literal
                    regexp))
      (let ((new-elem (copy-tree gnu-elem)))
        (setcar new-elem 'compilation-perl--munged-gnu)
        (setcar (cdr new-elem) regexp)
        (add-to-list 'compilation-error-regexp-alist-alist new-elem)

        (setq compilation-error-regexp-alist
              (remove 'gnu compilation-error-regexp-alist))
        (add-to-list 'compilation-error-regexp-alist
                     'compilation-perl--munged-gnu)))))


;;-----------------------------------------------------------------------------
;; Some hacks to prune back what Emacs 23 has builtin, where the
;; compilation-perl patterns above can do better.
;;
;; The changes remove the pattern symbol from
;; `compilation-error-regexp-alist', but leave the entry in the
;; `-alist-alist' for use if in fact still desired.

(when (eval-when-compile (boundp 'compilation-error-regexp-alist-alist))
  (dolist (elem '(
                  ;; This `perl--Test' entry in Emacs 23.1 was unnecessarily
                  ;; restrictive in the "^", as a "# Failed test" can occur
                  ;; in the middle of a line under Test::Harness.  Prefer
                  ;; `compilation--Test-failed' above.
                  ;;
                  (perl--Test
                   "^# Failed test [0-9]+ in \\([^ \t\r\n]+\\) at line \\([0-9]+\\)"
                   1 2)

                  ;; This `perl--Test2' entry in Emacs 23.1 was not very
                  ;; good in unnecessarily demanding NOK.
                  ;; Prefer`compilation--Test' above.
                  ;;
                  (perl--Test2
                   "^\\(.*NOK.*\\)?# Test [0-9]+ got:.* (\\([^ \t\r\n]+\\) at line \\([0-9]+\\))"
                   2 3)

                  ;; This `perl--Test::Harness' entry in Emacs 23.1 was not
                  ;; very good, it mismatched the Test.pm "Test 1 got: ..."
                  ;; output, getting the "(" into the filename
                  ;;
                  (perl--Test::Harness
                   "^.*NOK.* \\([^ \t\r\n]+\\) at line \\([0-9]+\\)"
                   1 2)))

    (when (member elem compilation-error-regexp-alist-alist)
      (setq compilation-error-regexp-alist
            (remove (car elem) compilation-error-regexp-alist)))))

;; LocalWords: podchecker xsubpp Gtk Devel Backtrace el pm perlcritic

(provide 'compilation-perl)

;;; compilation-perl.el ends here
