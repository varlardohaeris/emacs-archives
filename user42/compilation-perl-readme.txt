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

