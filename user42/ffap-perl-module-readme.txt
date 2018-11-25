;;; Commentary:

;; This spot of code lets M-x ffap find the source file for a Perl module.
;; For example Foo::Bar becomes /usr/share/perl5/Foo/Bar.pm or wherever in
;; the path.
;;
;; Variable names or sub-packages are stripped, and a prefix is added if
;; unique.  See the `ffap-perl-module-file-at-point' docstring below for
;; details.
;;
;; The lookup is independent of the major mode, so you can be in Man-mode,
;; diff-mode, pod-mode etc and still go to Perl source.

