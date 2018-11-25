;;; Commentary:
;;
;; This spot of code converts Perl single-quote '' strings to "", or back
;; again.  q{} and qq{} styles are supported, as are Locale::TextDomain
;; style __"" and N__"" translations.
;;
;; Move point to the start of the target string and use `perl-quote-single'
;; to make it a single '', or `perl-quote-double' to make it a double.  The
;; suggested key bindings are C-c ' and C-c " respectively, which
;; `perl-quote-keybindings' below can install.  See the docstrings for more.
;;
;; Both commands use `forward-sexp' to find the end of string, so if you're
;; not in perl-mode or cperl-mode then the mode will have to understand Perl
;; strings enough for `forward-sexp' to be right.  There's no real checking
;; of that, but for interactive use you can easily undo if it comes out
;; badly wrong.

;;; Install
;;
;; Put perl-quote.el in one of your `load-path' directories and in your
;; .emacs add
;;
;;     (autoload 'perl-quote-keybindings "perl-quote")
;;     (add-hook 'perl-mode-hook  'perl-quote-keybindings)
;;     (add-hook 'cperl-mode-hook 'perl-quote-keybindings)
;;
;; Or whichever perl-like modes you use, including maybe `pod-mode-hook' for
;; Perl code samples in POD files.
;;
;; There's autoload cookies for the functions and likely hook customize
;; options, if you know how to use `update-file-autoloads' and friends.

