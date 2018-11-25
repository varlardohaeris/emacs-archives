;;; Commentary:

;; `M-x perl-env-substitute-enable' extends `substitute-in-file-name' to
;; expand Perl style $ENV{VARNAME} as well as shell $VARNAME or ${VARNAME}.
;;
;; The main use is for following an interpolated string with M-x ffap.  By
;; default ffap doesn't accept braces {} at all, but
;; `perl-env-substitute-ffap-braces' can set that up, which is good for
;; shell script code with ${HOME} etc too.

