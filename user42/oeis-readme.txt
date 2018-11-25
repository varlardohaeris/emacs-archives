;;; Commentary:
;;
;; This is some helper commands for the Online Encyclopedia of Integer
;; Sequences (OEIS), http://oeis.org
;;
;;     M-x oeis-browse-anum        goto sequence web page
;;     M-x oeis-ffap-enable        for M-x ffap A-number at point
;;     M-x oeis-search             web site search
;;     M-x oeis-superseeker-mail   superseeker search
;;     M-x oeis-download           download html, b-file, etc
;;     M-x oeis-grep               search local stripped file
;;
;; Sequence web pages and b-files are first sought in `oeis-local-directories'
;; so as to go to local downloaded copies before the web site.

