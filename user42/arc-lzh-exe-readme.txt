;;; Commentary:

;; This is a spot of code for viewing the contents of LHa (ie. LZH)
;; self-extracting executables from `archive-mode'.  The executable part,
;; which is meant for MS-DOS, is not executed.
;;
;; There's no support for modifying such files, because it seems the Unix
;; version of the lha program doesn't have support for writing them (only
;; reading).
;;
;; A random .exe file of course might not be an LHa file, so `archive-exe-p'
;; and `archive-lzh-exe-mode-maybe' below check the contents before going to
;; archive-mode.
;;

