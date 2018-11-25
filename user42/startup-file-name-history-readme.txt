;;; Commentary:

;; This spot of code puts filenames on the Emacs command line into
;; `file-name-history', in addition to opening them in buffers in the usual
;; way.
;;
;; savehist.el can be used together with startup-file-name-history.el.
;; `savehist-mode' initializes `file-name-history' when .emacs loads, then
;; startup-file-name-history.el runs later during command line processing
;; and adds the command line files to whatever savehist loaded.

