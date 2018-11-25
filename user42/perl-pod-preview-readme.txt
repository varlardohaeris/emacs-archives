;;; Commentary:

;; M-x perl-pod-preview displays a preview of Perl POD format documentation
;; using pod2man and man or woman.  It can show POD inlined in Perl code or
;; in a separate .pod file.  See the `perl-pod-preview' docstring below for
;; details.
;;
;; The best feature is that when re-previewing the same file or buffer the
;; existing position in the preview is preserved, so if you change the
;; source a little you should be still quite close to it in the preview to
;; see how the change looks.
;;
;; Running man or woman for the formatting is unsophisticated, but it's a
;; fairly deliberate choice because pod2man+man is probably how most people
;; will look at your docs, so seeing what that gives is a good thing.

