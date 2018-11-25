;;; Commentary:

;; This is a spot of code for getting the right coding system when visiting
;; an EBook or EText from Project Gutenberg,
;;
;;     http://www.gutenberg.org      PG
;;     http://gutenberg.net.au       PG Australia
;;     http://www.gutenberg.nl       PG EU
;;
;; Gutenberg files come in various encodings.  Most have a "Character set
;; encoding" in the file, the code here looks for that.
;;
;; The code works both for a plain .txt file and for a .txt visited from a
;; .zip file (via `archive-mode').

