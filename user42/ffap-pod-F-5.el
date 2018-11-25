;;; ffap-pod-F.el --- follow Perl pod F<filename>

;; Copyright 2010, 2011, 2014 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 5
;; Keywords: files, ffap, perl
;; URL: http://user42.tuxfamily.org/ffap-pod-F/index.html
;; EmacsWiki: FindFileAtPoint

;; ffap-pod-F.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; ffap-pod-F.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This spot of code extends `M-x ffap' to recognise Perl POD F<> filename
;; markup like
;;
;;     F</etc/motd>
;;
;; By itself ffap takes the F< as part of the filename and so doesn't find
;; it.  See `ffap-pod-F-enable' below for more.

;;; Install:

;; Put ffap-pod-F.el in one of your `load-path' directories and the following
;; in your .emacs
;;
;;     (autoload 'ffap-pod-F-enable "ffap-pod-F" nil t)
;;
;; To enable it when ffap loads add the following
;;
;;     (eval-after-load "ffap" '(ffap-pod-F-enable))
;;
;; The function is interactive so `M-x ffap-pod-F-enable' can be use if only
;; wanted sometimes, etc.
;;
;; There's an autoload cookie for `ffap-pod-F-enable' if you know how to use
;; `update-file-autoloads' and friends, then just `eval-after-load' or
;; `M-x'.

;;; History:
;; 
;; Version 1 - the first version
;; Version 2 - unescape look in `sgml-char-names' too
;; Version 3 - oops, don't enable by default, only from ffap-pod-F-enable
;; Version 4 - allow /foo/no/such -> /foo reduction
;;           - exclude the F<> part from highlight
;; Version 5 - prefer `dotimes' over `position'

;;; Code:

;; for `ad-find-advice' macro when running uncompiled
;; (don't unload 'advice before our -unload-function)
(require 'advice)

(eval-when-compile
  (unless (fboundp 'dotimes)
    (require 'cl))) ;; for `dotimes' macro in emacs20

;; quieten the byte compiler, iso-cvt.el is in emacs but not xemacs21
(defvar iso-sgml2iso-trans-tab) ;; in iso-cvt.el
(defvar sgml-char-names)        ;; in sgml-mode.el

;;-----------------------------------------------------------------------------

;; Pod::Escapes
(defconst ffap-pod-F-Echars
  `((""         . "")
    ("lchevron" . "«")
    ("rchevron" . "»"))
  "An internal part of ffap-pod-F.el.
An alist of POD named escape chars like \"foo\" for E<foo>.
This is additional names not already in `sgml-char-names' and/or
`iso-sgml2iso-trans-tab'.")

(defun ffap-pod-F-ucs-string (num)
  "An internal part of ffap-pod-F.el.
Return a single-char string for unicode char NUM.
For Emacs 21 up this is simply `(string (decode-char 'ucs num))'.

For XEmacs 21 latin-1 characters are are always converted but
higher characters require `utf-16-be' charset, for example from
mule-ucs.  If `utf-16-be' is not available then the return for
the higher characters is nil."

  (cond ((eval-when-compile (fboundp 'decode-char))
         ;; emacs21 up
         (string (decode-char 'ucs num)))

        ((<= num 255)
         ;; xemacs21 latin1, maybe emacs20
         (decode-coding-string (string num) 'iso-8859-1))

        ((memq 'utf-16-be (coding-system-list))
         ;; xemacs21 with mule-ucs, maybe emacs20
         ;; not sure this is right on the surrogates D800 etc, but they
         ;; shouldn't occur
         (decode-coding-string (string (ash num -8)
                                       (logand num 255))
                               'utf-16-be))))
(eval-when-compile
  (put 'ffap-pod-F-ucs-string 'side-effect-free t))

(defun ffap-pod-F-string-to-number (str)
  "An internal part of ffap-pod-F.el.
STR is a string like decimal \"123\", hex \"0xFF\", or octal
\"0377\".  Return the integer value."
  (cond ((string-match "\\`0[0-7]*\\'" str)
         (string-to-number str 8))
        ((string-match "\\`[0-9]+\\'" str)
         (string-to-number str 10))
        ((string-match "\\`0[xX]\\([0-9a-fA-F]*\\)\\'" str)
         (string-to-number (match-string 1 str) 16))))

(defun ffap-pod-F-unescape-E (str)
  "An internal part of ffap-pod-F.
STR is the guts of an E<> escape, for example \"gt\" for E<gt>.
Return a string which is the character represented by that escape,
or return nil if STR is unrecognised.
See `ffap-pod-F-enable' for notes on the unescaping."

  (or (cdr (assoc str ffap-pod-F-Echars))

      (let ((num (ffap-pod-F-string-to-number str)))
        (and num
             (ffap-pod-F-ucs-string num)))

      (let (char)
        (dotimes (i (length sgml-char-names))
          (if (equal str (aref sgml-char-names i))
              (setq char i)))
        (and char (ffap-pod-F-ucs-string char)))

      (progn
        (require 'iso-cvt)
        ;; new in emacs21, not in xemacs21
        (and (boundp 'iso-sgml2iso-trans-tab)
             (cadr (assoc (concat "&" str ";")
                          iso-sgml2iso-trans-tab))))))

(defun ffap-pod-F-unescape (str)
  "An internal part of ffap-pod-F.el.
Return STR with any POD style E<char> and Z<> forms unescaped.
See `ffap-pod-F-enable' for notes on the unescaping."
  (let ((case-fold-search nil))
    (if (string-match "[EZ]<\\([^>]*\\)>" str)
        (progn
          (eval-and-compile ;; quieten the byte compiler
            (require 'sgml-mode))
          (with-temp-buffer
            (insert str)
            (goto-char (point-min))
            (while (re-search-forward "[EZ]<\\([^>]*\\)>" nil t)
              (let ((beg (match-beginning 0))
                    (end (match-end 0)))
                (when (setq str (ffap-pod-F-unescape-E (match-string 1)))
                  (delete-region beg end)
                  (insert str))))
            (buffer-string)))
      str)))

(defun ffap-pod-F-at-point ()
  "An internal part of ffap-pod-F.el.
Return a Perl POD markup F<> filename at point, or nil if none.
This function is designed for use within ffap and so sets
`ffap-string-at-point-region' to the filename part of the F<>,
ready to be highlighted for the user."

  (eval-and-compile ;; quieten the byte compiler
    (require 'ffap)) ;; for ffap-string-at-point-region
  (eval-and-compile ;; quieten the byte compiler
    (require 'thingatpt))

  ;; Narrowing to the current line as a speedup for big buffers.  It limits
  ;; the amount of searching forward and back that
  ;; `thing-at-point-looking-at' does when it works-around the way
  ;; re-search-backward doesn't match across point.
  ;;
  (and (save-restriction
         (narrow-to-region (line-beginning-position) (line-end-position))
         (let ((case-fold-search nil))
           (thing-at-point-looking-at "F<\
\\(\\(\\([EZ]<[^>]*>\\|[^<>]\\)*\\)>\
\\|< \\(.*?\\) >>\
\\|<< \\(.*?\\) >>>\
\\)")))
       (progn
         (setq ffap-string-at-point-region (list (or (match-beginning 4)
                                                     (match-beginning 5)
                                                     (match-beginning 2))
                                                 (or (match-end 4)
                                                     (match-end 5)
                                                     (match-end 2))))
         (or (match-string 4) ;; F<< >>
             (match-string 5) ;; F<<< >>>
             (ffap-pod-F-unescape (match-string 2)))))) ;; F<>

;; Mangle ffap-string-at-point rather than ffap-file-at-point.  Mangling
;; string-at allows a possibly non-existent directory to be pruned back.
;; Eg. F</tmp/no/such> offers /tmp the same as plain ffap on /tmp/no/such
;; offers /tmp.
;; 
(defadvice ffap-string-at-point (around ffap-pod-F disable)
  "Recognise Perl F<filename> per `ffap-pod-F-at-point'."
  (or (and (fboundp 'ffap-pod-F-at-point) ;; in case unloaded
           (setq ad-return-value (ffap-pod-F-at-point)))
      ad-do-it))

;;;###autoload
(defun ffap-pod-F-enable ()
  "Extend `ffap' to recognise Perl pod F<filename>.
The forms recognised are

    F<foo>
    F<< bar >>
    F<<< quux >>>

`ffap' doesn't by itself work on an F<> as it takes the \"F<\" as
part of the filename.  `ffap' does work on the multi-angle F<< >>
and F<<< >>> since the spaces separate the name from the markup
but `ffap-pod-F-at-point' extends it to work with point on the F
or the angles as well as the filename proper.

Escapes E<> like F<fooE<sol>bar> are expanded.  The POD basics
E<lt>, E<gt>, E<sol> etc are recognised, plus SGML names from
`sgml-char-names' and `iso-sgml2iso-trans-tab', and
latin-1/unicode numbers.  Obscure XML names might not be present
in `iso-sgml2iso-trans-tab' but could be added.  Z<> zero-widths
are stripped.  Escapes within an F<> will be unlikely, as
non-ascii rarely works well in filenames and otherwise only E<gt>
is necessary and > in a filename is unusual.

F<> is pod-specific but `ffap-pod-F-enable' applies it globally
since it shouldn't clash with anything else.

----
The `ffap-pod-F' home page is
URL `http://user42.tuxfamily.org/ffap-pod-F/index.html'"

  (interactive)
  (ad-enable-advice 'ffap-string-at-point 'around 'ffap-pod-F)
  (ad-activate      'ffap-string-at-point))

(defun ffap-pod-F-unload-function ()
  "An internal part of ffap-pod-F.el.
Remove defadvice from function `ffap-string-at-point'.
This is called by `unload-feature'."
  (when (ad-find-advice 'ffap-string-at-point 'around 'ffap-pod-F)
    (ad-remove-advice   'ffap-string-at-point 'around 'ffap-pod-F)
    (ad-activate        'ffap-string-at-point))
  nil) ;; and do normal unload-feature actions too

;; LocalWords: filename filenames motd unicode num latin charsets charsets
;; LocalWords: ucs xFF unescaped quux fooE lt ascii gt unescaping foo

;; Local variables:
;; coding: latin-1
;; End:

(provide 'ffap-pod-F)

;;; ffap-pod-F.el ends here
