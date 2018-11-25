;;; perl-pod-gt.el --- helpers for Perl POD <> markup

;; Copyright 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 18
;; Keywords: languages, perl, wp
;; URL: http://user42.tuxfamily.org/perl-pod-gt/index.html
;; EmacsWiki: PerlPodGt

;; perl-pod-gt.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; perl-pod-gt.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; This spot of code helps when writing Perl POD markup forms like C<...>,
;; B<...>, etc.  `perl-pod-gt-enable' sets up
;;
;;    * ">" key for smart E<gt> insertion
;;    * suppress line breaks when filling
;;    * some warning face overlays
;;
;; Commands M-x perl-pod-gt-single and M-x perl-pod-gt-double convert
;; C<<..>> to C<...> or vice versa, when you want to upgrade or downgrade.

;;; Emacsen:

;; Designed for Emacs 21 up.  Works in XEmacs 21 except there's no "nobreak"
;; feature there.  Partly works in Emacs 20 (`perl-pod-gt-single' doesn't
;; work, and on a tty there's no faces for the overlays).

;;; Install
;;
;; Put perl-pod-gt.el in one of your `load-path' directories and add to your
;; .emacs
;;
;;     (autoload 'perl-pod-gt-double "perl-pod-gt" nil t)
;;     (autoload 'perl-pod-gt-single "perl-pod-gt" nil t)
;;     (autoload 'perl-pod-gt-enable "perl-pod-gt")
;;     (add-hook 'perl-mode-hook 'perl-pod-gt-enable)
;;
;; And to use in cperl-mode, pod-mode, or similar
;;
;;     (add-hook 'cperl-mode-hook 'perl-pod-gt-enable)
;;     (add-hook 'pod-mode-hook   'perl-pod-gt-enable)
;;
;; There's autoload cookies for the functions and the customize options on
;; the hooks if you know how to use `update-file-autoloads' and friends or
;; install with `M-x package-install'.

;;; History:
;;
;; Version 1 - the first version
;; Version 2 - new `perl-pod-gt-double'
;; Version 3 - warning face done with overlays
;; Version 4 - don't line break after C<!>
;; Version 5 - new home page
;; Version 6 - new perl-pod-gt-single
;; Version 7 - case-sensitive for tighter C<>, in particular in the overlays
;;           - some emacs20 support
;; Version 8 - perl-pod-gt-single act on literal contained E<lt> too
;; Version 9 - add make-local-hook for xemacs21
;; Version 10 - perl-pod-gt-insert obey `overwrite-mode'
;; Version 11 - C-u > to force E<gt>, new C-u < similarly
;;            - perl-pod-gt-double convert E<lt> to < too
;; Version 12 - nobreak after C<#!> too
;; Version 13 - warning face allow C<==> and similar operators
;; Version 14 - warning face also ->[] ->{} ->()
;; Version 15 - nobreak in X<>
;; Version 16 - allow break in # X< or S< for mathematics in comments
;;            - must save-match-data in after-change-functions
;; Version 17 - perl-pod-gt-double don't make markup when converting E<lt>
;; Version 18 - quieten byte compiler on emacs20 dolist

;;; Code:

;; xemacs21 doesn't pre-load `overlay-put', so (require 'overlay).
;; But emacs20 doesn't have 'overlay feature, so cannot (require 'overlay)
;; there, so check for `overlay-put' already defined.
(eval-and-compile
  (unless (fboundp 'overlay-put)
    (require 'overlay))) ;; for xemacs21

(eval-when-compile
  (unless (fboundp 'dolist)
    (require 'cl))) ;; for `dolist' in emacs20


;;-----------------------------------------------------------------------------
;; compatibility

(defun perl-pod-gt-insert-command-char (char)
  "Insert CHAR as if entered by the user per `self-insert-command'.
This is an internal part of perl-pod-gt.el.
XEmacs offers `self-insert-internal' at the Lisp level, but for Emacs it
must be done with a temporary `last-command-event'."
  (if (eval-when-compile (fboundp 'self-insert-internal))
      (self-insert-internal char) ;; xemacs
    (let ((last-command-event char))
      (self-insert-command 1)))) ;; emacs


;;-----------------------------------------------------------------------------

(defun perl-pod-gt-find-markup ()
  "Find Perl POD C<...> markup surrounding point.
This might change, but here's how it works now:

If point is within a C<...>, B<<...>> etc markup form then move
point to the starting C, B, etc character, and return symbol `E'
if point was also within an E<> in that markup, or t if in an
ordinary part of that markup.

If point is not within a markup at all then return nil and point
is moved somewhere unspecified.

The strategy is to search forward from the start of the paragraph
for a C<, C<<, C<<< etc which extends across point.  This means
apparent nested forms like C<< B<x> >> get the outermost markup,
which is the way the formatters will treat it.  Looking just from
the start of the paragraph keeps down the amount parsed."

  (save-restriction
    (narrow-to-region (save-excursion (backward-paragraph) (point))
                      (point)) ;; start of paragraph through to point
    (let ((case-fold-search nil)
          (ret nil))
      (goto-char (point-min))
      (while (and (not ret)
                  ;; recognise markup forms I<>, B<> etc per 5.10.1 perlpod
                  (or (and (looking-at "[IBCLFSX]<+")
                           (goto-char (match-end 0)))
                      (re-search-forward "[IBCLFSX]<+" nil t)))

        (let* ((beg    (match-beginning 0))
               (angles (- (match-end 0) (match-beginning 0) 1))
               (re     (concat (make-string angles ?>)
                               "\\|E<[^>]*\\(\\(>\\)\\|$\\)"))
               (found  nil))
          (while (and (setq found (re-search-forward re nil t))
                      (match-beginning 2))) ;; matching a whole E
          (cond ((not found) ;; no end, so within markup
                 (goto-char beg)
                 (setq ret t))
                ((match-beginning 1) ;; last was a partial E<
                 (goto-char beg)
                 (setq ret 'E)))))
      ret)))

;;-----------------------------------------------------------------------------
;; paragraph breaking

(defun perl-pod-gt-in-comment-p ()
  "Return non-nil if point is within a comment.
This is an internal part of perl-pod-gt.el.

Return non-nil if point is within a comment, meaning a line starting
with `comment-start-skip'."

  (and comment-start-skip ;; nil if no comment syntax
       (save-excursion
         (beginning-of-line)
         (skip-chars-forward " \t")
         (looking-at comment-start-skip))))

;;;###autoload
(defun perl-pod-gt-nobreak-p ()
  "Don't break in certain Perl POD markup forms.
This function is designed for use in `fill-nobreak-predicate'.
It avoids a line break in the following circumstances,

  * Anywhere within an S<...> non-breaking form.
  * Anywhere within an X<...> index entry.
  * Immediately after a perl 5.6 style opening C<< or B<<< etc.
  * Immediately before a perl 5.6 style closing >> or >>> etc.
  * After a C<!> or C<#!>

S<> forms tell the formatters to avoid line breaks in the
contents and it's good to avoid them in the source too.

X<> index entries with a newline were not handled correctly by
Pod::Man 2.26, resulting in anything after a newline displaying
as ordinary text.  Keeping X<> all on one line avoids the
problem.

S<> and X<> are not applied in \"#\" comments since mathematical
comments like X<Y or S<2 can look like long markups and so be
long nobreaks.  If you have POD in \"#\" comments then after
uncommenting do a `fill-paragraph' to have S<> and X<> nobreaks
applied.

Perl 5.6 multi-angle C<< >> etc forms require whitespace after
the open and before the close.  A newline is fine for the
formatters, but it's easier to read the source if the markup is
on the same line as the first and last word of the content.  This
is a little like what `fill-french-nobreak-p' (new in Emacs 22)
does for double angle quote marks.

C<!> is likely to be talking about the ! operator, or C<#!> about
the shell interpreter mechanism, rather than an exclamation to
end a sentence.  Avoiding a line break stops nroff/troff putting
two spaces after it (at least with what pod2man of perl 5.10
generates)."

  (let ((case-fold-search nil))
    (or (save-excursion
          (skip-chars-backward " \t")
          (goto-char (- (point) 4))
          (looking-at "C<!>"))

        (save-excursion
          (skip-chars-backward " \t")
          (goto-char (- (point) 5))
          (looking-at "C<#!>"))

        (let ((orig-point (point)))
          (save-excursion
            (and (perl-pod-gt-find-markup)
                 (or
                  ;; No break anywhere within an S<> or X<>.
                  ;; Must have a closing ">".
                  ;; Must not be in a comment as that's probably mathematics.
                  (and (looking-at "[SX]<+[^>\n]*>")
                       (not (perl-pod-gt-in-comment-p)))

                  ;; no break immediately after a C<, B<<, etc
                  (progn
                    (looking-at ".\\(<+\\) *")   ;; opening angles <<<
                    (>= (match-end 0) orig-point))

                  ;; no break before terminating >>
                  (let ((angles (- (match-end 1) (match-beginning 1))))
                    (goto-char orig-point)
                    (looking-at (concat " *" (make-string angles ?>)))))))))))

;; Another possibility for X<> is to reckon X<= as mathematics not an X<>
;; index entry.
;; Old test, no break anywhere at all within an X<>
;; (member (char-after) '(?S ?X))


;;-----------------------------------------------------------------------------
;; < and > key commands

;;;###autoload
(defun perl-pod-gt-insert (&optional arg)
  ;; checkdoc-params: (arg)
  "Insert either \">\" or \"E<gt>\" as needed for Perl POD.
If inside a C<>, B<> etc form and you type \"->\", \"=>\" or a
space and \">\", then \">\" is inserted as \"E<gt>\".

It's easy to forget to escape the > when typing arrows or
\"greater than\" expression.  `perl-pod-gt-insert' tries to help
the common cases.  It also tries to keep out of your hair by only
acting when it's fairly clear E<gt> is right.

Perl 5.6 style doubled \"C<<...>>\" etc is recognised and doesn't
need escaping.  `perl-pod-gt-insert' just inserts a plain \">\"
in that case.

With prefix \\[universal-argument], insert \"E<gt>\" irrespective of the context.
This is good if the guessing is wrong, or just to force an E<>
form.  The converse, to force a plain \">\" can be done with
`\\[quoted-insert] >' in the usual way.

This command is designed to be bound to the \">\" key in
`perl-mode', `cperl-mode', `pod-mode', etc.  See
`perl-pod-gt-enable' to set that up."

  (interactive "P")
  (dolist (char
           ;; list of chars to insert
           (if (or arg   ;; arg means E<gt> always, otherwise auto-detect
                   (and (memq (char-before) '(?- ?= ? ))
                        (save-excursion
                          (and (eq t (perl-pod-gt-find-markup)) ;;in markup and not in E<>
                               (not (looking-at ".<<")))))) ;;not for 2 or more <<
               '(?E ?< ?g ?t ?>)
             '(?>)))
    (perl-pod-gt-insert-command-char char)))

;;;###autoload
(defun perl-pod-lt-insert (&optional arg)
  ;; checkdoc-params: (arg)
  "Insert either \"<\" or \"E<lt>\" Perl POD.
With prefix \\[universal-argument] insert \"E<lt>\", otherwise insert a plain \"<\".

Perhaps in the future there'll be some automated detection of
places where a E<lt> is wanted, but for now there's only
\\[universal-argument] similar to what `perl-pod-gt-insert' has.

This command is designed to be bound to the \"<\" key in
`perl-mode', `cperl-mode', `pod-mode', etc.  See
`perl-pod-gt-enable' to set that up."

  (interactive "P")
  (dolist (char (if arg
                    '(?E ?< ?l ?t ?>)
                  '(?<)))
    (perl-pod-gt-insert-command-char char)))

;;-----------------------------------------------------------------------------
;; markup double/single commands

;;;###autoload
(defun perl-pod-gt-double ()
  "Convert C<> style markup at point to C<< >>.
E<lt> or E<gt> escapes in the markup are converted to plain < or
> since those escapes are not needed in C<< >>.  Except,

- Runs E<gt>E<gt> are not changed to >> since that could
  wrongly be an end marker.
- E<gt> after another > like E<sol>E<gt> is not changed
  since E<sol>> etc is hard for human readers to see.
- E<lt> after IBCLFSX is not changed since since I< etc would be
  markup."

  (interactive)

  (let ((case-fold-search nil))
    (save-excursion
      (let ((orig-point (point)))
        (unless (perl-pod-gt-find-markup)
          ;; not on the inside of a C<> forms, but possibly on the C or <
          (goto-char orig-point)
          (or (looking-at "[IBCLFSX]<[^<]")
              (goto-char (max (1- (point)) (point-min))))))

      (or (looking-at "[IBCLFSX]\\(<\\)[^<]") ;; single C<... form
          (error "Not in a C<...> etc single angles form"))

      ;; "C<" becomes "C<< "
      (replace-match "<< " t t nil 1)

      ;; "E<gt>" becomes ">"
      ;; "E<lt>" becomes "<"
      ;; final ">" becomes " >>", if present
      (while (and (re-search-forward "\
\\(E<lt>\\)+\
\\|\\(E<gt>\\)+\
\\|\\(E<[^>]*>\\)\
\\|\\(>\\)"
                                     nil t)
                  (cond ((match-beginning 4)
                         ;; final ">", change to >> and stop looping
                         (replace-match " >>" t t)
                         nil)

                        ((match-beginning 3)
                         ;; some E<sol> or other E<>, no change
                         t) ;; keep looping

                        ((and (match-beginning 1) ;; is E<gt>
                              (memq (char-before (match-beginning 0))
                                    '(?I ?B ?C ?L ?F ?S ?X)))
                         ;; Have E<lt> after IBCLFSX.  Don't change E<lt> to
                         ;; "<" as that would make a markup.
                         t) ;; keep looping

                        ((and (match-beginning 2) ;; is E<gt>
                              (eq (char-before (match-beginning 0))
                                  ?>))
                         ;; Have E<gt> after another ">".  Don't change
                         ;; E<gt> to ">" as it would look unclear.
                         t) ;; keep looping

                        ((> (- (match-end 0) (match-beginning 0)) 5)
                         ;; Have run of more than one E<lt> or E<gt>.  Don't
                         ;; change them as don't want to look like closing
                         ;; ">>".
                         t) ;; keep looping

                        (t
                         (replace-match (if (match-beginning 1)
                                            "<"  ;; E<lt> becomes <
                                          ">")   ;; E<gt> becomes >
                                        t t)
                         t))))))) ;; keep looping

;;;###autoload
(defun perl-pod-gt-single ()
  "Convert C<< >> style markup at point to C<>.
See also Pod::Plainer which does a similar downgrade of C<<>> on
a whole document."
  (interactive)
  (let ((case-fold-search nil))
    (save-excursion
      (let ((orig-point (point)))
        (unless (and (or (perl-pod-gt-find-markup)
                         ;; not inside a C<> form, but possibly on the C or <
                         (progn
                           (goto-char orig-point)
                           (looking-at "[IBCLFSX]<+")))
                     (looking-at ".\\(<+\\) "))
          (error "Not in a C<<...>> etc form")))

      (let ((re (match-string 1)))  ;; the "<<" or "<<<" etc
        (unless (= (length re) 1)
          ;; no good for emacs20 here, no .*? non-greedy or \{\} reps
          (setq re (format "\\(%s \\).*?\\( ?\\(>\\|\\'\\)\\{%d\\}\\)"
                           re (length re)))
          (goto-char (1+ (point)))
          (or (looking-at re)
              (error "Oops, re-match of <<...>> failed"))

          (save-restriction
            (narrow-to-region (match-beginning 0) (match-end 0))

            ;; " >>" becomes ">"
            (replace-match ">" t t nil 2)

            ;; "<< " becomes "<"
            (replace-match "<" t t nil 1)

            ;; "<" and ">" become "E<lt>" and "E<gt>"
            (while (re-search-forward "[<>]" (1- (point-max)) t)
              (replace-match (cdr (assoc (match-string 0) '(("<" . "E<lt>")
                                                            (">" . "E<gt>"))))
                             t t)))))))) ;; fixedcase, literal


;;-----------------------------------------------------------------------------
;; warning overlays

(defface perl-pod-gt-warn
  '((((class color))
     (:background "red"))
    (t
     (:inverse-video t)))
  "Face for warning of bad Perl pod bits.
The default is the same as `trailing-whitespace' face, which is
red background on a colour screen, or inverse video on black and
white."
  :group 'faces ;; in absence of a better place
  :link  '(url-link :tag "perl-pod-gt.el home page"
                    "http://user42.tuxfamily.org/perl-pod-gt/index.html"))

(defun perl-pod-gt-warn-after-change (beg end prev-len)
  ;; checkdoc-params: (beg end prev-len)
  "Put a warning face on things between BEG and END.
This function is designed for use in `after-change-functions', as
set up by `perl-pod-gt-warn-enable'.

If BEG and END are in the middle of a line or lines then the face
is updated on the whole of those lines, since a change to a
starting C< etc may affect warnings required on some or all of
the rest of the line."

  (save-match-data
    (save-excursion
      ;; Extend to whole lines.
      ;;
      (goto-char beg)
      (setq beg (line-beginning-position))
      (goto-char end)
      (setq end (line-end-position))

      ;; Remove existing overlays in case
      ;;   - offending bits are now ok
      ;;   - or offending bits have been deleted leaving a zero-length overlay
      ;;   - and so as not to add multiple overlays onto unchanged bits
      ;;
      ;; emacs21 and xemacs21 don't have `remove-overlays' (new in emacs22),
      ;; but this is not the same anyway.  `remove-overlays' splits overlays
      ;; crossing the endpoints whereas here want to delete the lot.
      ;;
      (dolist (overlay (overlays-in beg end))
        (if (eq 'perl-pod-gt-warn (overlay-get overlay 'face))
            (delete-overlay overlay)))

      (let ((case-fold-search nil))
        (dolist (elem '(
;; "->" in the middle of C<> etc, but restricted following chars so
;;   C<$var-->          ok
;;   C<$var->method>    bad
;;   C<$var->{field}>   bad
;;   C<$var->[123]>     bad
;;   C<$var->()>        bad
(3 "[IBCLFSX]<\\([^<>]\\|E+<[a-z]+>\\)\\([^>]\\|E+<[a-z]+>\\)*\\(->\\)[A-Za-z_{[(]")

;; "=>" in the middle of C<> etc, like C<abc=>def>
;; Not in C<<.
;; Not at start, so C<=> is ok.
;; Exceptions for operators C<=>, C<+=>, C<//=>, etc
(2 "[IBCLFSX]<\\([^>]\\|E+<[a-z]+>\\)*\\(=>\\)"
   ".<<\\|[IBCS]<\\([-=+*/%!&|^x.]?\\|[/&|*]\\{2\\}\\|E<lt>E<lt>\\|E<gt>E<gt>\\)=>")

;; no space after C<<, or C<<<, etc
(1 "\\([IBCLFSX]<<+\\)[^< \t\r\n]")

;; no space before >> in a C<<...>>
(2 "[IBCLFSX]<<[^<]\\([^>]\\|>[^>]\\)*?[^ \t\r\n]\\(>>+\\)")

;; no space before >>> in a C<<<...>>>
(3 "[IBCLFSX]<<<[^<]\\([^>]\\|>\\([^>]\\|>[^>]\\)\\)*?[^ \t\r\n]\\(>>>+\\)")
))
          (let ((group  (nth 0 elem))
                (re     (nth 1 elem))
                (except (nth 2 elem)))
            (goto-char beg)
            (while (re-search-forward re end t)
              (let ((beg (match-beginning group))
                    (end (match-end group)))
                (unless (and except
                             (string-match except (match-string 0)))
                  (let ((overlay (make-overlay beg end (current-buffer)
                                               nil nil)))
                    (overlay-put overlay 'face 'perl-pod-gt-warn)))))))))))

(defun perl-pod-gt-warn-enable ()
  "Enable perl-pod-gt warning overlays in the current major mode.
This is done by the main `perl-pod-gt-enable' but can be used
alone."
  (perl-pod-gt-warn-after-change (point-min) (point-max) 0) ;; initial
  (if (eval-when-compile (fboundp 'make-local-hook))
      (make-local-hook 'after-change-functions)) ;; for xemacs21
  (add-hook 'after-change-functions
            'perl-pod-gt-warn-after-change
            t   ;; append
            t)) ;; buffer-local


;;-----------------------------------------------------------------------------

;;;###autoload
(defun perl-pod-gt-enable ()
  "Enable the features of `perl-pod-gt' in the current major mode.
This is designed for use from a hook like `perl-mode-hook',
`cperl-mode-hook' or `pod-mode-hook'.  It does the following,

* Binds the < and > keys to `perl-pod-lt-insert' and
  `perl-pod-gt-insert' in the major mode map (`current-local-map').

* Adds `perl-pod-gt-nobreak-p' to `fill-nobreak-predicate' for
  paragraph filling (buffer-local).

* Sets up warning face overlay on certain -> and similar probable
  errors within C<>, B<> etc markup (`perl-pod-gt-warn-enable').

The nobreak is skipped for XEmacs 21.4 where there's no
`fill-nobreak-predicate' mechanism.

No bindings are made for `perl-pod-gt-double' or
`perl-pod-gt-single', you can do that yourself if `M-x' is too
much."

  (interactive)
  (define-key (current-local-map) "<" 'perl-pod-lt-insert)
  (define-key (current-local-map) ">" 'perl-pod-gt-insert)
  (perl-pod-gt-warn-enable)

  (cond ((not (boundp 'fill-nobreak-predicate))
         ;; no such feature at all in xemacs 21, but don't lock this down
         ;; with an eval-when-compile in case there's an add-on creating
         ;; something compatible
         )

        ;; emacs22 fill-nobreak-predicate is a hook, add to it buffer-local
        ((get 'fill-nobreak-predicate 'custom-type)
         (add-hook 'fill-nobreak-predicate 'perl-pod-gt-nobreak-p nil t))

        ;; emacs20,21 fill-nobreak-predicate is a variable holding a
        ;; function if no existing value then plonk our function in
        ((not fill-nobreak-predicate)
         (set (make-local-variable 'fill-nobreak-predicate)
              'perl-pod-gt-nobreak-p))
        ;; If an existing value then append to it by making a lambda -- this
        ;; is fairly nasty.  Maybe use nobreak-fade.el instead.
        (t
         (set (make-local-variable 'fill-nobreak-predicate)
              `(lambda ()
                 (or (perl-pod-gt-nobreak-p)
                     (,fill-nobreak-predicate)))))))

;; In principle could add perl-pod-gt-nobreak-p as a customize option for
;; fill-nobreak-predicate (in emacs 22 where that variable is a hook).  But
;; it's fairly Perl specific and so unlikely to be wanted globally.

;;;###autoload
(custom-add-option 'perl-mode-hook  'perl-pod-gt-enable)
;;;###autoload
(custom-add-option 'cperl-mode-hook 'perl-pod-gt-enable)
;;;###autoload
(custom-add-option 'pod-mode-hook   'perl-pod-gt-enable)

;; LocalWords: el lt gt versa formatters perl whitespace nroff troff nobreak
;; LocalWords: nobreaks

(provide 'perl-pod-gt)

;;; perl-pod-gt.el ends here
