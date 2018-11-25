;;; perl-pod-preview.el --- preview perl pod documentation

;; Copyright 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016 Kevin Ryde
;;
;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 18
;; Keywords: languages, perl
;; URL: http://user42.tuxfamily.org/perl-pod-preview/index.html
;; EmacsWiki: PerlPodPreview

;; perl-pod-preview.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; perl-pod-preview.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


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

;;; Emacsen:
;;
;; Designed for Emacs 20 up, works in XEmacs 21.

;;; Install:

;; Put perl-pod-preview.el in one of your `load-path' directories, and in
;; your .emacs add
;;
;;     (autoload 'perl-pod-preview "perl-pod-preview" nil t)
;;
;; This makes M-x perl-pod-preview available, or you might like to bind it
;; to a key, for example f8 in cperl-mode,
;;
;;     (eval-after-load "cperl-mode"
;;       '(define-key cperl-mode-map [f8] 'perl-pod-preview))

;;; History:

;; Version 1 - the first version
;; Version 2 - copy default-directory from the source buffer
;; Version 3 - put perl 5.10 "POD ERRORS" section in the errors buffer
;; Version 4 - next-error in any buffer, eg. a tar file
;;           - make nroff errors a little clearer
;; Version 5 - fix for repeating a preview which has errors
;; Version 6 - save intermediate roff to show man errors
;; Version 7 - xemacs doesn't have no-record on switch-to-buffer-other-window
;; Version 8 - cope with non-existent `default-directory'
;; Version 9 - delete errors window when no errors
;; Version 10 - oops, `delete-windows-on' needs arg until emacs23
;; Version 11 - undo defadvice on unload-feature
;; Version 12 - express dependency on 'advice
;; Version 13 - revert-buffer and auto-revert-mode in the preview buffer
;;            - use woman-mode if no man, as proposed by Jeremy Braun
;; Version 14 - pod2man -name for xemacs tar-mode subfiles
;; Version 15 - perl 5.18 pod2man IO::File for stdin in error messages
;; Version 16 - kill error buffer too when kill preview buffer
;; Version 17 - new email
;; Version 18 - don't use second,third macros

;;; Code:

;; Explicit dependency on advice.el since `perl-pod-preview-unload-function'
;; needs `ad-find-advice' macro when running not byte compiled, and that
;; macro is not autoloaded.
(require 'advice)

(eval-when-compile
  (unless (and (fboundp 'declare)
               (fboundp 'ignore-errors)
               (fboundp 'dolist))
    (require 'cl))) ;; for emacs20

(defvar manual-program) ;; in man.el


;;-----------------------------------------------------------------------------

(defconst perl-pod-preview-buffer "*perl-pod-preview*"
  "The name of the buffer for `perl-pod-preview' output.")

(defconst perl-pod-preview-error-buffer "*perl-pod-preview-errors*"
  "The name of the buffer for `perl-pod-preview' error messages.")

(defconst perl-pod-preview-roff-buffer " *perl-pod-preview-roff*"
  "The name of the buffer for `perl-pod-preview' intermediate nroff.
This is normally only of interest if there's errors from \"man\".")

(defvar perl-pod-preview-origin nil
  "The input buffer being displayed in `perl-pod-preview-buffer'.")
(defvar perl-pod-preview-origin-tick
  "The modification tick from `perl-pod-preview-buffer'.
This is used by `perl-pod-preview-stale'.")

;;-----------------------------------------------------------------------------
;; `make-temp-file' new in emacs21, not in xemacs21

(cond ((or (eval-when-compile (fboundp 'make-temp-file))
           (fboundp 'make-temp-file))
       ;; emacs21 up, noticed at compile time or run time
       (eval-and-compile
         (defalias 'perl-pod-preview--make-temp-file 'make-temp-file)))

      ((locate-library "mm-util") ;; from gnus
       ;; xemacs21
       (autoload 'mm-make-temp-file "mm-util")
       (defalias 'perl-pod-preview--make-temp-file 'mm-make-temp-file))

      ((locate-library "poe") ;; from APEL
       ;; emacs20 with poe.el add-on
       (require 'poe)
       (defalias 'perl-pod-preview--make-temp-file 'make-temp-file))

      (t
       ;; umm, dunno, hope the user can define it
       (message "perl-pod-preview.el: don't know where to get `make-temp-file'")
       (defalias 'perl-pod-preview--make-temp-file 'make-temp-file)))


;;-----------------------------------------------------------------------------
;; `buffer-chars-modified-tick' new in emacs22

(eval-and-compile ;; quieten the byte compiler
  (defalias 'perl-pod-preview--buffer-chars-modified-tick
    (if (fboundp 'buffer-chars-modified-tick)
        'buffer-chars-modified-tick   ;; emacs22
      'buffer-modified-tick)))        ;; emacs21,xemacs21


;;-----------------------------------------------------------------------------
;; generic stuff

(eval-when-compile
  (defmacro perl-pod-preview--with-errorfile (&rest body)
    "An internal part of perl-pod-preview.el.
This macro does not exist when running byte compiled.

Create an `errorfile' for use by the BODY forms.
An `unwind-protect' ensures the file is removed no matter what
BODY does."
    (declare (debug t))  ;; emacs22,xemacs21, or 'cl
    `(let ((errorfile (perl-pod-preview--make-temp-file "perl-pod-preview-")))
       (unwind-protect
           (progn ,@body)
         (delete-file errorfile)))))

(defun perl-pod-preview--switch-to-buffer-other-norecord (buffer)
  ;; checkdoc-params: (buffer)
  "An internal part of perl-pod-preview.el.
`switch-to-buffer-other-window' with no-record when available.
Emacs has a no-record arg, XEmacs doesn't."
  (condition-case nil
      ;; emacs
      (switch-to-buffer-other-window buffer t) ;; no-record
    (error
     ;; xemacs
     (switch-to-buffer-other-window buffer))))


;;-----------------------------------------------------------------------------
;; window-point save/restore

(defun perl-pod-preview--linecol (point)
  "Return a pair (LINE . COLUMN) for POINT in the current buffer.
LINE counts from 0 for the first line.
COLUMN counts from 0 for the first column."
  (save-excursion
    (goto-char point)
    (cons (count-lines (point-min) (line-beginning-position))
          (current-column))))
  
(defun perl-pod-preview--goto-linecol (linecol)
  "Move point to LINECOL in the current buffer.
LINECOL is a pair (LINE . COLUMN), starting from (0 . 0) for the
first line and first column."
  (goto-char (point-min))
  (forward-line (car linecol)) ;; line
  (move-to-column (cdr linecol)))

(defun perl-pod-preview--wpoints-get (buffer)
  "Return a list of point and start positions for BUFFER.
Currently the return is

    (((WINDOW (START-LINE . START-COL) (POINT-LINE . POINT-COL))
      (WINDOW (START-LINE . START-COL) (POINT-LINE . POINT-COL))
      ...)
     .
     (LINE . COL))

The list is `window-start' and `window-point' positions for each
window currently displaying BUFFER, followed by the plain `point'
line/column.  As usual if one of the windows is the selected
window in the selected frame then plain `point' is the same as
the `window-point' there, otherwise plain `point' is separate."

  (and (get-buffer buffer)
       (with-current-buffer buffer
         (cons (mapcar (lambda (window)
                         (list window
                               (perl-pod-preview--linecol
                                (window-start window))
                               (perl-pod-preview--linecol
                                (window-point window))))
                       (get-buffer-window-list buffer
                                               t   ;; include minibuffer
                                               t)) ;; all frames
               (perl-pod-preview--linecol (point))))))
(eval-when-compile
  (put 'perl-pod-preview--wpoints-get 'side-effect-free t))

(defun perl-pod-preview--wpoints-set (buffer wpoints)
  "Set a window point and start positions of BUFFER from WPOINTS.
WPOINTS should be as returned by
`perl-pod-preview--wpoints-get'."

  (when (and wpoints
             (buffer-live-p buffer))
    (with-current-buffer buffer
      (dolist (window (get-buffer-window-list buffer
                                              t   ;; include minibuffer
                                              t)) ;; all frames
        (let ((elem (assoc window (car wpoints))))
          (if elem
              (progn
                (perl-pod-preview--goto-linecol (nth 1 elem))
                ;; don't let window-start be the very end of the buffer, since
                ;; that would leave it completely blank
                (if (= (point) (point-max))
                    (forward-line -1))
                (set-window-start window (point))

                (perl-pod-preview--goto-linecol (nth 2 elem))
                (set-window-point window (point)))

            ;; new windows are "restored" to the the buffer point and centre
            ;; of the screen
            (perl-pod-preview--goto-linecol (cdr wpoints))
            (set-window-point window (point))
            (save-selected-window
              (select-window window)
              (recenter nil)))))

      (perl-pod-preview--goto-linecol (cdr wpoints)))))

(eval-when-compile
  (defmacro perl-pod-preview--save-wpoints (&rest body)
    "An internal part of perl-pod-preview.el.
This macro does not exist when running byte compiled.

Save `window-start' and point positions as line/column.
The use of line/column means BODY can erase and rewrite the
buffer contents."

    (declare (debug t))  ;; emacs22,xemacs21, or 'cl
    `(let ((buffer  (current-buffer))
           (wpoints (perl-pod-preview--wpoints-get (current-buffer))))
       ,@body
       (perl-pod-preview--wpoints-set buffer wpoints))))


;;-----------------------------------------------------------------------------
;; man program availability

(defvar perl-pod-preview-have-man-p 'undecided
  "nil or t according to whether `manual-program' works.
This is an internal part of perl-pod-preview.el, used to cache
the test in function `perl-pod-preview-have-man-p'.  When that
function has not yet run the value is symbol `undecided'.")

(defun perl-pod-preview-have-man-p ()
  "Return non-nil if `manual-program' works.
This is an internal part of perl-pod-preview.el."
  (when (eq perl-pod-preview-have-man-p 'undecided)
    (require 'man)
    (setq perl-pod-preview-have-man-p
          (ignore-errors
            (equal 0 (call-process manual-program
                                   nil  ;; stdin
                                   nil  ;; stdout
                                   nil  ;; redisplay
                                   "--version")))))
  perl-pod-preview-have-man-p)

(defun perl-pod-preview-have-woman-p ()
  "Return non-nil if woman.el is available.
This is an internal part of perl-pod-preview.el."
  (or (featurep 'woman)
      (locate-library "woman")))


;;-----------------------------------------------------------------------------

;;;###autoload
(defun perl-pod-preview ()
  "Preview Perl POD documentation in the current buffer.
The buffer can be Perl code with embedded POD, or a .pod file.
It's put through the \"pod2man\" command then displayed with
\"man\" plus `Man-mode', or `woman-mode' if no man.

Errors from pod2man are shown in a `compilation-mode' buffer and
the usual `next-error' (\\[next-error]) steps through offending parts of
the source.

Errors from man are shown too, as \"<pod2man output>\", and
`next-error' goes to a temporary buffer with the pod2man
intermediate output.  The most common error is a word, URL, etc
too long to wrap and the temp buffer lets you see what it is,
though not where in the original POD.

If you re-run `perl-pod-preview' from the source buffer then the
position in the preview buffer is preserved.  So if you switch
back to the source, make a small change, and re-preview, then it
should still be close to the old position to see how it looks.

The source buffer is not saved for the preview and doesn't even
have to have a file.  This means you can preview formatted POD
out of a `tar-mode' member or similar \"virtual\" buffer.

The man program is taken from `manual-program' (like `Man-mode'
does).  If man is not available then `woman-mode' is used, if you
have woman.el.  Its version 0.551beta might not quite understand
some of the macro things pod2man emits.

In the *perl-pod-preview* output buffer `revert-buffer' re-runs
the preview from its originating buffer.  `auto-revert-mode' can
be used to do that automatically while you edit the source.

------
Non-ASCII handling isn't very good, mainly because pod2man is
conservative about what it spits out.  It'd be possible to use
another formatter, but pod2man is how people will see your docs,
so checking what comes out of it is no bad thing.  For reference
the non-ascii situation is roughly as follows,

* In Perl 5.8 and earlier, pod2man didn't accept \"=encoding\" so
  you're probably limited to ascii input there, although latin-1
  generally gets through pod2man and groff unmolested.

* In Perl 5.10, pod2man recognises \"=encoding\" but only turns a
  few latin-1 accented characters into roff forms, and everything
  else to \"X\" characters.  But alas as of groff 1.18 the inking
  used by pod2man for accented forms doesn't come out properly on
  a tty, so you end up with unaccented ascii.

------
See also `cperl-pod-to-manpage'.  As of cperl-mode.el 6.2 it
works from a disk copy of a buffer so can't run from `tar-mode'
members etc.  It generates a new buffer on each run.

------
The perl-pod-preview home page is
URL `http://user42.tuxfamily.org/perl-pod-preview/index.html'"

  (interactive)
  (let ((origin-buffer (current-buffer)))
    (switch-to-buffer perl-pod-preview-buffer)
    (perl-pod-preview-1 origin-buffer t)))

(defun perl-pod-preview-revert (&optional ignore-auto noconfirm)
  ;; checkdoc-params: (ignore-auto noconfirm)
  "Re-run `perl-pod-preview' to update a preview buffer.
This is the `revert-buffer-function' for the
`perl-pod-preview-buffer' buffer.  It's called with that buffer
as the current buffer."
  (perl-pod-preview-1 perl-pod-preview-origin nil))

(defun perl-pod-preview-1 (origin-buffer show-error-window)
  ;; checkdoc-params: (origin-buffer show-error-window)
  "An internal part of perl-pod-preview.el."

  ;; Clear existing `perl-pod-preview-error-buffer'.
  ;; Turn off any compilation-mode there so it won't attempt to parse
  ;; the contents until later when they've been variously munged.
  (with-current-buffer (get-buffer-create perl-pod-preview-error-buffer)
    (fundamental-mode)
    (setq buffer-read-only nil)
    (erase-buffer))

  ;; kill errors when kill preview
  (if (eval-when-compile (fboundp 'make-local-hook))
      (make-local-hook 'kill-buffer-hook)) ;; for xemacs21
  (add-hook 'kill-buffer-hook 'perl-pod-preview-kill-error-buffer
            nil ;; not append
            t)  ;; buffer-local

  (setq buffer-read-only nil)

  ;; If previewing a different buffer then erase here so as not to
  ;; restore point+window position into a completely different document.
  (if (not (equal perl-pod-preview-origin origin-buffer))
      (erase-buffer))
  (setq perl-pod-preview-origin origin-buffer)
  (setq perl-pod-preview-origin-tick
        (perl-pod-preview--buffer-chars-modified-tick origin-buffer))

  ;; default-directory set from origin-buffer, so find-file or whatever
  ;; offers the same default as there.  This is inherited on initial
  ;; creation of the preview buffer, but must be set explicitly when
  ;; previewing a different buffer.
  (setq default-directory (with-current-buffer origin-buffer
                            default-directory))

  (perl-pod-preview--with-errorfile
   (perl-pod-preview--save-wpoints
    (erase-buffer)

    ;; Coding for the input to pod2man is just the origin buffer
    ;; buffer-file-coding-system, since that's the bytes it would get from
    ;; the file.
    ;;
    ;; Coding for the output from pod2man is not quite clear.  The bytes
    ;; ought to be something "man" understands, which probably ought to
    ;; mean ascii-only (anything extra as troff directives).  Groff
    ;; (version 1.18) is usually happy with latin-1 input, and the
    ;; "man-db" man (version 2.5 at least) will actually try to guess the
    ;; input charset and convert to latin-1 (or something) for nroff/groff
    ;; as part of its preprocessing pipeline.
    ;;
    ;; In perl 5.8 pod2man didn't pay attention to non-ascii at all, it
    ;; just seemed to pass bytes through (and in particular gave an error
    ;; for a pod "=encoding" directive).  So it's anyone's guess what
    ;; encoding you'd be supposed to read from there.  Go latin-1 on the
    ;; slightly rash assumption that its most likely, and is what we feed
    ;; to "man -Tlatin1", and even if it's not true the bytes will get
    ;; through a latin-1 decode/encode to reach man unchanged.
    ;;
    ;; In perl 5.10 pod2man maybe probably hopefully puts out ascii-only,
    ;; having converted other input chars into roff sequences or expressions
    ;; (and falling back on "X" for unknown chars).  So nothing special
    ;; should be needed on the read coding for that.
    ;;
    (if (get-buffer perl-pod-preview-roff-buffer)
        (kill-buffer perl-pod-preview-roff-buffer)) ;; erase
    (with-current-buffer perl-pod-preview-origin
      (let ((coding-system-for-write buffer-file-coding-system)
            (coding-system-for-read  'iso-8859-1)
            (default-directory       "/")
            (process-connection-type nil)) ;; pipe
        (apply 'call-process-region
               (point-min) (point-max) "pod2man"
               nil ;; keep input
               (list perl-pod-preview-roff-buffer  ;; output
                     errorfile)
               nil ;; don't redisplay
               (perl-pod-preview-pod2man-args))))

    ;; Perl 5.8 pod2man prints errors to stderr, and turn "<standard
    ;; input>" filename into "<perl-pod-preview>" ready for the
    ;; `compilation-find-file' defadvice below.
    ;; Perl 5.18.1 pod2man prints IO::File=IO(0x8434be4) or similar, so
    ;; crunch that too.
    (with-current-buffer perl-pod-preview-error-buffer
      (insert-file-contents errorfile)
      (goto-char (point-min))
      (while (re-search-forward "<standard input>\\|IO::File=IO([^)]+)" nil t)
        (replace-match "<perl-pod-preview>" t t)))

    ;; Running man with "-Tlatin1" makes it print overstrikes and
    ;; underscores for bold and italics, which `Man-fontify-manpage' below
    ;; crunches into fontification.
    ;;
    ;; "-Tutf8" output would also be possible, but for now its only effect
    ;; is to make unicode hyphens and other stuff that doesn't display on
    ;; a latin1 tty.  In the future if pod2man put extended characters
    ;; through in way groff understood then probably would want -Tutf8 so
    ;; as to see those.
    ;;
    ;; For man-db (version 2.5 at least) a side-effect of either of those
    ;; -T options is to lose input charset guessing (its "manconv"
    ;; program).  This is why "pod2man my-utf8.pod | man -l -" gives
    ;; sensibly formatted output (though with "?" marks for undisplayable
    ;; chars) whereas with -T here it's garbage and errors on such a
    ;; ".pod".
    ;;
    (cond ((and (not (perl-pod-preview-have-man-p))
                (perl-pod-preview-have-woman-p))
           (require 'woman)
           (insert (with-current-buffer perl-pod-preview-roff-buffer
                     (buffer-string)))
           (woman-process-buffer))

          (t
           (require 'man) ;; for `manual-program'
           (with-current-buffer perl-pod-preview-roff-buffer
             (let ((coding-system-for-write 'iso-8859-1)
                   (coding-system-for-read  'iso-8859-1)
                   (default-directory       "/")
                   (process-connection-type nil)) ;; pipe
               (call-process-region (point-min) (point-max)
                                    manual-program   ;; program name
                                    nil              ;; don't delete input
                                    (list perl-pod-preview-buffer ;; stdout
                                          errorfile)              ;; stderr
                                    nil              ;; don't redisplay
                                    ;; program arguments
                                    "-Tlatin1" "-l" "-")))

           ;; "man" normal output, in particular crunch the backspace
           ;; overstriking before looking for "POD ERRORS" section
           (if (eval-when-compile (fboundp 'Man-mode))
               ;; emacs21 and emacs22
               (progn
                 (Man-fontify-manpage)
                 (Man-mode))
             ;; xemacs21
             (Manual-nuke-nroff-bs)
             (Manual-mode))))

    (set (make-local-variable 'revert-buffer-function)
         'perl-pod-preview-revert)
    (set (make-local-variable 'buffer-stale-function)
         'perl-pod-preview-stale)
    (set-buffer-modified-p nil)

    ;; Perl 5.10 pod2man prints errors in a "POD ERRORS" section of
    ;; the normal output.
    (let ((errstr (perl-pod-preview-extract-pod-errors
                   "<perl-pod-preview>")))
      (if errstr
          (with-current-buffer perl-pod-preview-error-buffer
            (goto-char (point-max))
            (insert errstr))))

    ;; "man" errors, with "<standard input>" changed to "<pod2man output>"
    ;; ready for the `compilation-find-file' defadvice below
    (with-current-buffer perl-pod-preview-error-buffer
      (goto-char (point-max))
      (save-excursion
        (insert-file-contents errorfile))
      (while (search-forward "<standard input>" nil t)
        (replace-match "<pod2man output>" t t)))

    ;; show errors in a window, but only if there are any
    (save-selected-window
      (with-current-buffer perl-pod-preview-error-buffer
        (if (= (point-min) (point-max))
            (progn
              ;; No errors, kill buffer and window.  Killing the window
              ;; prevents something unrelated showing in a small window
              ;; which can be annoying for buffer cycling etc.
              (delete-windows-on (current-buffer))
              (kill-buffer nil))

          ;; emacs21 ignores the first two lines of a compilation-mode
          ;; buffer, so add in dummies
          (goto-char (point-min))
          (insert "perl-pod-preview pod2man\n\n")

          ;; switch to display, and if it it's newly displayed then shrink
          ;; to what's needed if there's only a couple of lines of errors
          (let ((existing-window (get-buffer-window (current-buffer))))
            (when show-error-window
              (perl-pod-preview--switch-to-buffer-other-norecord (current-buffer)))
            (if (not existing-window)
                (shrink-window-if-larger-than-buffer
                 (get-buffer-window (current-buffer)))))
          (compilation-mode)))))))

(defun perl-pod-preview-pod2man-args ()
  "Return a list of arguments for the pod2man program.
This is an internal part of `perl-pod-preview'.

Currently this consists of a \"-name\" argument which resembles
the name pod2man would have shown if it were run on the actual
buffer filename instead of fed from stdin.  This name is shown by
pod2man in the output header and footer."

  (list "-name"
        (upcase
         (file-name-nondirectory
          (file-name-sans-extension
           (cond (buffer-file-name
                  buffer-file-name)

                 ;; XEmacs 21.4 tar-mode sub-file has buffer-file-name nil, but
                 ;; does have buffer-name like "Foo.pm (in Foo-1.tar.gz)", pick
                 ;; out the basename from there.
                 ;;
                 ((string-match " (in " (buffer-name))
                  (substring (buffer-name)
                             0 (match-beginning 0)))
                 (t
                  (buffer-name))))))))

(defun perl-pod-preview-extract-pod-errors (filename)
  "Return a string of errors from a POD ERRORS in the current buffer.
If there's no POD ERRORS section then return nil.

Perl 5.10 pod2man (or rather Pod::Simple) emits a \"POD ERRORS\"
section for problems it finds in the input.  That section is
extracted here and each \"Around line N\" gets a GNU style
\"filename:linenum:colnum:\" inserted for the benefit of the
standard `compilation-mode' matching.  The FILENAME arg is used
for the originating file to show."

  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (when (search-forward "POD ERRORS\n" nil t)
        (let ((beg (match-beginning 0)))

          ;; "POD ERRORS" section extends to next heading, which means an
          ;; unindented line, ie. beginning with a non-space, or if no more
          ;; headings then the end of the buffer
          (let ((str (buffer-substring-no-properties
                      beg
                      (or (and (re-search-forward
                                "^[^ \t\r\n]" nil t)
                               (match-beginning 0))
                          (point-max)))))
            (with-temp-buffer
              (insert str)

              ;; delete trailing multiple newlines
              (goto-char (point-min))
              (if (re-search-forward "\n+\\'" nil t)
                  (replace-match "\n" t t))

              ;; mung "Around line" to something the standard
              ;; compilation-mode regexps can match
              (goto-char (point-min))
              (while (re-search-forward "^[ \t]+Around line \\([0-9]+\\):"
                                        nil t)
                (save-excursion
                  (goto-char (match-beginning 0))
                  ;; Insert a dummy column number for xemacs21.
                  ;; (The patterns in emacs21 can match line number alone.)
                  (insert filename ":" (match-string 1) ":0:\n")))

              (buffer-string))))))))
      
(defadvice compilation-find-file (around perl-pod-preview activate)
  "Use `perl-pod-preview-origin' for pod2man errors."
  ;; args: (compilation-find-file MARKER FILENAME DIRECTORY &rest FORMATS)
  (cond ((and (boundp 'perl-pod-preview-origin)
              (buffer-live-p perl-pod-preview-origin)
              (equal "<perl-pod-preview>" (ad-get-arg 1))) ;; FILENAME
         (setq ad-return-value perl-pod-preview-origin))
        ((and (boundp 'perl-pod-preview-roff-buffer)
              (buffer-live-p (get-buffer perl-pod-preview-roff-buffer))
              (equal "<pod2man output>" (ad-get-arg 1))) ;; FILENAME
         (setq ad-return-value perl-pod-preview-roff-buffer))
        (t
         ad-do-it)))

(defun perl-pod-preview-stale (&optional noconfirm)
  ;; checkdoc-params: (noconfirm)
  "Return non-nil if the `perl-pod-preview' output is stale.
This is the `buffer-stale-function' for the
`perl-pod-preview-buffer' buffer.  It's called with that buffer
as the current buffer.

If the originating buffer has changed, per its
`buffer-chars-modified-tick', then the output is considered
stale."

  (and perl-pod-preview-origin
       (not (equal perl-pod-preview-origin-tick
                   (perl-pod-preview--buffer-chars-modified-tick
                    perl-pod-preview-origin)))))

(defun perl-pod-preview-kill-error-buffer ()
  "Kill `perl-pod-preview-error-buffer' and its window.
This is an internal part of perl-pod-preview.el.
It's used when the `perl-pod-preview-buffer' is killed to remove
any error display too."
  (let ((buffer (get-buffer perl-pod-preview-error-buffer)))
    (when (buffer-live-p buffer)
      (delete-windows-on buffer)
      (kill-buffer buffer))))

(defun perl-pod-preview-unload-function ()
  "Remove advice from `compilation-find-file'.
This is called by `unload-feature'."
  (when (ad-find-advice 'compilation-find-file 'around 'perl-pod-preview)
    (ad-remove-advice   'compilation-find-file 'around 'perl-pod-preview)
    (ad-activate        'compilation-find-file))
  nil) ;; and do normal unload-feature actions too

;; LocalWords: inlined manpage docstring latin roff groff nroff linenum
;; LocalWords: colnum formatter ascii tty arg filename el WoMan

(provide 'perl-pod-preview)

;;; perl-pod-preview.el ends here
