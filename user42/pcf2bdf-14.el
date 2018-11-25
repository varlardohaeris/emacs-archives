;;; pcf2bdf.el --- view .pcf compiled font files as bdf

;; Copyright 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015 Kevin Ryde

;; Author: Kevin Ryde <user42@zip.com.au>
;; Version: 14
;; Keywords: data, font, pcf, bdf
;; EmacsWiki: Pcf2Bdf
;; URL: http://user42.tuxfamily.org/pcf2bdf/index.html

;; pcf2bdf.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; pcf2bdf.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a bit of fun running the pcf2bdf program on .pcf font files to
;; see bdf source.  The main aim is to see sizes, comments, copyright
;; notice, etc, but cute use of `format-alist' in fact allows saving, though
;; you almost certainly won't want to edit a font that way.
;;
;; See the `pcf2bdf' function docstring for more.

;;; Install:

;; To make M-x pcf2bdf available, put pcf2bdf.el in one of your `load-path'
;; directories and the following in your .emacs
;;
;;     (autoload 'pcf2bdf "pcf2bdf" nil t)
;;     (modify-coding-system-alist 'file "\\.pcf\\'" 'raw-text-unix)
;;
;; To use pcf2bdf automatically on pcf files, which is the suggested usage,
;; also put
;;
;;     (add-to-list 'auto-mode-alist '("\\.pcf\\'" . pcf2bdf))
;;
;; There's autoload tags below for this, if you use `update-file-autoloads'
;; and friends.

;;; History:

;; Version 1 - the first version
;; Version 2 - use some eval-when-compile
;; Version 3 - xemacs21 eval-after-load only takes a string
;; Version 4 - use pipe rather than pty for subprocess
;; Version 5 - workaround emacs23 write-region-post-annotation-function
;;           - improved error display and next-error handling
;; Version 6 - undo defadvice on unload-feature
;; Version 7 - better write-region-post-annotation-function
;; Version 8 - express dependency on 'advice
;; Version 9 - compilation-find-file args by number, so not depend on names
;; Version 10 - emacs20 try make-temp-file from APEL
;; Version 11 - don't delete bytes if no pcf2bdf program
;; Version 12 - tweak make-temp-file fallback for emacs20
;; Version 13 - don't demand apel poe.el when byte compiling
;; Version 14 - require 'compile for variables when byte compiling

;;; Emacsen:

;; Designed for Emacs 21 and up, works in XEmacs 21.
;; Believe works in Emacs 20.

;;; Code:

;; Explicit dependency on advice.el since `pcf2bdf-unload-function' needs
;; `ad-find-advice' macro when running not byte compiled, and that macro is
;; not autoloaded.
(require 'advice)

;;;###autoload
(modify-coding-system-alist 'file "\\.pcf\\'" 'raw-text-unix)


;;-----------------------------------------------------------------------------
;; compatibility

;; `make-temp-file' new in emacs21, not in xemacs21
(cond ((or (eval-when-compile (fboundp 'make-temp-file))
           (fboundp 'make-temp-file))
       ;; emacs21 up, noticed at compile time or run time
       (eval-and-compile
         (defalias 'pcf2bdf--make-temp-file 'make-temp-file)))

      ((locate-library "mm-util") ;; from gnus
       ;; xemacs21
       (eval-and-compile
         (autoload 'mm-make-temp-file "mm-util")
         (defalias 'pcf2bdf--make-temp-file 'mm-make-temp-file)))

      ((locate-library "poe") ;; from APEL
       ;; emacs20 with poe.el add-on
       (require 'poe)
       (eval-and-compile
         (defalias 'pcf2bdf--make-temp-file 'make-temp-file)))

      (t
       ;; umm, dunno, hope the user can define it
       (message "pcf2bdf.el: don't know where to get `make-temp-file'")
       (defalias 'pcf2bdf--make-temp-file 'make-temp-file)))

;; `set-buffer-multibyte' not in xemacs21
(eval-and-compile
  (defalias 'pcf2bdf--set-buffer-multibyte
    (if (eval-when-compile (fboundp 'set-buffer-multibyte))
        'set-buffer-multibyte  ;; emacs
      'identity)))             ;; not applicable in xemacs21


;;-----------------------------------------------------------------------------
;; generic

(eval-when-compile
  (defmacro pcf2bdf--with-errorfile (&rest body)
    "An internal part of pcf2bdf.el.
This macro doesn't exist when running byte compiled.

Create a temporary file for use by the BODY forms.
Variable `errorfile' is the filename.
An `unwind-protect' ensures the file is removed no matter what
BODY does."
    ;; (declare (debug t))  ;; emacs22,xemacs21, or 'cl
    `(let ((errorfile (pcf2bdf--make-temp-file "pcf2bdf-el-")))
       (unwind-protect
           (progn ,@body)
         (delete-file errorfile)))))

;; quieten byte compiler pre-emacs23
(defvar write-region-post-annotation-function)

(eval-when-compile
  (defmacro pcf2bdf--without-post-kill (&rest body)
    "An internal part of pcf2bdf.el.
This macro doesn't exist when running byte compiled.

Evaluate BODY without post-annotation kill-buffer.
If `write-region-post-annotation-function' is set buffer-local to
`kill-buffer' then set it to nil for BODY, and restore by an
`unwind-protect' afterwards.

This is a workaround for a bug in Emacs 23.1 where
`write-region-post-annotation-function' is set to `kill-buffer',
meaning any writes done by an encode function kill the buffer
that the encode is supposed to be operating on, usually making it
go on to mangle the contents of an unrelated buffer."

    ;; (declare (debug t))  ;; emacs22,xemacs21, or 'cl
    `(let* ((pcf2bdf--without-post-kill--bad
             (and (local-variable-p 'write-region-post-annotation-function
                                    (current-buffer))
                  (eq write-region-post-annotation-function
                      'kill-buffer)))
            (pcf2bdf--without-post-kill--buffer (current-buffer)))
       (unwind-protect
           (progn
             (if pcf2bdf--without-post-kill--bad
                 (setq write-region-post-annotation-function nil))
             ;; (message "buf  %S" pcf2bdf--without-post-kill--buffer)
             ;; (message " bad  %S" pcf2bdf--without-post-kill--bad)
             ;; (message " now  %S" write-region-post-annotation-function)
             ,@body)
         (and pcf2bdf--without-post-kill--bad
              (buffer-live-p pcf2bdf--without-post-kill--buffer)
              (with-current-buffer pcf2bdf--without-post-kill--buffer
                (set (make-local-variable 'write-region-post-annotation-function)
                     'kill-buffer)))))))

(defun pcf2bdf-switch-to-buffer-other-window (buffer)
  "An internal part of pcf2bdf.el.
Switch to display BUFFER in another window.
If it wasn't already in a window then the window is shrunk with
`shrink-window-if-larger-than-buffer'."
  (let ((existing-window (get-buffer-window buffer)))
    (condition-case nil
        ;; emacs two args
        (switch-to-buffer-other-window buffer t) ;; no-record
      (error
       ;; xemacs one arg
       (switch-to-buffer-other-window buffer)))
    (if (not existing-window)
        (shrink-window-if-larger-than-buffer
         (get-buffer-window buffer)))))

;;-----------------------------------------------------------------------------
;; compilation-mode

;; Error messages from bdftopcf, eg.
;;     BDF Error on line 3: Blah Blah
;;
;; There's no filename in the messages but a couple of hacks below get
;; around that for the error output buffer.
;;
(eval-after-load "compile"
  '(let ((error-elem '(pcf2bdf--bdftopcf "^BDF Error on line \\([0-9]+\\)"
                                         nil 1))
         (file-elem  '(pcf2bdf--filename
                       "^\\(### pcf2bdf\\.el input:\\)$" 1)))
     (eval-when-compile (require 'compile))
     (cond
      ((eval-when-compile (boundp 'compilation-error-regexp-systems-list))
       ;; xemacs21
       (add-to-list 'compilation-error-regexp-alist-alist
                    (list (car error-elem)
                          '("^\\(BDF Error on line \\)\\([0-9]+\\)" 1 2)))
       (compilation-build-compilation-error-regexp-alist))

      ((eval-when-compile (boundp 'compilation-error-regexp-alist-alist))
       ;; emacs22 up
       (add-to-list 'compilation-error-regexp-alist-alist file-elem)
       (add-to-list 'compilation-error-regexp-alist       (car file-elem))
       (add-to-list 'compilation-error-regexp-alist-alist error-elem)
       (add-to-list 'compilation-error-regexp-alist       (car error-elem)))

      (t
       ;; emacs21
       (add-to-list 'compilation-error-regexp-alist (cdr error-elem))
       (add-to-list 'compilation-file-regexp-alist (cdr file-elem))))))

(defvar pcf2bdf-originating-buffer nil
  "An internal part of pcf2bdf.el.
Originating bdf text buffer for *pcf2bdf-errors*.
This has a buffer-local value in the *pcf2bdf-errors* buffer.")
(make-variable-buffer-local 'pcf2bdf-originating-buffer)

(defadvice compilation-find-file (around pcf2bdf activate)
  "Use `pcf2bdf-originating-buffer' for bdftopcf errors."
  ;; args: (compilation-find-file MARKER FILENAME DIRECTORY &rest FORMATS)
  (if (and pcf2bdf-originating-buffer
           (member (ad-get-arg 1) ;; FILENAME
                   '("### pcf2bdf.el input:"
                     "BDF Error on line "))) ;; xemacs21 hack
      (setq ad-return-value pcf2bdf-originating-buffer)
    ad-do-it))

(defun pcf2bdf-unload-function ()
  "Remove advice from `compilation-find-file'.
This is called by `unload-feature'."
  (when (ad-find-advice 'compilation-find-file 'around 'pcf2bdf)
    (ad-remove-advice   'compilation-find-file 'around 'pcf2bdf)
    (ad-activate        'compilation-find-file))
  nil) ;; and do normal unload-feature actions too

;;-----------------------------------------------------------------------------
;; format

;; No automatic detection here.  The pattern would be "\\`\x01\x66\x63\x70",
;; but it's conceivable someone might make a pcf image mode displaying the
;; glyphs as images, or something like that, in which case auto-decode would
;; be harmful.
;;
;; The "file" program version 4.24 in /usr/share/file/magic notes there's a
;; potential clash between pcf and MIPSEL COFF object file too, as both
;; start with #x01 #x66.  Though that's hardly likely to be a problem in
;; practice.
;;
(add-to-list 'format-alist '(pcf2bdf
                             "PCF compiled font file."
                             nil ;; no automatic decode
                             pcf2bdf-decode
                             pcf2bdf-encode
                             t     ;; encode modifies the region
                             nil)) ;; write removes from buffer-file-formats

(defun pcf2bdf-decode (beg end)
  ;; checkdoc-params: (beg end)
  "Run pcf2bdf on raw .pcf bytes in the current buffer.
This function is for use from `format-alist'.

The buffer should be unibyte as per a `raw-text-unix' read.  The
bytes are put through pcf2bdf to get bdf text and the buffer is
switched to multibyte.  An error is thrown if pcf2bdf can't be
run or the buffer contents are invalid."

  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      ;; fairly sure pcf2bdf spits out ascii-only, but go `undecided'
      (pcf2bdf-run-fmt "pcf2bdf" 'raw-text-unix 'undecided nil)
      (pcf2bdf--set-buffer-multibyte t)
      (point-max))))

(defun pcf2bdf-encode (beg end buffer)
  ;; checkdoc-params: (beg end buffer)
  "Run bdftopcf on bdf text in the current buffer.
This function is for use from `format-alist'.

The buffer text is put through bdftopcf to produce pcf bytes,
which replace the text and the buffer is switched to unibyte.
An error is thrown if bdftopcf can't be run or the buffer
contents are invalid."

  (pcf2bdf--without-post-kill
   (save-excursion
     (save-restriction
       (narrow-to-region beg end)
       ;; dunno what input coding bdftopcf expects, chances are it's meant to
       ;; be all ascii
       (pcf2bdf-run-fmt "bdftopcf" 'iso-8859-1 'raw-text-unix buffer)
       (point-max)))))

(defun pcf2bdf-run-fmt (command write-coding read-coding originating-buffer)
  ;; checkdoc-params: (command write-coding read-coding originating-buffer)
  "An internal part of pcf2bdf.el.
Run bdftopcf or pcf2bdf on the current buffer.

COMMAND is a string \"bdftopcf\" or \"pcf2bdf\", the program to
run.  Program output replaces the buffer contents.

ORIGINATING-BUFFER is the source buffer for when running bdftopcf.
This might be different from the current buffer.  It's recorded
as the place to go for errors."

  (pcf2bdf--with-errorfile
   (let ((input-buffer (current-buffer))
         status)

     (with-temp-buffer
       (let ((temp-buffer (current-buffer)))
         (with-current-buffer input-buffer
           (let ((coding-system-for-write write-coding)
                 (coding-system-for-read  read-coding)
                 (default-directory       "/")
                 (process-connection-type nil)) ;; pipe
             (setq status (call-process-region
                           (point-min) (point-max) command
                           nil               ;; don't delete old
                           (list temp-buffer ;; stdout to temp buffer
                                 errorfile)  ;; stderr to file
                           nil))))           ;; no redisplay

         ;; replace input-buffer with new if `call-process-region' didn't error out
         (copy-to-buffer input-buffer (point-min) (point-max))))

     (if (equal 0 status)
         (let ((buffer (get-buffer "*pcf2bdf-errors*")))
           (when buffer
             (delete-windows-on buffer)
             (kill-buffer buffer)))

       (save-selected-window
         (with-current-buffer (get-buffer-create "*pcf2bdf-errors*")
           (setq buffer-read-only nil)
           (fundamental-mode)
           (erase-buffer)

           ;; emacs21 ignores the first two lines of a compilation-mode
           ;; buffer, so add in dummies
           (if (numberp status)
               (insert (format "%s exit %s\n\n" command status))
             (insert (format "%s %s\n\n" command status)))

           ;; matched by `pcf2bdf--filename' compile pattern above
           (insert "### pcf2bdf.el input:\n")

           (insert-file-contents errorfile)

           (pcf2bdf-switch-to-buffer-other-window (current-buffer))
           (goto-char (point-min))
           (if originating-buffer
               (compilation-mode))
           (setq pcf2bdf-originating-buffer originating-buffer)
           (error "%s error, see *pcf2bdf-errors* buffer" command)))))))

;;-----------------------------------------------------------------------------
;; apply format

;;;###autoload
(defun pcf2bdf ()
  "Decode a pcf font file to bdf text using the pcf2bdf program.
The buffer should be raw bytes (`raw-text-unix' unibyte).

The `format-alist' mechanism is used, so the bdf can in fact be
edited and re-saved, with recoding done with the usual X bdftopcf
program.  It's unlikely you'd want to do that since there's
nothing to set the various endianness and width options for
bdftopcf to make a .pcf that a given server would most enjoy.

If `auto-compression-mode' is enabled then .pcf.gz files can be
read too.  (The bytes are decompressed before they reach this
`pcf2bdf' decode.)

For saving, note that `tar-mode' (as of Emacs 23) doesn't follow
`buffer-file-format' so writing a .pcf inside a .tar gives just
the bdf text.  This afflicts all file format things, including
the builtin `enriched-mode'.  So don't do that.  `archive-mode'
saving is ok.

The buffer is put into `bdf-mode' if such a mode exists,
otherwise `fundamental-mode'.  See also bm-po.el (which was also
called po.el) from BITMAP-MULE for some bit-editing of the bdf
hexadecimal.

The pcf2bdf program home page is
  URL `https://github.com/ganaware/pcf2bdf/'
The pcf2bdf.el home page is
  URL `http://user42.tuxfamily.org/pcf2bdf/index.html'"

  (interactive)
  (unless (memq 'pcf2bdf buffer-file-format)
    (let ((inhibit-read-only t))
      (format-decode-buffer 'pcf2bdf)))

  (if (fboundp 'bdf-mode) ;; if a hypothetical bdf-mode exists
      (bdf-mode)
    (fundamental-mode)))

;;;###autoload (add-to-list 'auto-mode-alist '("\\.pcf\\'" . pcf2bdf))


;;-----------------------------------------------------------------------------

;; LocalWords: pcf bdf bdftopcf unibyte endianness gz builtin ok recoding po el

(provide 'pcf2bdf)

;;; pcf2bdf.el ends here
