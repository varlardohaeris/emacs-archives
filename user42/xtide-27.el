;;; xtide.el --- XTide display in Emacs

;; Copyright 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 27
;; Keywords: calendar, tides
;; URL: http://user42.tuxfamily.org/xtide/index.html
;; EmacsWiki: XTide

;; xtide.el is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; xtide.el is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.
;;
;; Note that the data produced by XTide, and then shown by xtide.el, is NOT
;; FOR NAVIGATION.


;;; Commentary:

;; `M-x xtide' displays a tide height graph in Emacs using the "tide"
;; program from XTide (http://www.flaterco.com/xtide).
;;
;; `M-x calendar' is extended with a "T" key to display the tides for the
;; selected day, like "S" does for sunrise/sunset.
;;
;; The graph is a PNG image if Emacs and the display supports that,
;; otherwise XTide's ascii-art fallback.  Left and right arrow keys move
;; forward or back in time.  The alternative displays from xtide like "b"
;; banner or "p" plain times can be viewed too, see the mode help "C-h m" or
;; the menu for key bindings and choices.
;;
;; The location is taken from a customizable `xtide-location' variable, or
;; if that's not set then from the XTIDE_DEFAULT_LOCATION environment
;; variable which xtide uses.  If neither is set then M-x xtide starts in
;; the `xtide-location-mode' selector.  The "l" key goes there too, to
;; change to tides at a different place.
;;
;; In xtide-location-mode if you find a place close to what you want then
;; try "d" (`xtide-location-sort-distance') to sort by distance from there
;; to see nearby stations.  If you have `calendar-latitude' and
;; `calendar-longitude' set for your location then try "C-u d" to see
;; stations near your own lat,long.

;;; Emacsen:

;; Designed for Emacs 21 up and XEmacs 21 up.
;;
;; Works in Emacs 20 if you have either Gnus mm-util.el or APEL poe.el for
;; make-temp-file.  No graphics images in Emacs 20, only the ascii art.

;;; Install:

;; To make `M-x xtide' available put xtide.el in one of your `load-path'
;; directories and the following in your .emacs
;;
;;     (autoload 'xtide "xtide" nil t)
;;
;; To get the calendar mode "T" key bound when calendar loads, add
;;
;;     (autoload 'xtide-calendar-setups "xtide")
;;     (add-hook 'calendar-load-hook 'xtide-calendar-setups)
;;
;; The docstring for `xtide-calendar-setups' has notes on how to defer
;; loading xtide in the calendar until it's actually used.
;;
;; If you make bookmarks in the location mode buffer with `M-x bookmark-set'
;; etc (Emacs 23 up) then ensure the jump function is autoloaded too,
;;
;;     (autoload 'xtide-location-bookmark-jump "xtide")
;;
;; There's autoload cookies for these various functions below if you install
;; via `M-x package-install' or know how to use `update-file-autoloads'.

;; Bugs:

;; The graphics image shown always uses the xtide default height 312 pixels,
;; irrespective of the Emacs window height.  Probably should scale something
;; like 80 columns text worth of pixels is 48 hours, then height in pixels
;; by aspect ratio or window height whichever is smaller.  The aim would be
;; sensible scaling on high resolution screen where traditional typical size
;; in pixels would be too small.
;;
;; For reference, there was some breakage in Emacs 23.1 (fixed in 23.2)
;; under Gtk with mode-specific menus in the menu-bar.  The "XTide" menu
;; shows either empty or a copy of a menu from wherever you started
;; M-x xtide.  This was a general problem, it can be seen for instance in
;; tex-mode, eg. start "emacs -Q" then C-x C-f /tmp/x.tex and the TeX menu
;; is Lisp bits.

;;; History:

;; Version 1 - the first version
;; Version 2 - interactive location selection
;; Version 3 - misc tweaks, incl read-only buffer, and font-lock on "+" marker
;; Version 4 - C-u d to sort from calendar location
;; Version 5 - namespace clean on xemacs bits
;; Version 6 - add menus
;; Version 7 - displayed position held in timezone-independent form
;;           - new location sort order "C-u a" by locality
;; Version 8 - utf8 locale fixes per report by Alan E. Davis
;; Version 9 - `xtide-run-switches' suggested by Alan E. Davis
;;           - new w/W width zoom
;; Version 10 - buttonize the tcd file in "a" mode
;; Version 11 - use pipe rather than pty for subprocesses
;; Version 12 - fixes for calendar key setups
;; Version 13 - propagate calendar date through location mode
;; Version 14 - better sort order for latin-1 location names
;; Version 15 - kill buffers on unload-feature
;; Version 16 - use easy-menu to have menus in xemacs
;; Version 17 - clone-buffer for multiple xtide buffers
;; Version 18 - fix for png image display
;; Version 19 - add a `revert-buffer-function' for redrawing
;; Version 20 - fixes for emacs20,21 `derived-mode-p'
;; Version 21 - bookmark.el for location mode buffer
;;            - don't zap current buffer in `xtide-location-mode'
;; Version 22 - use run-mode-hooks,
;;              setup for customize-mode in xtide-location-mode
;; Version 23 - add desktop.el save/restore
;;            - add Calendar/Tides menu entry in xemacs
;;            - fix for calendar date propagation
;; Version 24 - fix for initial time
;; Version 25 - fix for location distance sort on West longitudes
;; Version 26 - add C-x C-s save, add bookmark.el for tide buffer
;; Version 27 - tool-bar tide specific items only, no copy of the global

;;; Code:

(eval-when-compile
  (unless (and (fboundp 'declare)
               (fboundp 'dolist)
               (fboundp 'dotimes))
    (require 'cl))) ;; for `dolist',`dotimes' macros in emacs20

;; in bookmark.el
(defvar bookmark-make-record-function)

;; Conditionalized declarations like this don't influence emacs23 byte
;; compiler.  How to give them and still be compatible?
;; (eval-when-compile (require 'bookmark)) works, but don't want to drag it
;; in when running from source.
;;
;; (when (eval-when-compile (fboundp 'declare-function))
;;   (declare-function bookmark-make-record-default "bookmark" (&optional pos-only))
;;   (declare-function bookmark-prop-get            "bookmark" (bookmark prop))
;;   (declare-function bookmark-default-handler     "bookmark" (bmk))
;;   (declare-function bookmark-get-bookmark-record "bookmark" (bmk)))


;;-----------------------------------------------------------------------------
;; custom variables

;;;###autoload
(defgroup xtide nil "Xtide."
  :prefix "xtide-"
  :group  'applications
  ;; links in reverse order to how they actually display
  :link   '(url-link :tag "xtide.el home page"
                     "http://user42.tuxfamily.org/xtide/index.html")
  :link   '(url-link :tag "XTide home page"
                     "http://www.flaterco.com/xtide"))

(defcustom xtide-location ""
  "The location for XTide tidal predictions.
This must be a place known to xtide.  Often just a first word or
two is enough to be unique.  If an empty string or nil then the
environment variable XTIDE_DEFAULT_LOCATION is used instead, if
that's set.

`xtide-location-mode' changes this variable as you go to
different places.  When you find one you like you can copy it to
your .emacs or save with \\[customize-variable] `xtide-location'
or \\[customize-group] xtide.  Customize will report
`xtide-location' has been changed outside the customize buffer
but clicking on Save still works.

Each `xtide-mode' buffer has this buffer-local, so different
buffers can show different places.  Not sure if customize enjoys
that, but it's a good way to see tides at different places in two
buffers."

  :type  'string
  :group 'xtide)

;; not sure it makes much sense to set xtide-location as buffer-local
;; anywhere except an xtide buffer, but any string is safe
;;;###autoload
(put 'xtide-location 'safe-local-variable 'stringp)

(defcustom xtide-run-switches nil
  "List of extra command line switches to pass to the xtide program.
You can use this for various xtide options not directly
controlled by xtide.el.  For example

    '(\"-fc\" \"SeaGreen\")

would set the flood colour to SeaGreen which is the same as the
ebb colour, if you don't want to see rising and falling tides in
contrasting colours.

You can set xtide options from your ~/.xtide.xml file too.  The
settings here override .xtide.xml.

For available options see the xtide README file or
URL `http://www.flaterco.com/xtide/settings.html'.

This is a \"risky-local-variable\" because a malicious \"-o
outfile\" option could append to a random file."

  :type  '(repeat string)
  :group 'xtide)

;;;###autoload
(put 'xtide-run-switches 'risky-local-variable t)

(defcustom xtide-mode-hook nil
  "*Hook run by `xtide-mode'."
  :type  'hook
  :group 'xtide)

(defcustom xtide-location-mode-hook nil
  "*Hook run by `xtide-location-mode'."
  :type  'hook
  :group 'xtide)


;;-----------------------------------------------------------------------------
;; constants

(defconst xtide-buffer "*xtide*"
  "The xtide tides buffer name.")

(defconst xtide-location-buffer "*xtide-location*"
  "The xtide location buffer.")


;;-----------------------------------------------------------------------------
;; compatibilities

(cond ((or (eval-when-compile (fboundp 'make-temp-file))
           (fboundp 'make-temp-file))
       ;; emacs21 up, noticed at compile time or run time
       (eval-and-compile
         (defalias 'xtide-make-temp-file 'make-temp-file)))

      ((locate-library "mm-util") ;; from gnus
       ;; xemacs21
       (autoload 'mm-make-temp-file "mm-util")
       (defalias 'xtide-make-temp-file 'mm-make-temp-file))

      ((locate-library "poe") ;; from APEL
       ;; emacs20 with poe.el add-on
       (require 'poe)
       (defalias 'xtide-make-temp-file 'make-temp-file))

      (t
       ;; umm, dunno, hope the user can define it
       (message "xtide.el: don't know where to get `make-temp-file'")
       (defalias 'xtide-make-temp-file 'make-temp-file)))

(defun xtide-image-type-available-p (type)
  "An internal part of xtide.el.
As per Emacs `image-type-available-p'.
TYPE is a symbol like `png'."
  (cond ((eval-when-compile (fboundp 'image-type-available-p))
         ;; emacs21 up
         (image-type-available-p type))
        ((eval-when-compile (fboundp 'image-instantiator-format-list))
         ;; xemacs21
         (memq type (image-instantiator-format-list)))
        (t  ;; emacs20
         nil)))

(defun xtide-display-images-p ()
  "An internal part of xtide.el.
As per Emacs `display-images-p'.
Return non-nil if the current frame can display images."
  (if (eval-when-compile (fboundp 'display-images-p))
      (display-images-p)  ;; emacs
    window-system))       ;; xemacs21 approximation

(defvar xtide-buffer-multibyte-p nil
  "An internal part of xtide.el.")
(make-variable-buffer-local 'xtide-buffer-multibyte-p)
(defun xtide-set-buffer-multibyte (bool)
  "An internal part of xtide.el.
`set-buffer-multibyte' to BOOL in Emacs.
Remember the setting in `xtide-buffer-multibyte-p'."
  (if (eval-when-compile (fboundp 'set-buffer-multibyte))
      (set-buffer-multibyte bool))
  ;; no multibyte applicable in xemacs21 but remember the setting
  (setq xtide-buffer-multibyte-p bool))

;; Could check at run-time instead or as well in case the user had a
;; suitable easymenu.el.  For now assume the byte-compile time conditions
;; are the same as the runtime.
(eval-when-compile
  (defun xtide-easy-menu-help (str)
    "An internal part of xtide.el.
This function does not exist when running byte compiled.
Return a list `(:help STR)' if easy-menu supports that, otherwise nil.
The easymenu.el in XEmacs 21 doesn't allow :help."
    (and (eval-when-compile
           (string-match ":help\\b" (documentation 'easy-menu-define)))
         (list :help str))))

(eval-when-compile
  (defmacro xtide-with-temp-message (message &rest body)
    "An internal part of xtide.el.
This macro does not exist when running byte compiled.

Display MESSAGE temporarily while evaluating BODY.
This is the same as `with-temp-message' but has a workaround for a bug in
Emacs 21.4 where the temporary message isn't erased if there was no previous
message."
    (declare (debug t) (indent 1)) ;; from 'cl
    (if (eval-when-compile (featurep 'xemacs))
        ;; new key for each macro usage, so can nest
        (let ((key (gensym "xtide-with-temp-message--")))
          `(unwind-protect
               (progn
                 (display-message ',key ,message)
                 (prog1 (progn ,@body)
                   (clear-message ',key)))
             (clear-message ',key)))

      `(let* ((xtide-with-temp-message--oldmsg (current-message)))
         (unwind-protect
             (prog1 (with-temp-message ,message ,@body)
               (or xtide-with-temp-message--oldmsg (message nil)))
           (or xtide-with-temp-message--oldmsg (message nil)))))))

;;-----------------------------------------------------------------------------
;; stuff new in emacs21

(defun xtide--derived-mode-p (&rest modes)
  "An internal part of xtide.el.
Return non-nil if the current mode is derived any of MODES (symbols).
This is `derived-mode-p' when available, or some explicit code
for Emacs 20."
  ;; emacs21 doesn't autoload derived-mode-p, so `require' it
  (require 'derived)
  (if (fboundp 'derived-mode-p)
      (apply 'derived-mode-p modes)
    ;; emacs20, similar loop to emacs21 `derived-mode-p'
    (let ((m major-mode))
      (while (and (not (memq m modes))
                  (setq m (get m 'derived-mode-parent))))
      m)))

;;-----------------------------------------------------------------------------
;; stuff new in emacs22

(eval-when-compile
  (if (eval-when-compile (fboundp 'with-case-table))
      ;; emacs22 up
      (defalias 'xtide--with-case-table 'with-case-table)

    ;; emacs20, emacs21, xemacs21
    (defmacro xtide--with-case-table (table &rest body)
      "An internal part of xtide.el.
This macro doesn't exist when running byte compiled.
Run BODY with a `set-case-table' of TABLE.
The previous case-table is restored with an `unwind-protect'."
      (declare (indent 1)) ;; from 'cl
      (let ((old-case-table (make-symbol "old-case-table")))
        `(let ((,old-case-table (current-case-table)))
           (unwind-protect
               (progn
                 (set-case-table ,table)
                 ,@body)
             (set-case-table ,old-case-table)))))))


;;-----------------------------------------------------------------------------
;; misc

;; Xtide 2.9 and up follows a utf-8 locale for output, though maybe only
;; 2.9.4 prints the location listing in utf-8 correctly.  Past versions
;; always printed latin-1 irrespective of the locale.  Instead of figuring
;; out what it can or will do, the idea of `xtide-with-defanged-locale' is
;; to run in "C" locale which will get latin-1 in both past and present
;; xtide.  Xtide as of version 2.10 is latin-1 internally anyway, so nothing
;; is lost by not using utf-8.  If/when that's not so then something more
;; sophisticated could be needed.
;;
;; Xtide 2.9.5 and up with the msdos visual compiler and without langinfo()
;; always uses codepage 1252 instead of looking at the locale, or something
;; like that.  cp1252 is a small superset of latin1 mis-using the 8-bit
;; control chars 0x80 to 0x9F.  Xtide doesn't do anything with those extras,
;; so it's fine to read/write latin1 in this case too.
;;
;; Some .tcd files from March 2007 had some utf-8 which then came through to
;; the tide program output, but that was just a temporary balls-up in those
;; files.
;;
(eval-when-compile
  (defmacro xtide-with-defanged-locale (&rest body)
    "An internal part of xtide.el.
This macro doesn't exist when running byte compiled.
Run BODY with locale forced to LANG=C etc."
    `(let ((process-environment (copy-sequence process-environment)))
       ;; probably forcing just LC_CTYPE is enough
       (setenv "LANG" "C")
       (setenv "LC_ALL" "C")
       (setenv "LC_CTYPE" "C")
       ,@body)))

(eval-when-compile
  (defmacro xtide-with-errorfile (&rest body)
    "An internal part of xtide.el.
This macro doesn't exist when running byte compiled.
Create an `errorfile' for use by the BODY forms.
A local variable `errorfile' is bound to the filename of a newly
created temporary file.  An `unwind-protect' around BODY ensures
the file is removed no matter what BODY does."
    `(let ((errorfile (xtide-make-temp-file "xtide-el-")))
       (unwind-protect
           (progn ,@body)
         (delete-file errorfile)))))

(defun xtide-unaccent-case-table ()
  "An internal part of xtide.el.
Return a case table downcasing latin-1 accented letters to ascii.
The effect is to sort accented characters in with unaccented.

xtide's own location display uses a more sophisticated collation
mapping (see moascf() in Dstr.cc), but this is close enough and
lets Emacs `sort-fold-case' do the work of mapping characters.

Currently for Emacs 21 this just returns `current-case-table',
since there's something fishy in the setups needed for it to sort
with a new case table."

  (cond
   ((eval-when-compile (fboundp 'put-case-table))
    ;; xemacs21
    (let ((table (copy-case-table (current-case-table)))
          (from  "¿¡¬√ƒ≈∆«»… ÀÃÕŒœ—“”‘’÷ÿŸ⁄€‹›‡·‚„‰ÂÊÁËÈÍÎÏÌÓÔÒÚÛÙıˆ¯˘˙˚¸˝ˇ")
          (to    "aaaaaaaceeeeiiiinoooooouuuuyaaaaaaaceeeeiiiinoooooouuuuyy"))
      (dotimes (i (length from))
        (put-case-table 'downcase (aref from i) (aref to i) table))
      table))

   ((eval-when-compile (fboundp 'with-case-table))
    ;; emacs22 up
    ;; Could keep a table around permanently and just set-char-table-parent
    ;; each time.  But it's probably not used much, and `sort-subr' is slow
    ;; already, so it wouldn't speed anything up.
    (let* ((table (make-char-table 'case-table nil))
           (from  "¿¡¬√ƒ≈∆«»… ÀÃÕŒœ—“”‘’÷ÿŸ⁄€‹›‡·‚„‰ÂÊÁËÈÍÎÏÌÓÔÒÚÛÙıˆ¯˘˙˚¸˝ˇ")
           (to    "aaaaaaaceeeeiiiinoooooouuuuyaaaaaaaceeeeiiiinoooooouuuuyy"))
      (dotimes (i (length from))
        (aset table (aref from i) (aref to i)))
      (set-char-table-parent table (current-case-table))
      table))

   (t
    ;; emacs21
    (current-case-table))))


(eval-when-compile
  (defmacro xtide-with-TZ (tz &rest body)
    ;; checkdoc-params: (tz body)
    "An internal part of xtide.el.
This macro doesn't exist when running byte compiled.

Run BODY with `set-time-zone-rule' temporarily to TZ.
The current timezone (per `getenv' \"TZ\") is restored by an
`unwind-protect'.

This doesn't work properly if a `set-time-zone-rule' has been
applied but (setenv \"TZ\") not updated accordingly.  A `setenv'
is done here so that `xtide-with-TZ' will nest successfully at
least."

    ;; This getenv and restore is similar to what add-log.el
    ;; `add-change-log-entry' and time-stamp.el `time-stamp-string' do.  In
    ;; Emacs 23.2 the initializer in `display-time-world-list' always sets
    ;; back to nil, but a restore seems a much better idea.
    ;;
    ;; (setenv "TZ") makes the new TZ visible to a nested `xtide-with-TZ' or
    ;; to `add-change-log-entry'.  As of Emacs 23.2 `set-time-zone-rule'
    ;; changes TZ in the C-level environ[], but not in the lisp level
    ;; `process-environment'.
    ;;
    ;; setenv in emacs22 up returns the value set, but in emacs21 and xemacs21
    ;; it's the whole new `process-environment', or some such, so don't depend
    ;; on the value.
    ;;
    ;; setenv in emacs22 up also calls `set-time-zone-rule' itself, but for
    ;; emacs21 and xemacs21 must do so explicitly
    ;;
    (declare (indent 1)) ;; from 'cl
    `(let ((xtide-with-TZ--old (getenv "TZ"))
           (xtide-with-TZ--new ,tz)) ;; evaluate `tz' expression only once
       (setenv "TZ" xtide-with-TZ--new)
       (set-time-zone-rule xtide-with-TZ--new)
       (unwind-protect
           (progn ,@body)
         (setenv "TZ" xtide-with-TZ--old)
         (set-time-zone-rule xtide-with-TZ--old)))))


;;-----------------------------------------------------------------------------
;; location chooser

(defvar xtide-location-mode-target nil
  "An internal part of xtide.el.
Buffer-local in an `xtide-location-mode' buffer and giving the
name of the target `xtide-mode' buffer which should be acted on
when the user selects a location (with `xtide-location-go').

If there's more than one `xtide-mode' buffer then a location
selector must note which it's supposed to act on.

nil or a no-longer existing buffer name here will act on the
default *xtide* buffer.")

(make-variable-buffer-local 'xtide-location-mode-target)

(defun xtide-location-value ()
  "An internal part of xtide.el.
Return current `xtide-location' variable or env var.
`xtide-location' is preferred, otherwise the
XTIDE_DEFAULT_LOCATION environment variable is used, or if
neither then return nil.

An empty string in either is not returned.  That could be an
empty customization or slightly botched XTIDE_DEFAULT_LOCATION
setup where the user probably meant unset.  Leading and trailing
whitespace is stripped so that stray spaces in customize are
ignored."

  (let (ret)
    (dolist (location (list xtide-location
                            (getenv "XTIDE_DEFAULT_LOCATION")))
        (when (and location
                   (not ret))
          (if (string-match "\\`\\(\\s-\\|\n\\)+" location) ;; leading white
              (setq location (substring location (match-end 0))))
          (if (string-match "\\(\\s-\\|\n\\)+\\'" location) ;; trailing white
              (setq location (substring location 0 (match-beginning 0))))
          (and (not (string-equal "" location))
               (setq ret location))))
    ret))

(defun xtide-insert-locations ()
  "An internal part of xtide.el.
Insert XTide's list of known locations into the current buffer by
running \"tide -ml\"."

  ;; -tw sets the terminal width, the value 110 here is meant to be enough
  ;; not to truncate any names.
  ;;
  ;; No redisplay because `xtide-location-mode' is going to jump to the
  ;; current `xtide-location' entry or the start of the buffer, so no point
  ;; drawing as the list comes out.
  ;;
  (xtide-with-defanged-locale
   (let ((coding-system-for-read 'iso-8859-1)
         (default-directory "/") ;; in case inherit non-existent directory
         (process-connection-type nil)) ;; pipe
     (call-process "tide"
                   nil           ;; input
                   (list t nil)  ;; output to buffer, stderr discard
                   nil           ;; no redisplay
                   "-tw" "110"
                   "-ml"))))

(defun xtide-location-sort-alphabetical (&optional arg)
  ;; checkdoc-params: (arg)
  "Sort location names alphabetically.
With a \\[universal-argument] prefix, sort by locality first, so
for instance all the places \"..., Hawaii\" sort together.
\(Which assumes everything in Hawaii ends \", Hawaii\".
Try \"\\[xtide-location-sort-distance]\" by distance when near what you want.)

In Emacs 21, the few places with accented characters sort to the
start of the list.  Emacs 22 up and XEmacs are ok."
  (interactive "P")

  ;; `sort-subr' here and in `xtide-location-sort-distance' below isn't
  ;; fantastically fast, but it's a lot easier than writing some special
  ;; code that might do better
  ;;
  ;; Must load sort.el explicitly (not just the `sort-subr' autoload) to
  ;; define `sort-fold-case' before let-binding that variable, otherwise the
  ;; let makes it unbound again afterwards.
  ;;
  (eval-and-compile ;; and when compile quieten xemacs for sort-fold-case bind
    (require 'sort))

  (let* ((origin (buffer-substring (line-beginning-position)
                                   (line-end-position))))
    (goto-char (point-min))
    (when (looking-at "Location list.*\n*")
      (goto-char (match-end 0)))

    (let ((inhibit-read-only t)
          (sort-fold-case   t))
      (xtide--with-case-table (xtide-unaccent-case-table)
        (sort-subr nil 'forward-line 'end-of-line
                   (and arg 'xtide-location-sort-locality-key))))

    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (search-forward origin nil t)
    (beginning-of-line)))

(defun xtide-location-sort-locality-key ()
  "An internal part of xtide.el.
Return a string for a \"by locality\" sort key.
Point should be on a location line in the `xtide-location-mode'
buffer.  The return is say \"Hawaii,Niihau Island,Nonopapa\" with
the locality first.  This is just a reverse of the
comma-separated parts.  Of course if location names aren't in
that style it won't be much good as a sort order."
  (let ((loc (downcase (xtide-location-mode-location-at-point))))
    ;; trailing " - READ http://..."
    ;; or       "(2)"
    ;; or       "(2) (sub)"
    ;; or       "(expired 1988-12-31)"
    ;; goes to start
    (if (string-match "\\(- READ.*\\|([^,]*)\\)\\'" loc)
        (setq loc (concat (match-string 0 loc)
                          ","
                          (substring loc 0 (match-beginning 0)))))
    ;; lose trailing spaces
    (if (string-match "\\s-+\\'" loc)
        (setq loc (substring loc 0 (match-beginning 0))))
    (mapconcat 'identity (nreverse (split-string loc "[ \t]*,[ \t]*"))
               ",")))

(eval-when-compile
  (put 'xtide-great-circle-distance 'pure t))
(defun  xtide-great-circle-distance (lat1 long1 lat2 long2)
  "An internal part of xtide.el.
Return the great-circle angular distance from LAT1,LONG1 to LAT2,LONG2.
The arguments and return are in radians."
  ;; This is per http://williams.best.vwh.net/avform.htm#Dist
  ;; Shouldn't have to worry too much about accuracy near the poles or for
  ;; very close points since there aren't many of either in the database.
  (* 2 (asin (sqrt (+ (expt (sin (* 0.5 (- lat1 lat2))) 2)
                      (* (cos lat1) (cos lat2)
                         (expt (sin (* 0.5 (- long1 long2))) 2)))))))

(defconst xtide-location-lat-long-regexp
  ;;            1=lat           2=lat dir    3=long          4=long dir
  "[A-Za-z]+ +\\([0-9.]+\\). *\\([NS]\\), *\\([0-9.]+\\). *\\([EW]\\)$"
  "An internal part of xtide.el.
Regexp for latitude and longitude on a location line.
It matches for example \"Ref 33.9833∞ S, 151.2167∞ E\" as from
xtide Coordinates::print().  The \"Ref\" or \"Sub\" is included
in the match so it can be removed to get the location part
alone.")

(defun xtide-location-line-lat-long (line)
  "An internal part of xtide.el.
From an XTide location LINE, return a list (LATITUDE LONGITUDE) in radians.
LINE is a string like
    \"Botany Bay, Australia        Ref 33.9833∞ S, 151.2167∞ E\"
Latitude returned is positive for north, negative for south.
Longitude returned is positive for east, negative for west."
  (unless (string-match xtide-location-lat-long-regexp line)
    (error "Unrecognised xtide location line: %S" line))
  (let ((lat  (string-to-number (match-string 1 line)))
        (long (string-to-number (match-string 3 line))))
    (if (string-equal "S" (match-string 2 line))
        (setq lat (- lat)))
    (if (string-equal "W" (match-string 4 line))
        (setq long (- long)))
    (list (degrees-to-radians lat) (degrees-to-radians long))))

(defvar calendar-longitude) ;; in solar.el
(defvar calendar-latitude)

(defun xtide-location-sort-distance (&optional arg)
  ;; checkdoc-params: (arg)
  "Sort by distance from the location at point.
With a \\[universal-argument] prefix, sort by distance from `calendar-latitude' and
`calendar-longitude'.

This is good for seeing other locations close to one you've found
or close to your own location.

The sort order is angular distance as the crow flies, so places
in opposite directions along the coast will be intermixed, and
nearby offshore islands likewise mixed.  It'd be cute to follow a
coastline approximately in sequence, but that's too hard."

  (interactive "P")
  (when (or arg
            (xtide-location-mode-location-at-point))
    (let* (origin-lat origin-long)
      (if arg
          (if (and calendar-latitude calendar-longitude)
              (setq origin-lat  (degrees-to-radians calendar-latitude)
                    origin-long (degrees-to-radians calendar-longitude))
            (error "`calendar-latitude' and/or `calendar-longitude' not set"))

        (let* ((origin-str  (buffer-substring (line-beginning-position)
                                              (line-end-position)))
               (origin-latlong (xtide-location-line-lat-long origin-str)))
          (setq origin-lat  (car origin-latlong)
                origin-long (cadr origin-latlong))))

      (goto-char (point-min))
      (when (looking-at "Location list.*\n*")
        (goto-char (match-end 0)))

      (let ((inhibit-read-only t))
        (sort-subr nil 'forward-line 'end-of-line
                   (lambda ()
                     (let ((line (buffer-substring (point)
                                                   (line-end-position))))
                       (apply 'xtide-great-circle-distance
                              origin-lat origin-long
                              (xtide-location-line-lat-long line))))))

      (when (get-buffer-window (current-buffer))
        (set-window-start (get-buffer-window (current-buffer))
                          (point-min) t))
      (goto-char (point-min))
      (forward-line 2))))

(defun xtide-location-mode-location-at-point ()
  "An internal part of xtide.el.
Return the XTide location at point in an `xtide-location-mode'
buffer, or return nil if not on a recognised location line."
  (save-excursion
    (beginning-of-line)
    (and (re-search-forward xtide-location-lat-long-regexp
                            (line-end-position) t)
         (goto-char (match-beginning 0))
         (skip-chars-backward " \t")
         (buffer-substring-no-properties (line-beginning-position) (point)))))

(defun xtide-location-go ()
  "View tides for the location name at point."
  (interactive)
  (let ((location (xtide-location-mode-location-at-point)))
    (unless location
      (error "No location on this line"))

    (setq-default xtide-location location) ;; global
    (let ((location-buffer (current-buffer))
          (target-buffer   (or (and (stringp xtide-location-mode-target)
                                    (get-buffer xtide-location-mode-target))
                               (get-buffer xtide-buffer))))
      ;; Bury since "l" in the xtide buffer can go back to location selection.
      ;; Or is it better not to change buffer order?
      (bury-buffer location-buffer)

      (if (buffer-live-p target-buffer)
          ;; target `xtide-mode' buffer still exists
          (progn
            (switch-to-buffer target-buffer)
            (setq xtide-location location) ;; buffer-local
            (xtide-mode-redraw))
        ;; target `xtide-mode' buffer doesn't exist, use or create the
        ;; *xtide* one, taking global xtide-location
        (xtide))

      ;; `xtide-time' coming from `xtide-calendar-tides' only used once
      (with-current-buffer location-buffer
        (kill-local-variable 'xtide-time)))))

(defvar xtide-location-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "q"   'kill-this-buffer)
    (define-key m "a"   'xtide-location-sort-alphabetical)
    (define-key m "d"   'xtide-location-sort-distance)
    (define-key m "p"   'previous-line)
    (define-key m "n"   'next-line)
    (define-key m [?\r] 'xtide-location-go)
    m)
  "Keymap for `xtide-location-mode' display buffer.")

(easy-menu-define xtide-location-mode-menu
  xtide-location-mode-map "XTide Location Menu"
  (eval-when-compile
    `("XTide"
      ["Go" xtide-location-go
       ,@(xtide-easy-menu-help "View tides at location at point")]
      ["Sort Alphabetically" xtide-location-sort-alphabetical
       ,@(xtide-easy-menu-help "Sort location names alphabetically")]
      ["Sort Alpha by Locality" ,(lambda ()
                                   (interactive)
                                   (xtide-location-sort-alphabetical t))
       :keys "\\[universal-argument] \\[xtide-location-sort-alphabetical]"
       ,@(xtide-easy-menu-help "Sort location names alphabetically, by locality part first")]
      ["Sort by Distance" xtide-location-sort-distance
       ,@(xtide-easy-menu-help "Sort by distance from the location at point.")]
      ["Sort by Distance from You" ,(lambda ()
                                      (interactive)
                                      (xtide-location-sort-distance t))
       :keys "\\[universal-argument] \\[xtide-location-sort-distance]"
       ,@(xtide-easy-menu-help "Sort by distance from your location, as given by `calendar-latitude' and `calendar-longitude' (per C-u d)")])))

;;;###autoload
(defun xtide-location-mode ()
  "XTide location display and selection.
Move point to a location and press \\[xtide-location-go] to see its tides.
\\[xtide-location-sort-distance] sorts by distance from the location at point, to find nearby places.
\\[xtide-location-sort-alphabetical] goes back to alphabetical.

Point starts at your `xtide-location' or XTIDE_DEFAULT_LOCATION,
if they're set.  The latitude and longitude of each line are off
screen to the right.  Move point across with \\[move-end-of-line] to seem them,
unless your window is very wide.

\\{xtide-location-mode-map}

There's a tie-in to bookmark.el (for Emacs 23 up) allowing it to
save positions and return to the buffer later if desired.

There's also a tie-in to desktop.el so a location buffer can be
saved and re-created."

  ;; This is not `(interactive)' because the interactive entrypoint is
  ;; `M-x xtide-location-select'.  It's not usual for a user to put an
  ;; existing buffer into `xtide-location-mode', since it's usually applied
  ;; to a list emitted by the xtide program (`xtide-insert-locations'), not
  ;; to a file etc.

  (kill-all-local-variables)
  (use-local-map xtide-location-mode-map)
  (easy-menu-add xtide-location-mode-menu)
  (setq major-mode     'xtide-location-mode
        mode-name      "XTide Location"
        truncate-lines t)
  (set (make-local-variable 'bookmark-make-record-function)
       'xtide-location-bookmark-make-record)
  (set (make-local-variable 'desktop-save-buffer)
       'ignore) ;; nil for desktop.el MISC-DATA

  (if (eval-when-compile (fboundp 'run-mode-hooks)) ;; xemacs21,emacs22 up
      (run-mode-hooks 'xtide-location-mode-hook)
    (run-hooks 'xtide-location-mode-hook)))

;; for `M-x customize-mode'
(put 'xtide-location-mode 'custom-mode-group 'xtide)

;; scroll-1.el is good in xtide-location-mode
(if (fboundp 'scroll-1-keybindings)
    (custom-add-option 'xtide-location-mode-hook 'scroll-1-keybindings))


;;-----------------------------------------------------------------------------
;; xtide-location-mode bookmark.el tie-in

(defun xtide-location-bookmark-make-record ()
  "An internal part of xtide.el.
Return a bookmark record for an `xtide-location-mode' line.
This function is designed for use from variable
`bookmark-make-record-function' as a tie-in to bookmark.el.

The bookmark records the location name so it works whether the
locations are sorted or not (`xtide-location-sort-alphabetical'
etc)."

  (nconc (list (xtide-location-mode-location-at-point)  ;; location name as bookmark name
               (cons 'location (xtide-location-mode-location-at-point))
               (cons 'column   (current-column))
               '(handler     . xtide-location-bookmark-jump))
         (bookmark-make-record-default t))) ;; NO-FILE, point only

;; autoload so available to saved bookmarks when xtide.el isn't yet loaded
;;;###autoload
(defun xtide-location-bookmark-jump (bmk)
  "Jump to bookmark record BMK for `xtide-location-mode'.
This is designed for use from the bookmark records created by
`xtide-location-bookmark-make-record'."

  (xtide-location-select)
  (goto-char (point-min))
  (let ((location (bookmark-prop-get bmk 'location)))
    (if (re-search-forward (concat "^" (regexp-quote location)) nil t)
        (move-to-column (or (bookmark-prop-get bmk 'column) 0))
      ;; location string not found, fall back to plain bookmark `position' etc
      (message "xtide location string not found: %S" location)
      (bookmark-default-handler
       (nconc (list "" ;; dummy bookmark name
                    (cons 'buffer (current-buffer)))
              (bookmark-get-bookmark-record bmk)))))) ;; sans bookmark name


;;-----------------------------------------------------------------------------
;; location timezones

(defvar xtide-location-time-zone-cache nil
  "An internal part of xtide.el.
An alist of (LOCATION . TIME-ZONE-RULE-STRING) used by
`xtide-location-time-zone'.

Currently this is never cleared out, on the assumption you'll
only normally look at a few places.")

(defun xtide-location-time-zone (location)
  "An internal part of xtide.el.
Return a time zone rule string for LOCATION.
The zone is found by running \"tide -ma -l LOCATION\", then
cached in `xtide-location-time-zone-cache'.  If LOCATION is
unknown then the return is nil."
  (or (cdr (assoc location xtide-location-time-zone-cache))
      (with-temp-buffer
        (setq default-directory "/") ;; in case inherit non-existent
        (xtide-with-temp-message (format "Getting xtide timezone for %s ..."
                                         location)
          (when (eq 0 (xtide-with-defanged-locale
                       (let ((process-connection-type nil)) ;; pipe
                         (call-process "tide"
                                       nil        ;; stdin /dev/null
                                       (list t t) ;; stdout+stderr to buffer
                                       nil        ;; no redisplay
                                       "-ma" "-l" location))))
            (goto-char (point-min))
            (when (re-search-forward "^Time zone *\\(.*\\)" nil t)
              (add-to-list 'xtide-location-time-zone-cache
                           (cons location (match-string 1)))
              (match-string 1)))))))


;;-----------------------------------------------------------------------------
;; tide display

(defvar xtide-time nil
  "The start time of the XTide display.
In the current implementation this is `current-time' list format
of Greenwich Mean Time, and is passed to xtide with
`format-time-string' in the timezone of the location shown.")

(defvar xtide-output-mode "g"
  "The XTide output mode, as a string, eg. \"g\" for graph.
This is buffer-local in an `xtide-mode' buffer and is changed by
commands like `xtide-show-banner'.  The global value is used for
new `xtide-mode' buffers.")

(defvar xtide-aspect-ratio 1.0
  "The XTide \"-ga\" aspect ratio for graphs.
This is buffer-local in an `xtide-mode' buffer and is increased
or decreased by `xtide-zoom-in-horizontal' and
`xtide-zoom-out-horizontal'.  The global value of this variable
is used in new `xtide-mode' buffers.")

(defun xtide-window-text-area-pixel-width ()
  "An internal part of xtide.el.
Return the width in pixels of the text portion of the current window.
This is `window-text-area-pixel-width' in XEmacs, or a fallback
calculation in GNU Emacs."
  (if (eval-when-compile (fboundp 'window-text-area-pixel-width))
      (window-text-area-pixel-width)        ;; xemacs
    (* (window-width) (frame-char-width)))) ;; emacs

(defun xtide-run (f-option output-buffer)
  "An internal part of xtide.el.
Run xtide on TIME, MODE and F-OPTION.
F-OPTION is either \"-ft\" for text, or \"-fp\" for png.

The output from xtide is inserted into OUTPUT-BUFFER and the
return value is 0 for success or a string describing an error.
The string includes anything xtide printed to stderr.
OUTPUT-BUFFER gets either plain text, or raw PNG image bytes.
For the latter the buffer generally should be unibyte."

  (let* ((status      nil)
         (old-point   (with-current-buffer output-buffer (point)))
         (location    (xtide-location-value))
         ;; may run xtide to get timezone, so make time string before the
         ;; "Running" message
         (time-string (xtide-with-TZ (xtide-location-time-zone location)
                                     (format-time-string "%Y-%m-%d %H:%M" xtide-time))))

    (xtide-with-temp-message "Running xtide..."
      ;; No redisplay in `call-process'.  The database lookup takes a few
      ;; seconds but it doesn't print anything to stdout to see while it runs.
      ;;
      (xtide-with-errorfile
       (xtide-with-defanged-locale
        (let ((default-directory       "/") ;; in case inherit non-existent
              (process-connection-type nil) ;; pipe
              (coding-system-for-read  (if (equal f-option "-fp")
                                           'binary       ;; png image
                                         'iso-8859-1)))  ;; text
          (setq status
                (apply 'call-process "tide"
                       nil                 ;; input /dev/null
                       (list output-buffer ;; stdout
                             errorfile)    ;; stderr
                       nil                 ;; no redisplay
                       ;; arguments
                       f-option
                       "-m"  xtide-output-mode
                       "-tw" (number-to-string (1- (window-width)))
                       "-gw" (number-to-string
                              (xtide-window-text-area-pixel-width))
                       "-b"  time-string
                       (append
                        (and xtide-aspect-ratio
                             (list "-ga" (number-to-string xtide-aspect-ratio)))
                        (and location
                             (list "-l" location))
                        xtide-run-switches)))))

       ;; when the location is not found the exit status is still 0, but
       ;; there's nothing written to stdout
       (unless (and (eq 0 status)
                    (with-current-buffer output-buffer
                      (/= (point) old-point)))
         (setq status
               (concat (with-temp-buffer
                         (let ((coding-system-for-read 'iso-8859-1))
                           (insert-file-contents errorfile))
                         (buffer-string))
                       "\n"
                       (if (stringp status)
                           status
                         (format "Exit %s" status))))))

      status)))

(defun xtide-run-insert-image ()
  "An internal part of xtide.el.
Insert xtide output in PNG image format into the current buffer.
If there's an error running xtide then its error message is
inserted in the buffer instead."

  (xtide-set-buffer-multibyte nil)
  (let ((status (xtide-run "-fp" (current-buffer))))
    (cond ((not (equal 0 status))
           (xtide-set-buffer-multibyte t)
           (insert status))

          ((eval-when-compile (fboundp 'make-glyph))
           ;; XEmacs 21.  Image in a glyph displayed by an extent for
           ;; `invisible' plus `end-glyph' on top of raw bytes.
           (let* ((glyph (make-glyph (vector 'png :data (buffer-string)))))
             (set-extent-properties (make-extent (point-min) (point-max))
                                    (list 'invisible t
                                          'end-glyph glyph))))

          (t
           ;; Emacs.  `display' text property on top of raw data bytes.
           (add-text-properties (point-min) (point-max)
                                `(display (image :type png
                                                 :data ,(buffer-string))))))))

(defun xtide-run-insert-text ()
  "An internal part of xtide.el.
Insert xtide output in text format in the current buffer.
TIME is in a `current-time' style list.
MODE is an xtide -m option string, like \"g\" for graph.
If there's an error running xtide, its error message is inserted in the
buffer instead."

  (xtide-set-buffer-multibyte t)
  (let* ((status (xtide-run "-ft" (current-buffer))))
    (unless (eq 0 status)
      (insert status))))

(eval-and-compile ;; so can get docstring during byte-compile
(defun xtide-forward-6hour ()
  "Go forward 6 hours in the tidal display."
  (interactive)
  (eval-and-compile ;; also at compile time to quieten the byte compiler
    (require 'time-date))
  ;; `time-add' is not in emacs 21 (only emacs 22), so go through floating
  ;; point seconds
  (setq xtide-time (seconds-to-time (+ (if (eval-when-compile
                                             (fboundp 'float-time))
                                           (float-time xtide-time)
                                         (time-to-seconds xtide-time))
                                       21600)))
  (xtide-mode-redraw))
)

(eval-and-compile ;; so can get docstring during byte-compile
(defun xtide-backward-6hour ()
  "Go back 6 hours in the tidal display."
  (interactive)
  (require 'time-date)
  (setq xtide-time (subtract-time xtide-time '(0 21600 0)))
  (xtide-mode-redraw))
)

(eval-and-compile ;; so can get docstring during byte-compile
(defun xtide-show-about-location ()
  "Show information about this tidal location.
This includes coordinates, timezone, datum, and possibly comments
on the source of the harmonics.

If you have tcd-format.el installed, and button.el of Emacs 22 up,
then the harmonics file .tcd shown is buttonized to go to the raw
data (press \\[forward-button] to go to the button then \\<button-map>\\[push-button] to follow it)."
  (interactive)
  (setq xtide-output-mode "a")
  (xtide-mode-redraw))
)

(eval-and-compile ;; so can get docstring during byte-compile
(defun xtide-show-banner ()
  "Show horizontal text banner style graph of tides."
  (interactive)
  (setq xtide-output-mode "b")
  (xtide-mode-redraw))
)

(eval-and-compile ;; so can get docstring during byte-compile
(defun xtide-show-calendar ()
  "Show calendar of a few days tide times."
  (interactive)
  (setq xtide-output-mode "c")
  (xtide-mode-redraw))
)

(eval-and-compile ;; so can get docstring during byte-compile
(defun xtide-show-calendar-alternative ()
  "Show calendar of tide times, in alternative format of days in columns."
  (interactive)
  (setq xtide-output-mode "C")
  (xtide-mode-redraw))
)

(eval-and-compile ;; so can get docstring during byte-compile
(defun xtide-show-graph ()
  "Show tides in graph of height.
This is the default display mode."
  (interactive)
  (setq xtide-output-mode "g")
  (xtide-mode-redraw))
)

(eval-and-compile ;; so can get docstring during byte-compile
(defun xtide-show-medium-rare ()
  "Show tides in \"medium rare\" format, meaning a list of hourly heights."
  (interactive)
  (setq xtide-output-mode "m")
  (xtide-mode-redraw))
)

(eval-and-compile ;; so can get docstring during byte-compile
(defun xtide-show-plain-times ()
  "Show plain tide times format (including sun and moon times)."
  (interactive)
  (setq xtide-output-mode "p")
  (xtide-mode-redraw))
)

(eval-and-compile ;; so can get docstring during byte-compile
(defun xtide-show-raw-times ()
  "Show tide times in raw format, meaning heights against Unix time seconds."
  (interactive)
  (setq xtide-output-mode "r")
  (xtide-mode-redraw))
)

(eval-and-compile ;; so can get docstring during byte-compile
(defun xtide-show-statistics ()
  "Show some statistics about tide heights at this time and location."
  (interactive)
  (setq xtide-output-mode "s")
  (xtide-mode-redraw))
)

(eval-and-compile ;; so can get docstring during byte-compile
(defun xtide-zoom-in-horizontal ()
  "Zoom in on the tide graph, horizontally.
This goes in to see a curve in more detail, showing less time on
the screen."
  (interactive)
  (setq xtide-aspect-ratio (* xtide-aspect-ratio 1.7))
  (xtide-mode-redraw))
)

(eval-and-compile ;; so can get docstring during byte-compile
(defun xtide-zoom-out-horizontal ()
  "Zoom out of the tide graph, horizontally.
This goes out to see more time horizontally, squeezing up the
tidal curves."
  (interactive)
  (setq xtide-aspect-ratio (/ xtide-aspect-ratio 1.7))
  (xtide-mode-redraw))
)

(defconst xtide-zoom-output-modes '("b" "g")
  "An internal part of xtide.el.
The `xtide-output-mode' values where zooming does something.")

(defun xtide-tcd-goto-location (location)
  "An internal part of xtide.el.
Move point to the section for LOCATION in a tide database dump.
The current buffer should be a `tcd-format-decode' style restored
dump of a .tcd file.  LOCATION (a string) is tide station name.

Note: Don't use this function, it's likely to be renamed and
moved to tcd-format.el or a hypothetical tcd-mode.el."

  (goto-char ;; leave point unchanged if location not found
   (save-excursion

     ;; location name on a line of its own like
     ;;     Abott Harbour, Nova Scotia
     ;; or within the XML part like
     ;;     <subordinatestation name="York Harbor, Maine"
     (goto-char (point-min))
     (or (re-search-forward (concat "^\\(<subordinatestation name=\"\\)?"
                                    (regexp-quote location)
                                    "\\(\"\\|$\\)$") nil t)
         (error "Location not found: '%s'" location))

     ;; then skip back past "#" comments about the location
     (forward-line -1)
     (while (and (looking-at "#")
                 (not (bobp)))
       (forward-line -1))
     (unless (looking-at "#")
       (forward-line))

     (point)))
  (recenter 0))

(defun xtide-button-goto-tcd (button)
  ;; checkdoc-params: (button)
  "An internal part of xtide.el.
Button action to follow link to tcd harmonics file.
This function is designed for use as the `action' in a
`make-button'.

This is only meant for use when tcd-format.el is installed, which
means `tcd' in `format-alist', since otherwise a .tcd file is
binary and can't be usefully visited."

  (find-file (button-label button))
  (xtide-tcd-goto-location (button-get button 'xtide-location)))

(eval-and-compile ;; so can get docstring during byte-compile
(defun xtide-location-select ()
  "Go to an `xtide-location-mode' selection buffer to choose a location."
  (interactive)

  (let ((target (and (xtide--derived-mode-p 'xtide-mode)
                     (buffer-name))))
    (if (get-buffer xtide-location-buffer)
        (switch-to-buffer xtide-location-buffer) ;; existing buffer
      ;; new buffer
      (switch-to-buffer (get-buffer-create xtide-location-buffer))
      (xtide-insert-locations)
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (goto-char (point-min))
      (xtide-location-mode))

    ;; start at selected location, if there is one
    (let ((location (xtide-location-value)))
      (when location
        (goto-char (point-min))
        (re-search-forward (concat "^" (regexp-quote location)) nil t)
        (beginning-of-line)))

    (set (make-local-variable 'xtide-location-mode-target) target)
    (message "Press `%s' to select location"
             (key-description (where-is-internal 'xtide-location-go
                                                 overriding-local-map t)))))
)

(defun xtide-mode-redraw ()
  "An internal part of xtide.el.
Redraw contents of XTide display based on current variables."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (if (and (string-equal xtide-output-mode "g")
             (xtide-display-images-p)
             (xtide-image-type-available-p 'png))
        (xtide-run-insert-image)
      (xtide-run-insert-text))

    ;; For "a" "About" style, buttonize the harmonics filename if
    ;; tcd-format.el is available.  If not available then don't buttonize
    ;; since the .tcd is a binary file and there's nothing much to be done
    ;; with it.
    ;;
    ;; `erase-buffer' above removes any previous button, so can just make a
    ;; new one here every time.
    ;;
    ;; button.el is new in Emacs 22 up.  XEmacs 21.4.22 has a `make-button'
    ;; in cal-compat.el (part of M-x calendar), but that file doesn't seem
    ;; to make a full button.el replacement and probably isn't meant for use
    ;; outside use.  In particular its make-button errors out unless
    ;; diary-face has been loaded from diary-lib.el, or something.
    ;;
    (when (and (string-equal xtide-output-mode "a")
               (assq 'tcd format-alist)
               (featurep 'button)) ;; new in emacs22
      (goto-char (point-min))
      (when (re-search-forward "In file\\s-*\\(.*\\)" nil t)
        (make-button (match-beginning 1) (match-end 1)
                     'action 'xtide-button-goto-tcd
                     'xtide-location xtide-location))))

  (set-buffer-modified-p nil)
  ;; no cursor in graph mode, because a cursor on top of an image looks
  ;; pretty ordinary, in particular a blinking cursor is very annoying
  (if (eval-when-compile (boundp 'cursor-type)) ;; not in xemacs
      (setq cursor-type (if (equal "g" xtide-output-mode)
                            nil
                          t)))
  (goto-char (point-min))

  ;; Output modes which are graphical or ASCII art are `truncate-lines' t so
  ;; wraparound doesn't make a mess.  Output modes which are text are
  ;; `truncate-lines' nil so that long lines are not missed.  In practice
  ;; only "a" about is likely to have long lines.
  (setq truncate-lines
        (not (member xtide-output-mode '("a"      ;; about
                                         "m"      ;; medium rare
                                         "p"      ;; plain times
                                         "r"      ;; raw times
                                         "s"))))) ;; statistics

;; ENHANCE-ME: A `buffer-stale-function' could check if the current time "+"
;; mark moved.  The default text mode width is 15 minutes per char, the
;; `xtide-aspect-ratio' or user's ~/.xtide.xml would change that.  Could
;; also check if the current time is in fact on screen, or newly moving onto
;; or off screen.  Or might in the future have a tracking style which moved
;; with the current time.
;;
(defun xtide-revert-buffer (&optional _ignore-auto _noconfirm)
  ;; checkdoc-params: (_ignore-auto _noconfirm)
  "An internal part of xtide.el.
Refresh the xtide output.
This is designed for use from `revert-buffer-function'."
  (xtide-mode-redraw))

(defun xtide-save (filename)
  "Save XTide image or text to a file.
If the FILENAME entered exists already then prompt whether to
overwrite it.

Currently graphics images are raw PNG bytes in the buffer with
suitable display properties to show as an image.  This is
experimental and might change but is how the graphical save is
currently implemented.  \\[write-file] (`write-file') works but
it switches the buffer to a normal visit of the new file in
`image-mode' etc so you lose `xtide-mode'."

  (interactive (list (read-file-name (if xtide-buffer-multibyte-p
                                         "Save to file (text): "
                                       "Save to file (PNG): "))))
  (setq default-directory (file-name-directory filename))
  ;; emacs20 up has a CONFIRM parameter for `write-region' to overwrite an
  ;; existing file, but xemacs21 doesn't (instead a coding system parameter).
  (and (file-exists-p filename)
       (not (file-directory-p filename)) ;; write-region gives error for dir
       (not (y-or-n-p (format "File `%s' exists; overwrite? " filename)))
       (error "Cancelled"))
  (write-region (point-min) (point-max) filename))

(defvar xtide-mode-map
  (let ((m (make-sparse-keymap "XTide")))
    ;; forward-button is new in emacs22, but bind it always since don't have
    ;; anything else to offer in emacs21 and xemacs21
    (define-key m "\t"        'forward-button)
    (define-key m "q"         'kill-this-buffer)
    (define-key m "\C-x\C-s"  'xtide-save)
    (define-key m "W"         'xtide-zoom-in-horizontal)
    (define-key m "w"         'xtide-zoom-out-horizontal)
    (define-key m "s"         'xtide-show-statistics)
    (define-key m "r"         'xtide-show-raw-times)
    (define-key m "p"         'xtide-show-plain-times)
    (define-key m "m"         'xtide-show-medium-rare)
    (define-key m "l"         'xtide-location-select)
    (define-key m "g"         'xtide-show-graph)
    (define-key m "C"         'xtide-show-calendar-alternative)
    (define-key m "c"         'xtide-show-calendar)
    (define-key m "b"         'xtide-show-banner)
    (define-key m "a"         'xtide-show-about-location)
    (define-key m " "         'xtide-forward-6hour)
    (define-key m [backspace] 'xtide-backward-6hour) ;; xemacs
    (define-key m "\d"        'xtide-backward-6hour) ;; Del key
    (define-key m [left]      'xtide-backward-6hour)
    (define-key m [right]     'xtide-forward-6hour)
    m)
  "Keymap for `xtide-mode' display buffer.")

(easy-menu-define xtide-mode-menu xtide-mode-map "XTide Menu"
  ;; The docstrings of the relevant functions are used for the :help.
  ;; (when easymenu.el supports :help).
  ;; The docstring of `xtide-show-about-location' is a touch longer than
  ;; would really want for a help echo, but ok for now.
  ;;
  (eval-when-compile
    `("XTide"
      ["Location"                              xtide-location-select
       ,@(xtide-easy-menu-help (documentation 'xtide-location-select))]

      ["Forward 6 Hours"                       xtide-forward-6hour
       ,@(xtide-easy-menu-help (documentation 'xtide-forward-6hour))
       :active  (not (equal xtide-output-mode "a"))
       :keys    "<right>" ;; among the multiple keys
       ]
      ["Backward 6 Hours"                      xtide-backward-6hour
       ,@(xtide-easy-menu-help (documentation 'xtide-backward-6hour))
       :active  (not (equal xtide-output-mode "a"))
       :keys    "<left>" ;; among the multiple keys
       ]

      ["Zoom out horizontally"                 xtide-zoom-out-horizontal
       ,@(xtide-easy-menu-help (documentation 'xtide-zoom-out-horizontal))
       :active  (member xtide-output-mode xtide-zoom-output-modes)]
      ["Zoom in horizontally"                  xtide-zoom-in-horizontal
       ,@(xtide-easy-menu-help (documentation 'xtide-zoom-in-horizontal))
       :active  (member xtide-output-mode xtide-zoom-output-modes)]

      ["--" nil]
      ["About Location"                        xtide-show-about-location
       ,@(xtide-easy-menu-help (documentation 'xtide-show-about-location))
       :style     radio
       :selected  (equal xtide-output-mode "a")]
      ["Banner"                                xtide-show-banner
       ,@(xtide-easy-menu-help (documentation 'xtide-show-banner))
       :style     radio
       :selected  (equal xtide-output-mode "b")]
      ["Calendar"                              xtide-show-calendar
       ,@(xtide-easy-menu-help (documentation 'xtide-show-calendar))
       :style     radio
       :selected  (equal xtide-output-mode "c")]
      ["Calendar Alternative"                  xtide-show-calendar-alternative
       ,@(xtide-easy-menu-help (documentation 'xtide-show-calendar-alternative))
       :style     radio
       :selected  (equal xtide-output-mode "C")]
      ["Graph"                                 xtide-show-graph
       ,@(xtide-easy-menu-help (documentation 'xtide-show-graph))
       :style     radio
       :selected  (equal xtide-output-mode "g")]
      ["Medium Rare"                           xtide-show-medium-rare
       ,@(xtide-easy-menu-help (documentation 'xtide-show-medium-rare))
       :style     radio
       :selected  (equal xtide-output-mode "m")]
      ["Plain Times"                           xtide-show-plain-times
       ,@(xtide-easy-menu-help (documentation 'xtide-show-plain-times))
       :style     radio
       :selected  (equal xtide-output-mode "p")]
      ["Raw Times"                             xtide-show-raw-times
       ,@(xtide-easy-menu-help (documentation 'xtide-show-raw-times))
       :style     radio
       :selected  (equal xtide-output-mode "r")]
      ["Statistics"                            xtide-show-statistics
       ,@(xtide-easy-menu-help (documentation 'xtide-show-statistics))
       :style     radio
       :selected  (equal xtide-output-mode "s")])))

(defvar xtide-mode-tool-bar-map
  (and (eval-when-compile (boundp 'tool-bar-map)) ;; not in emacs20, xemacs21

       ;; Emacs 21 doesn't have the new `tool-bar-local-item-from-menu' to
       ;; operate on a given keymap.  Binding `tool-bar-map' tricks
       ;; `tool-bar-add-item-from-menu' into operating on this new one.
       ;;
       ;; Previously had some stuff here copying the global toolbar,
       ;; including some trickery to put the items before the Emacs 23
       ;; "help" item (which is gone from Emacs 24).  Think it's clearer
       ;; without the globals, and in particular the specific `xtide-save'
       ;; can appear instead of the global `save-buffer'.
       ;;
       (let* ((tool-bar-map (make-keymap "XTide")))
         (tool-bar-add-item "saveas" 'xtide-save 'xtide-save
                            :help "Save XTide buffer")

         (tool-bar-add-item-from-menu 'xtide-backward-6hour "left-arrow"
                                      xtide-mode-map)
         (tool-bar-add-item-from-menu 'xtide-forward-6hour "right-arrow"
                                      xtide-mode-map)

         (tool-bar-add-item-from-menu 'xtide-zoom-out-horizontal "zoom-out"
                                      xtide-mode-map)
         (tool-bar-add-item-from-menu 'xtide-zoom-in-horizontal "zoom-in"
                                      xtide-mode-map)

         (tool-bar-add-item-from-menu 'xtide-location-select "jump-to"
                                      xtide-mode-map)
         tool-bar-map))

  "Keymap for tool bar in `xtide-mode' display buffer.
This is save, left/right and zoom buttons.")

(defun xtide-font-lock-plus (limit)
  ;; checkdoc-params: (limit)
  "An internal part of xtide.el.
Font lock matcher for the \"+\" current time marker.
This is applied in \"g\" and \"b\" modes.  In other modes it
returns nil for no match."
  (and (member xtide-output-mode '("g" "b"))
       (search-forward "+" limit t)))

(defconst xtide-font-lock-keywords
  '(xtide-font-lock-plus)
  "An internal part of xtide.el.
`font-lock-keywords' for `xtide-mode'.")

(defun xtide-mode ()
  "XTide tide times display.
Note that the data shown is NOT FOR NAVIGATION.

\\{xtide-mode-map}
The initial graph shows the time now and a further day or two.
The \"+\" marker is the current time.  If you go to a wild date
or lose track then kill the buffer and restart to get back to
today.  There's no automatic redisplay as time passes, but press
\"\\[xtide-show-graph]\" to get a new graph.

Dates and times are in the timezone of the location.  This is
good for local tides, and usually makes most sense elsewhere.

When switching location the absolute time is preserved.  If
you're looking at Sydney Australia 10am Saturday and switch to
Sydney Nova Scotia then you see 7pm Friday because that's the
same moment in that timezone.  When the \"+\" marker for \"now\"
is on screen you can see it stays in the same place in the
window.

There's a tie-in to bookmark.el (for Emacs 23 up) allowing it to
save the tide location and mode to return later if desired.

There's also a tie-in to desktop.el so a tide buffer can be saved
and re-created.

----
The XTide home page is
  URL `http://www.flaterco.com/xtide'
The xtide.el home page is
  URL `http://user42.tuxfamily.org/xtide/index.html'

And see tcd-format.el for nosing around raw harmonics files.
  URL `http://user42.tuxfamily.org/tcd-format/index.html'"

  ;; This is not `(interactive)' because the interactive entrypoint is
  ;; `M-x xtide'.  It's not usual for a user to put an existing buffer into
  ;; `xtide-mode', since doing so will kill any existing contents.
  ;;
  ;; Don't think `define-derived-mode' can be asked to make a
  ;; non-interactive mode func.

  (kill-all-local-variables)

  (setq major-mode     'xtide-mode
        mode-name      "XTide")

  ;; variables with current positions, modes etc
  (set (make-local-variable 'xtide-location) (xtide-location-value))
  (make-local-variable 'xtide-output-mode)
  (make-local-variable 'xtide-time)
  (make-local-variable 'xtide-aspect-ratio)
  (or xtide-time
      (setq xtide-time (current-time)))

  (set (make-local-variable 'bookmark-make-record-function)
       'xtide-bookmark-make-record)
  (set (make-local-variable 'desktop-save-buffer)
       'xtide-mode-desktop-misc-data)

  (when (eval-when-compile (boundp 'tool-bar-map)) ;; not in xemacs21
    (set (make-local-variable 'tool-bar-map)
         xtide-mode-tool-bar-map))
  (set (make-local-variable 'font-lock-defaults)
       '(xtide-font-lock-keywords
         t	 ;; no syntactic fontification (of strings etc)
         nil   ;; no case-fold
         nil)) ;; no changes to syntax table

  (use-local-map xtide-mode-map)
  (easy-menu-add xtide-mode-menu)
  (setq buffer-read-only t)
  (set (make-local-variable 'revert-buffer-function) 'xtide-revert-buffer)

  (if (eval-when-compile (fboundp 'run-mode-hooks)) ;; xemacs21,emacs22 up
      (run-mode-hooks 'xtide-mode-hook)
    (run-hooks 'xtide-mode-hook)))

;;;###autoload
(defun xtide ()
  "Display a tides graph using XTide.
If a location has been set in either `xtide-location' or the
XTIDE_DEFAULT_LOCATION environment variable, then this function
goes straight to `xtide-mode'.  If not, then `xtide-location-select'
is used to get a location."

  (interactive)
  (cond ((not (xtide-location-value))
         ;; no location selected, start in the location mode chooser
         (xtide-location-select))

        ((get-buffer xtide-buffer)
         ;; existing xtide buffer, switch to it, inherit xtide-time if set
         (let ((time xtide-time))
           (switch-to-buffer (get-buffer xtide-buffer))
           (when time
             (set (make-local-variable 'xtide-time) time)
             (xtide-mode-redraw))))

        (t
         ;; start new xtide buffer, inherit xtide-time if set
         (let ((time (or xtide-time (current-time))))
           (switch-to-buffer (get-buffer-create xtide-buffer))
           (xtide-mode)
           (set (make-local-variable 'xtide-time) time))
         (xtide-mode-redraw))))


;;-----------------------------------------------------------------------------
;; xtide-mode bookmark.el tie-in

(defun xtide-bookmark-make-record ()
  "An internal part of xtide.el.
Return a bookmark record for `xtide-mode'.
This function is designed for use from variable
`bookmark-make-record-function' as a tie-in to bookmark.el.

The bookmark records the tide location name, output mode, and
position of point in the buffer.  The time displayed is not
recorded since it's expected that when returning to a bookmark
the current time will be more useful."

  (nconc (list xtide-location ;; location name as bookmark name
               (cons 'location     xtide-location)
               (cons 'output-mode  xtide-output-mode)
               (cons 'aspect-ratio xtide-aspect-ratio )
               (cons 'line         (count-lines (point-min) (point)))
               (cons 'column       (current-column))
               '(handler         . xtide-bookmark-jump))
         (bookmark-make-record-default t))) ;; NO-FILE, point only

;; autoload so it's available to saved bookmarks when xtide.el isn't yet loaded
;;;###autoload
(defun xtide-bookmark-jump (bmk)
  "Jump to bookmark record BMK for `xtide-mode'.
This is designed for use from the bookmark records created by
`xtide-location-bookmark-make-record'."
  (if (get-buffer xtide-buffer)
         (switch-to-buffer (get-buffer xtide-buffer))
    (switch-to-buffer (get-buffer-create xtide-buffer))
    (xtide-mode))
  (setq xtide-location     (bookmark-prop-get bmk 'location)
        xtide-output-mode  (bookmark-prop-get bmk 'output-mode)
        xtide-aspect-ratio (bookmark-prop-get bmk 'aspect-ratio))
  (xtide-mode-redraw)
  (goto-char (point-min))
  (forward-line   (or (bookmark-prop-get bmk 'line) 0))
  (move-to-column (or (bookmark-prop-get bmk 'column) 0)))


;;-----------------------------------------------------------------------------
;; calendar tie-in

;;;###autoload
(defun xtide-calendar-tides (&optional arg)
  "Display tides for the calendar cursor date.
With a prefix ARG, prompt for a date to display."
  (interactive "P")

  ;; Quieten the byte compiler a little.
  ;; This func is called from calendar mode anyway.
  (eval-when-compile (require 'calendar))

  ;; XTide starts its display a little before the time asked, so give
  ;; 2:30am to get the display starting at about 1:00am.  If the location
  ;; selector comes up to choose a location the requested time is still
  ;; saved so it's shown after selecting a location.
  ;;
  ;; emacs23 renames `extract-calendar-day' to `calendar-extract-day' etc,
  ;; pointlessly incompatible and irritating.  The original names might
  ;; not have been fantastic, but the only thing changing them can achieve
  ;; is to stop perfectly good programs from working.
  ;;
  (let* ((date (if arg
                   (calendar-read-date)
                 (calendar-cursor-to-date t)))

         (time (encode-time
                0 30 2 ;; 2:30am
                (if (eval-when-compile (fboundp 'calendar-extract-day))
                    (calendar-extract-day   date)
                  (extract-calendar-day     date))
                (if (eval-when-compile (fboundp 'calendar-extract-month))
                    (calendar-extract-month date)
                  (extract-calendar-month   date))
                (if (eval-when-compile (fboundp 'calendar-extract-year))
                    (calendar-extract-year  date)
                  (extract-calendar-year    date)))))

    (if (xtide-location-value)
        (progn
          ;; xtide display top window, but calendar in the bottom remains
          ;; the selected window
          (set (make-local-variable 'xtide-time) time)
          (xtide)
          (switch-to-buffer calendar-buffer)
          (kill-local-variable 'xtide-time)
          (display-buffer xtide-buffer))

      ;; no location, go to selector in top window
      (other-window 1)
      (xtide-location-select)
      ;; target time when selection made
      (set (make-local-variable 'xtide-time) time))))

;;;###autoload
(defun xtide-calendar-setups ()
  "Setup xtide additions to `calendar-mode'.
This binds the `T' key in the calendar to `xtide-calendar-tides'.

This function can be used from `calendar-load-hook', though doing
so means you load xtide immediately on starting the calendar.
Copying the `define-key' into your .emacs can avoid that.
\(Copying the menu entry setups would be required too, if not
using the autoloads.)

Beware that if calendar.el has already been loaded by something
else then `calendar-load-hook' doesn't run, so make sure you
`add-hook' early enough in your .emacs file."

  (define-key calendar-mode-map "T" 'xtide-calendar-tides))

;;;###autoload
(custom-add-option 'calendar-load-hook 'xtide-calendar-setups)

;; The Calendar/Tides menu entry is added unconditionally.  It won't upset
;; anyone's key binding preferences.
;;
;; Must be string (eval-after-load "calendar" ...) not symbol
;; (eval-after-load 'calendar ...) because Emacs 23.1 has (provide
;; 'calendar) at the start of the file, wrongly claiming the feature is
;; available when it's not yet.
;;
;; calendar.el recommends `calendar-load-hook' for key bindings.  That's
;; usually ok from .emacs, but if calendar.el is already loaded then adding
;; to the hook from here does nothing.
;;
;; Emacs 21 `define-key-after' must be a single event, so lookup the
;; sub-keymap and act there.  That lookup copes with the different symbol
;; name in Emacs 23 too.
;;
;;;###autoload
(when (eval-when-compile (fboundp 'define-key-after)) ;; GNU emacs
  (eval-after-load "calendar"
    '(let ((keymap (or (lookup-key calendar-mode-map
                                   [menu-bar moon]) ;; emacs20,21,22
                       (lookup-key calendar-mode-map
                                   [menu-bar Sun/Moon])))) ;; emacs23
       ;; allow for keymap not found in case yet another change in the
       ;; keymap symbols, and since emacs20 calendar.el only makes the
       ;; menu-bar under `window-system'
       (when (keymapp keymap)
         (define-key-after keymap [xtide]
           '(menu-item "Tides" xtide-calendar-tides
                       :help "View tides for the selected day using XTide.")
           t))))) ;; add at end of keymap

;; xemacs21 has `add-menu-button' which must be called from within a
;; `calendar-mode' buffer to add to the Calendar menu.  Tie it onto
;; `calendar-add-menus' so as not to junk up `calendar-mode-hook'.
;;
;; Any existing `calendar-mode' buffers won't be updated.  Could go through
;; the buffer-list looking for calendar buffers and add-menu-button in each,
;; but hope the autoload is good enough for now.
;;
;;;###autoload
(when (eval-when-compile (fboundp 'add-menu-button)) ;; xemacs21
  (defadvice calendar-add-menus (after xtide activate)
    "Add a Calendar/Tides menu entry for xtide.el."
    (add-menu-button '("Calendar") ["Tides" xtide-calendar-tides])))


;;-----------------------------------------------------------------------------
;; desktop.el save/restore
;;
;; When `desktop-create-buffer' finds an autoload such as `xtide-mode' in a
;; saved desktop it loads the .el for that mode function, so don't need
;; autoloads for `xtide-mode-desktop-restore' or the
;; `desktop-buffer-mode-handlers' setup.

(defun xtide-mode-desktop-restore (_file-name buffer-name misc-data)
  "Restore `xtide-mode' buffer for desktop.el.
This is designed for use from `desktop-buffer-mode-handlers'.

The `xtide-location' and `xtide-output-mode' in the xtide buffer
are restored.  The time displayed is not restored, but instead
moves to the current time."
  ;; checkdoc-params: (_file-name buffer-name misc-data)

  ;; `xtide-location' and `xtide-output-mode' are buffer-local variables but
  ;; are saved through the MISC-DATA mechanism so that they can be applied
  ;; before (xtide-mode-redraw) here.  MISC-DATA is an alist to allow for
  ;; possible future extensions or changes here.

  (switch-to-buffer buffer-name)
  (xtide-mode)
  (setq xtide-location    (cdr (assq 'xtide-location    misc-data)))
  (setq xtide-output-mode (cdr (assq 'xtide-output-mode misc-data)))
  (xtide-mode-redraw))

(defun xtide-mode-desktop-misc-data (_desktop-dirname)
  "An internal part of xtide.el.
Return a list of data to save an `xtide-mode' buffer in desktop.el."
  ;; checkdoc-params: (_desktop-dirname)
  (list (cons 'xtide-location    xtide-location)
        (cons 'xtide-output-mode xtide-output-mode)))

(defun xtide-location-mode-desktop-restore (_file-name buffer-name _misc-data)
  "Restore `xtide-location-mode' buffer for desktop.el.
This is designed for use from `desktop-buffer-mode-handlers'."
  ;; checkdoc-params: (_file-name buffer-name _misc-data)

  (switch-to-buffer buffer-name)
  (xtide-insert-locations)
  (set-buffer-modified-p nil)
  (setq buffer-read-only t)
  (xtide-location-mode)
  (current-buffer))

(eval-after-load "desktop"
  '(when (boundp 'desktop-buffer-mode-handlers) ;; new in emacs22
    (add-to-list 'desktop-buffer-mode-handlers
                 '(xtide-mode . xtide-mode-desktop-restore))
    (add-to-list 'desktop-buffer-mode-handlers
                 '(xtide-location-mode . xtide-location-mode-desktop-restore))
    (add-to-list 'desktop-locals-to-save 'xtide-location-mode-target)))


;;-----------------------------------------------------------------------------
;; xtide-mode buffers become fairly useless after unload-feature, but don't
;; think need to kill them outright.  But re-loading xtide.el after an
;; unload makes a bit of a mess of variables, including keymaps, of existing
;; buffers.

;; (defun xtide-unload-function ()
;;   "Kill xtide buffers on unloading xtide.el."
;;   ;; `xtide-calendar-tides' is left in the calendar mode map, but that's ok
;;   ;; as its an autoload
;;   (if (get-buffer xtide-buffer)
;;       (kill-buffer xtide-buffer))
;;   (if (get-buffer xtide-location-buffer)
;;       (kill-buffer xtide-location-buffer))
;;   nil) ;; and do normal unload-feature actions too


;;-----------------------------------------------------------------------------

;; LocalWords: XTide XTide's customizable fc fp ft ga SeaGreen xml easymenu
;; LocalWords: downcasing latin moascf Dstr cc customization Niihau Nonopapa
;; LocalWords: tcd buttonized Ret outfile el ascii env var radians png
;; LocalWords: whitespace eg stderr unrecognised filename Scotia fallback
;; LocalWords: lat Customize pm autoloads unibyte

;; Coding cookie here instead of in the first line so as to keep it out of
;; finder-by-keyword summary.
;;
;; Local variables:
;; coding: latin-1
;; End:

(provide 'xtide)

;;; xtide.el ends here
