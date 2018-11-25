;;; wx-look.el --- lookup wxWidgets functions in its manual.

;; Copyright 2004, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2015, 2016 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 7
;; Keywords: tools, c, wxWidgets
;; URL: http://user42.tuxfamily.org/wx-look/index.html

;; wx-look.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; wx-look.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; `M-x wx-lookup-symbol' displays the documentation for wxWidgets 2.8
;; functions and variables from its HTML manual, similar to
;; M-x info-lookup-symbol does for info files.  See the `wx-lookup-symbol'
;; docstring below for more.
;;
;; Some mangling is applied to help wxPerl, and wxPython should mostly work
;; too.  But exactly what can be looked up depends on the wx.htx/wx.hhk
;; index file.
;;
;; The wxWidgets 2.6 manual doesn't seem to have a suitable index, so cannot
;; be used.  The wxWidgets 3.0 has a html index but don't really want to
;; parse it.
;;
;; See gtk-look.el for similar on the Gtk/Gnome etc manuals.
;; See Perl Padre::Plugin::wxWidgets for a POD-ized copy of the Wx manual.

;;; Install:

;; Put wx-look.el in one of your `load-path' directories, and in your
;; .emacs add
;;
;;     (autoload 'wx-lookup-symbol "wx-look" nil t)
;;
;; This makes M-x wx-lookup-symbol available, but you'll probably want to
;; bind it to a key.  C-h C-k is one possibility, being close to C-h C-i for
;; `info-lookup-symbol'.  For instance to do that globally,
;;
;;     (define-key global-map [?\C-h ?\C-k] 'wx-lookup-symbol)

;;; Emacsen:

;; Designed for Emacs 21 and higher.  Works in XEmacs 21 and Emacs 20.

;;; History:

;; Version 1 - the first version
;; Version 2 - wx-lookup-manual-directory default newest wx
;;           - try wx.hhk if no wx.htx
;; Version 3 - fix let-bind of `regexp'
;;           - use 'cl for dolist in emacs20
;; Version 4 - some wxPerl aliases
;; Version 5 - initial wx optional for guess, more aliases
;; Version 6 - cl macros only when needed
;; Version 7 - prefer the wx 2.8 manual as it works

;;; Code:

(require 'browse-url)

(eval-when-compile
  (unless (fboundp 'dolist)
    (require 'cl))) ;; for `dolist' in emacs20

;;----------------------------------------------------------------------------
;; with-auto-compression-mode

;; same in gtk-look.el
(eval-when-compile
  (defmacro wx-lookup--with-auto-compression (&rest body)
    "An internal part of wx-look.el.
This macro doesn't exist when running byte compiled.

Evaluate BODY forms with `auto-compression-mode' enabled.
This is `with-auto-compression-mode' made available in XEmacs 21
and with a workaround for Emacs 21.

\(Emacs 21.4 `with-auto-compression-mode' expands to call
`jka-compr-installed-p' but that function isn't autoloaded.  As a
workaround `jka-compr' is loaded here explicitly.)"

    (if (eval-when-compile
          (fboundp 'with-auto-compression-mode))
        ;; emacs21 up
        `(progn
           (require 'jka-compr) ;; for emacs21 running byte compiled
           (with-auto-compression-mode ,@body))

      ;; xemacs21 and emacs20 don't have `with-auto-compression-mode'.  They
      ;; also doesn't have an `auto-compression-mode' variable (which emacs21
      ;; up has) for the current state, hence calling `jka-compr-installed-p'.
      ;;
      `(progn
         (eval-and-compile ;; quieten the byte compiler
           (require 'jka-compr))
         (let ((wx-lookup--with-auto-compression--old-state
                (jka-compr-installed-p)))

           ;; turn on if not already on
           ;; xemacs21 has a toggle-auto-compression which takes a "no message"
           ;; arg, but not emacs21
           (if (not wx-lookup--with-auto-compression--old-state)
               (auto-compression-mode 1))

           (unwind-protect
               (progn ,@body)
             ;; turn off again if it was off before
             (if (not wx-lookup--with-auto-compression--old-state)
                 (auto-compression-mode -1))))))))

;;----------------------------------------------------------------------------
;; variables

;;;###autoload
(defgroup wx-lookup nil
  "wxWidgets documentation lookup."
  ;; pruning "wx-lookup-" off `wx-lookup-manual-directory' looks ok
  :prefix "wx-lookup-"
  :group 'languages ;; same as info-look
  :link  '(url-link
           :tag "wx-look.el home page"
           "http://user42.tuxfamily.org/wx-look/index.html"))

(defvar wx-lookup-cache 'uninitialized
  "Cache of targets for `wx-lookup-symbol'.
The current format is (NAME . (FILE . LINK)).
NAME is a function or datatype string.
FILE is a .html filename, without a directory.
LINK is an anchor within FILE.
Being an alist means this can be passed to `completing-read' and
friends.

LINK is separate from FILE to save a little memory by having one
of each FILE string.

If `wx-lookup-cache' is not yet initialized the value is the
symbol `uninitialized'.  `wx-lookup-cache-init' should be called
to ensure it's initialized.")

;; must have wx-lookup-reset defined here for wx-lookup-manual-directory
;; initial :set to use
(defun wx-lookup-reset ()
  "Discard data cached for `wx-lookup-symbol'.
This can be used to get newly installed documents recognised."
  (interactive)
  (setq wx-lookup-cache 'uninitialized))

;; note this defcustom is after `wx-lookup-reset' so the :set method here
;; can call `wx-lookup-reset' immediately for setting the initial value
(defcustom wx-lookup-manual-directory
  ;; eg. on Debian "/usr/share/doc/wx2.8-doc/wx-manual.html/"
  (let ((dir-list (directory-files "/usr/share/doc/"
                                   t  ;; absolute filenames
                                   "wx.*-doc")))
    ;; sort by version number parts so wx2.8 ahead of wx2.6, etc
    ;; Crib: spit-string no OMIT-NULLS arg in emacs21
    (setq dir-list (sort dir-list
                         (lambda (x y)
                           (let ((x-list (delete "" (split-string x "[^0-9]+")))
                                 (y-list (delete "" (split-string y "[^0-9]+"))))
                             (while (and x-list y-list
                                         (equal (car x-list) (car y-list)))
                               (setq x-list (cdr x-list))
                               (setq y-list (cdr y-list)))
                             (or (not y-list)
                                 (and x-list
                                      (> (string-to-number (car x-list))
                                         (string-to-number (car y-list)))))))))

    ;; /wx-manual.html/ subdirectory
    (setq dir-list (mapcar (lambda (dir)
                             (setq dir (expand-file-name "wx-manual.html/" dir))
                             (and (file-exists-p dir) dir))
                           dir-list))
    (car (delq nil dir-list)))

  "Directory containing wxWidgets manual.
If you change this variable then call `wx-lookup-reset' to clear
previously cached data.  If you use the `customize' interface
then this is done automatically."

  :set (lambda (sym val)
         (custom-set-default sym val)
         (wx-lookup-reset))
  :type  'directory
  :group 'wx-lookup)

(defvar wx-lookup-history nil
  "Symbols previously looked up with `wx-lookup-symbol'.")


;;----------------------------------------------------------------------------
;; generic

(defun wx-lookup-string-suffix-ci-p (suff str)
  "Return non-nil if string SUFF is a suffix of STR, ignoring case."
  (and (>= (length str) (length suff))
       (if (eval-when-compile (fboundp 'compare-strings)) ;; not in xemacs21
           (eq t (compare-strings str (- (length str) (length suff)) nil
                                  suff nil nil
                                  t)) ;; ignore case
         (setq suff (upcase suff))
         (setq str (upcase str))
         (string-equal suff
                       (substring str (- (length str) (length suff)))))))
;; case conversion tables not considered constant by byte-opt.el, so not pure
(eval-when-compile
  (put 'wx-lookup-string-suffix-ci-p 'side-effect-free t))

(defsubst wx-lookup-assoc-string-ignore-case (key alist)
  "Lookup a string KEY in ALIST, case insensitively."
  (if (eval-when-compile (fboundp 'assoc-string))
      (assoc-string key alist t)
    (assoc-ignore-case key alist)))

(defun wx-lookup-browse-url-other-window (url)
  ;; checkdoc-params: (url)
  "`browse-url' but in an \"other-window\" if it uses an Emacs window."

  ;; this convoluted code copes with various types of browser that
  ;; `browse-url' might invoke: perhaps an external program in its own X
  ;; window, perhaps something in an emacs buffer.  And when in a buffer it
  ;; might switch to an "other window" itself or just use the current
  ;; window; and perhaps the current buffer (and window) is already the
  ;; browser buffer
  ;;
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((orig-win-conf (current-window-configuration))
        (orig-buffer   (current-buffer))
        (orig-window   (selected-window)))
    (unwind-protect
        (with-temp-buffer
          (let ((dummy-buf (current-buffer)))
            (switch-to-buffer-other-window (current-buffer))
            (let ((other-window (get-buffer-window dummy-buf)))
              (select-window other-window)
              (browse-url url)

              (cond ((and (eq dummy-buf (window-buffer other-window))
                          (eq orig-buffer (window-buffer orig-window)))
                     ;; browse-url didn't touch the buffers, it left the
                     ;; original and dummy current, so it's an external
                     ;; window system program; let window configs go back
                     ;; how they were
                     )

                    ((eq orig-buffer (window-buffer other-window))
                     ;; browse-url has changed dummy-buf to orig-buf in the
                     ;; other-window, which means we were in the browser
                     ;; buffer already and shouldn't have split with "other
                     ;; window"; so put window configs back how they were,
                     ;; but don't change point in the browser buffer as
                     ;; that's the new document position
                     (let ((point (window-point other-window)))
                       (set-window-configuration orig-win-conf)
                       (setq orig-win-conf nil)
                       (with-current-buffer orig-buffer
                         (goto-char point))))

                    (t
                     ;; browse-url has selected a buffer; but it might have
                     ;; done "other window" itself (eg. w3m-browse-url
                     ;; does); don't let two "other window" invocations
                     ;; leave our original buffer at the bottom and the
                     ;; browser at the top, instead force our orig-window
                     ;; back to orig-buffer, and let the other window we
                     ;; made show the browser buffer
                     (setq orig-win-conf nil)
                     (let ((browser-buffer (window-buffer other-window)))
                       (select-window other-window)
                       (switch-to-buffer browser-buffer)
                       (select-window orig-window)
                       (switch-to-buffer orig-buffer)))))))

      ;; restore windows for non-buffer browse-url or an error
      (if orig-win-conf
          (set-window-configuration orig-win-conf)))))


;;----------------------------------------------------------------------------
;; cached wx.htx or wx.hhk contents

(defvar wx-lookup-alias-list
  '(("wxRichTextCtrl::GetTextAttrStyle"     . "wxRichTextCtrl::GetStyle")
    ("wxRichTextCtrl::GetTextAttrExStyle"   . "wxRichTextCtrl::GetStyle")
    ("wxRichTextCtrl::GetRichTextAttrStyle" . "wxRichTextCtrl::GetStyle")

    ("wxRichTextCtrl::GetTextAttrExStyleForRange"   . "wxRichTextCtrl::GetStyleForRange")
    ("wxRichTextCtrl::GetRichTextAttrStyleForRange" . "wxRichTextCtrl::GetStyleForRange")

    ("wxRichTextCtrl::GetTextAttrUncombinedStyle"     . "wxRichTextCtrl::GetUncombinedStyle")
    ("wxRichTextCtrl::GetTextAttrExUncombinedStyle"   . "wxRichTextCtrl::GetUncombinedStyle")
    ("wxRichTextCtrl::GetRichTextAttrUncombinedStyle" . "wxRichTextCtrl::GetUncombinedStyle")

    ("wxTextCtrl::GetTextAttrStyle"   . "wxTextCtrl::GetStyle")

    ("Wx::ExecuteCommand"      . "::wxExecute")
    ("Wx::ExecuteArgs"         . "::wxExecute")
    ("Wx::ExecuteStdout"       . "::wxExecute")
    ("Wx::ExecuteStdoutStderr" . "::wxExecute")

    ("wxDataFormat::newNative" . "wxDataFormat::wxDataFormat")

    ("wxListCtrl::FindItemAtPos" . "wxListCtrl::FindItem")
    ("wxListCtrl::FindItemData"  . "wxListCtrl::FindItem")

    ("wxListCtrl::InsertStringItem"      . "wxListCtrl::InsertItem")
    ("wxListCtrl::InsertImageItem"       . "wxListCtrl::InsertItem")
    ("wxListCtrl::InsertImageStringItem" . "wxListCtrl::InsertItem")

    ("wxTreeCtrl::GetPlData" . "wxTreeCtrl::GetItemData")
    ("wxTreeCtrl::GetPyData" . "wxTreeCtrl::GetItemData")

    ("wxTreeCtrl::SetPlData" . "wxTreeCtrl::SetItemData")
    ("wxTreeCtrl::SetPyData" . "wxTreeCtrl::SetItemData")

    ("wxCaret::GetSizeWH"    . "wxCaret::GetSize") ;; wxPerl
    ("wxCaret::GetSizeTuple" . "wxCaret::GetSize") ;; wxPython, maybe

    ("wxDC::GetSizeWH"    . "wxDC::GetSize") ;; wxPerl
    ("wxDC::GetSizeTuple" . "wxDC::GetSize") ;; wxPython

    ("wxWindow::GetSizeWH"    . "wxWindow::GetSize") ;; wxPerl
    ("wxWindow::GetSizeTuple" . "wxWindow::GetSize") ;; wxPython

    ("wxFULLSCREEN_NOMENUBAR"   . "wxTopLevelWindow::ShowFullScreen")
    ("wxFULLSCREEN_NOTOOLBAR"   . "wxTopLevelWindow::ShowFullScreen")
    ("wxFULLSCREEN_NOSTATUSBAR" . "wxTopLevelWindow::ShowFullScreen")
    ("wxFULLSCREEN_NOBORDER"    . "wxTopLevelWindow::ShowFullScreen")
    ("wxFULLSCREEN_NOCAPTION"   . "wxTopLevelWindow::ShowFullScreen")
    ("wxFULLSCREEN_ALL"         . "wxTopLevelWindow::ShowFullScreen")

    ("wxImage::FindHandlerName"    . "wxImage::FindHandler") ;; wxPerl
    ("wxImage::FindHandlerExtType" . "wxImage::FindHandler")
    ("wxImage::FindHandlerType"    . "wxImage::FindHandler")
    ("wxImage::FindHandlerMime"    . "wxImage::FindHandler")

    ("wxBitmap::FindHandlerName"    . "wxBitmap::FindHandler") ;; wxPerl
    ("wxBitmap::FindHandlerExtType" . "wxBitmap::FindHandler")
    ("wxBitmap::FindHandlerType"    . "wxBitmap::FindHandler")
    ("wxBitmap::FindHandlerMime"    . "wxBitmap::FindHandler")

    ("Wx::gettext" . "::wxGetTranslation"))

  "Some aliases for Wx functions.
This is currently wxPerl and a few wxPython things not otherwise
indexed.  You'd prefer the Wx manual had these itself, but until
then these aliases help you find them.")

(defun wx-lookup-cache-init ()
  "Initialize `wx-lookup-cache', if not already done.
The return is the `wx-lookup-cache' list."
  (when (eq wx-lookup-cache 'uninitialized)

    (unless wx-lookup-manual-directory
      (error "`wx-lookup-manual-directory' not set (or nothing in default locations)"))

    ;; build in `result' and only after that set wx-lookup-cache, so as not
    ;; to leave a half built cache if killed (C-g) part-way through
    (let ((result nil)
          (case-fold-search nil)
          matchnum-file
          matchnum-link
          matchnum-name
          filename
          regexp)

      ;; Debian package wx2.8-doc 2.8.10.1-3 has wx.htx.gz.  Perhaps it
      ;; won't be compressed in the future though, as the other wx.hhp.gz
      ;; etc don't work with the wx help system when compressed.
      ;; /usr/share/doc/wx2.8-doc/wx-manual.html/wx.htx.gz
      (cond ((or (file-exists-p
                  (setq filename (expand-file-name
                                  "wx.hhk" wx-lookup-manual-directory)))
                 (file-exists-p
                  (setq filename (expand-file-name
                                  "wx.hhk.gz" wx-lookup-manual-directory))))
             ;; entries like
             ;;     <OBJECT type="text/sitemap">
             ;;      <param name="Local" value="wx_wxapp.html#wxappctor">
             ;;      <param name="Name" value="wxApp::wxApp">
             ;;     </OBJECT>
             ;;
             (setq regexp "<OBJECT[^>]*>\\s-+\
<param name=\"Local\" value=\"\\([^\">#]+\\)#\\([^\">]+\\)\">\\s-+\
<param name=\"Name\" value=\" *\\([^\">]+\\)\">")
             (setq matchnum-file 1
                   matchnum-link 2
                   matchnum-name 3))

            ((or (file-exists-p
                  (setq filename (expand-file-name
                                  "wx.htx" wx-lookup-manual-directory)))
                 (file-exists-p
                  (setq filename (expand-file-name
                                  "wx.htx.gz" wx-lookup-manual-directory))))
             ;; lines like "wxappctor|wx_wxapp.html|wxApp::wxApp\n"
             ;;              anchor    file          symbol
             ;;
             ;; \\s-* discard leading whitespace on "...| GetExtension" in
             ;; wx2.8.10.1.  Does that space have any meaning?  Similar
             ;; \\s-*\n discard trailing whitespace on some like "Data
             ;; provider "
             ;;
             (setq regexp "^\
\\([^|\n]*\\)\
|\\([^|\n]*\\)\
|\\s-*\\([^|\n]*\\)\
\\(|.*\\)?\\s-*\n")
             (setq matchnum-link 1
                   matchnum-file 2
                   matchnum-name 3))

            (t
             (error "wxWidgets manual not found")))

      (wx-lookup--with-auto-compression
       (with-temp-buffer

         (let (file-list prev-name prev-file prev-link)
           ;; In Emacs 21.3 jka-compr doesn't erase the buffer properly with
           ;; the "replace" argument to `insert-file-contents,' so use
           ;; `erase-buffer' instead.  (Fixed in Emacs 22.)
           (erase-buffer)
           (insert-file-contents filename)
           (message "Processing %s" filename)

           (while (re-search-forward regexp nil t)
             (let ((link (match-string matchnum-link))
                   (file (match-string matchnum-file))
                   (name (match-string matchnum-name)))

               ;; &lt; and &gt; from Foo<> templating
               ;; hopefully no need for full entity decode, just these
               (dolist (pair '(("&lt;"  . "<")
                               ("&gt;"  . ">")
                               ("&amp;" . "&")))
                 (while (string-match (car pair) name)
                   (setq name (concat (substring name 0 (match-beginning 0))
                                      (cdr pair)
                                      (substring name (match-end 0))))))

               ;; "EVT_NOTIFY(event, id, func)" -> "EVT_NOTIFY"
               ;; drop the formal params on the few event macros
               (if (string-match "(" name)
                   (setq name (substring name 0 (match-beginning 0))))

               ;; "wxwindowgetname|wx_wxwindow.html|wxWindow::GetName"
               ;; "wxwindowgetname|wx_wxwindow.html|GetName"
               ;;
               ;; Often but not always (as of wx 2.8.10.1) methods are the
               ;; fully qualified name followed by the method alone pointing
               ;; to the same place.  Drop the unqualified one.  This check
               ;; assumes the unqualified is second, which is a bit rash but
               ;; enough for now.
               ;;
               (unless (and (equal name prev-name)
                            (equal file prev-file)
                            (equal link prev-link))
                 (setq prev-name (if (string-match "::" name)
                                     (substring name (match-end 0))
                                   name))
                 (setq prev-file file)
                 (setq prev-link link)

                 ;; share strings for filenames
                 (setq file (or (car (member file file-list))
                                (car (push file file-list))))

                 (push (cons name (cons file link))
                       result)))))))

      ;; wxPerl methods without their own index entries as of Wx 2.8.12.1
      (dolist (alias wx-lookup-alias-list)
        (unless (assoc (car alias) result)
          (let ((pair (assoc (cdr alias) result)))
            (if pair
                (push (cons (car alias) (cdr pair))
                      result)))))

      (setq wx-lookup-cache result)))
  wx-lookup-cache)


;;-----------------------------------------------------------------------------
;; symbol thing-at-point

(defun wx-lookup-symbol-method-candidates (method)
  "Return a list of wxWidgets symbols (strings) having METHOD as a suffix.
For example \"SetValue\" gives a list
\(\"wxTextCtrl::SetValue\" \"wxComboCtrl::SetValue\" ...)."

  (wx-lookup-cache-init)
  (let ((ret (and (wx-lookup-assoc-string-ignore-case method wx-lookup-cache)
                  (list method))))    ;; whole name if exists
    (setq method (concat "::" method)) ;; and otherwise final after ::
    (dolist (elem wx-lookup-cache ret)
      (let ((name (car elem)))
        (if (wx-lookup-string-suffix-ci-p method name)
            (setq ret (cons name ret)))))))

(defun wx-lookup-canonicalize-symbol (str)
  "Return canonicalized wxWidgets function name etc from string STR.
Some transformations are applied to turn wxPerl into the C++
style of the wxWidgets manual.  For example \"Wx::Frame->new\"
becomes the C++ constructor \"WxFrame::WxFrame\".  There's
probably some similar wanted wxPython (much of it is close enough
already).

Not much attention is paid to upper/lower case in the transformed
return.  It's mostly left like the input, anticipating a
case-insensitive lookup by `completing-read' in
`wx-lookup-symbol-interactive-arg'."

  ;; note xemacs21 replace-match doesn't take a "subexp" arg when
  ;; replacing in a string (only in a buffer)

  (when str
    (let* ((case-fold-search nil))

      ;; wxPerl "Wx::Event::EVT_FOO" -> "EVT_FOO" fully qualified macro call
      ;; Though as of wx 2.8.10.1 there's only a few EVT macros indexed.
      (if (string-match "\\`Wx::Event::\\(EVT[^:]*\\)\\'" str)
          (setq str (match-string 1 str)))

      ;; wxPerl "Wx::wxFOO" -> "wxFOO"
      (if (string-match "\\`Wx::wx" str)
          (setq str (replace-match "wx" t nil str)))

      ;; "wxID_FOO" -> "wxID"
      ;; Seems "wxID" is the only index entry for the predefined IDs, go to
      ;; there if nothing for the full id name.
      (and (string-match "\\`wxID_" str)
           (not (wx-lookup-assoc-string-ignore-case str (wx-lookup-cache-init)))
           (setq str "wxID"))

      ;; wxPerl "Wx::AboutBox" -> "::wxAboutBox" global func, if it exists
      (and (string-match "\\`Wx::" str)
           (let ((newstr (concat "::wx" (substring str (match-end 0)))))
             (and (wx-lookup-assoc-string-ignore-case newstr
                                                      (wx-lookup-cache-init))
                  (setq str newstr))))

      ;; wxPerl "Wx::GetDisplaySize" -> "::wxDisplaySize" global func, if it
      ;; exists
      (and (string-match "\\`Wx::Get" str)
           (let ((newstr (concat "::wx" (substring str (match-end 0)))))
             (and (wx-lookup-assoc-string-ignore-case newstr
                                                      (wx-lookup-cache-init))
                  (setq str newstr))))

      ;; wxPerl "Wx::Foo" -> "wxFoo" class name
      ;; if not a wxPerl-specific entry
      (and (string-match "\\`Wx::" str)
           (not (wx-lookup-assoc-string-ignore-case str (wx-lookup-cache-init)))
           (setq str (replace-match "wx" t nil str)))

      ;; wxActivateEvent
      (and (string-equal str "EVT_HIBERNATE")
           (not (wx-lookup-assoc-string-ignore-case str (wx-lookup-cache-init)))
           (setq str "wxCloseEvent"))

      ;; wxCloseEvent
      (and (member str '("EVT_QUERY_END_SESSION"
                         "EVT_END_SESSION"))
           (not (wx-lookup-assoc-string-ignore-case str (wx-lookup-cache-init)))
           (setq str "wxCloseEvent"))

      ;; wxCommandEvent
      (and (member str '("EVT_BUTTON"
                         "EVT_CHECKBOX"
                         "EVT_CHOICE"
                         "EVT_COMBOBOX"
                         "EVT_LISTBOX"
                         "EVT_LISTBOX_DCLICK"
                         "EVT_MENU" ;; wxCommandEvent not wxMenuEvent
                         "EVT_MENU_RANGE"
                         "EVT_CONTEXT_MENU"
                         "EVT_RADIOBOX"
                         "EVT_RADIOBUTTON"
                         "EVT_SCROLLBAR"
                         "EVT_SLIDER"
                         "EVT_TEXT"
                         "EVT_TEXT_ENTER"
                         "EVT_TEXT_MAXLEN"
                         "EVT_TOGGLEBUTTON"
                         "EVT_TOOL"
                         "EVT_TOOL_RANGE"
                         "EVT_TOOL_RCLICKED"
                         "EVT_TOOL_RCLICKED_RANGE"
                         "EVT_TOOL_ENTER"))
           (not (wx-lookup-assoc-string-ignore-case str (wx-lookup-cache-init)))
           (setq str "wxCommandEvent"))

      ;; wxFocusEvent
      (and (member str '("EVT_SET_FOCUS"
                         "EVT_KILL_FOCUS"))
           (not (wx-lookup-assoc-string-ignore-case str (wx-lookup-cache-init)))
           (setq str "wxFocusEvent"))

      ;; wxJoystickEvent macros
      ;; EVT_JOYSTICK_EVENTS() is picked up by the general case
      (and (string-match "\\`EVT_JOY_" str)
           (not (wx-lookup-assoc-string-ignore-case str (wx-lookup-cache-init)))
           (setq str "wxJoystickEvent"))

      ;; wxKeyEvent macros
      (and (string-equal "EVT_CHAR" str)
           (not (wx-lookup-assoc-string-ignore-case str (wx-lookup-cache-init)))
           (setq str "wxKeyEvent"))

      ;; wxMouseEvent macros
      (and (member str '("EVT_LEFT_DOWN"
                         "EVT_LEFT_UP"
                         "EVT_LEFT_DCLICK"
                         "EVT_MIDDLE_DOWN"
                         "EVT_MIDDLE_UP"
                         "EVT_MIDDLE_DCLICK"
                         "EVT_RIGHT_DOWN"
                         "EVT_RIGHT_UP"
                         "EVT_RIGHT_DCLICK"
                         "EVT_MOTION"
                         "EVT_ENTER_WINDOW"
                         "EVT_LEAVE_WINDOW"
                         "EVT_MOUSEWHEEL"
                         "EVT_MOUSE_EVENTS"))
           (not (wx-lookup-assoc-string-ignore-case str (wx-lookup-cache-init)))
           (setq str "wxMouseEvent"))

      ;; EVT_FOO -> wxFooEvent, if EVT not indexed but the event exists
      (and (string-match "\\`EVT_" str)
           (not (wx-lookup-assoc-string-ignore-case str (wx-lookup-cache-init)))
           (let ((parts (cdr (split-string str "_"))))
             (while parts
               (let ((pair (wx-lookup-assoc-string-ignore-case
                            (apply 'concat "wx" (append parts '("event")))
                            (wx-lookup-cache-init))))
                 (if pair
                     (setq str (car pair) parts nil)
                   (setq parts (butlast parts 1)))))))

      ;; wxPerl "WxFoo->new" -> "WxFoo::WxFoo" constructor
      (if (string-match "->new\\'" str)
          (setq str (concat (substring str 0 (match-beginning 0))
                            "::" (substring str 0 (match-beginning 0)))))

      ;; wxPerl "Foo->bar->quux" becomes "quux", since can't know what class
      ;; the bar() method returns
      (when (string-match "->.*->" str)
        (setq str (substring str (match-end 0))))

      ;; wxPerl "WxFoo->bar" becomes "WxFoo::bar" if that's known, or "bar"
      ;; if not.  Checking that WxFoo is a known class allows for method
      ;; calls on subclasses.  FIXME: Is there many class methods apart from
      ;; new() ?
      ;;
      (when (string-match "->" str)
        (let ((pre  (substring str 0 (match-beginning 0)))
              (post (substring str (match-end 0))))
          (setq str (concat pre "::" post))

          (unless (wx-lookup-assoc-string-ignore-case
                   str (wx-lookup-cache-init))
            (setq str post))))

      ;; Foo -> wxFoo, if Foo doesn't exist, and isn't a method, and if
      ;; wxFooCtrl exists
      (and (not (string-match "\\`[Ww]x" str))
           (not (wx-lookup-assoc-string-ignore-case str (wx-lookup-cache-init)))
           (not (wx-lookup-symbol-method-candidates str))
           (wx-lookup-assoc-string-ignore-case (concat "wx" str)
                                               (wx-lookup-cache-init))
           (setq str (concat "wx" str)))))

  str)

(defun wx-lookup-symbol-bounds-of-thing-at-point ()
  "Find the bounds of a `wx-lookup-symbol' symbol at point.
The return is a pair (BEG . END) of buffer positions, or nil if
point is not at or within a symbol."

  ;; For perl style "Wx::Foo->bar" demand the left side start with a capital
  ;; letter like "Wx::Label->new", so as to distinguish it from an instance
  ;; method like "$label->SetText".  For an instance method the return is
  ;; just the "SetText" part (when point is within that "SetText").
  ;;
  ;; The desired match is the one earliest in the buffer which covers point.
  ;; `re-search-backwards' is no good for that, as it stops at the first
  ;; match, not the earliest possible.  `thing-at-point-looking-at' is
  ;; better, but the optional "(...)?" perl class part ends up with only a
  ;; partial match (like only the "Store" part of "TreeStore->"), not the
  ;; biggest surrounding point.  So the strategy is to look forwards from
  ;; the start of the line for the first which covers point.
  ;;
  (save-excursion
    (let ((case-fold-search nil)
          (orig-point (point))
          (re "\\(\\([A-Z][a-zA-Z0-9_:]*[a-zA-Z0-9_]\\)?->\\)?\\([a-zA-Z_][a-zA-Z0-9_:]*[a-zA-Z0-9]\\)"))
      (beginning-of-line)
      (let (found)
        (while (and (setq found (re-search-forward re
                                                   (line-end-position)
                                                   t))
                    (< (match-end 0) orig-point)))
        (and found
             (<= (match-beginning 0) orig-point)
             (if (match-beginning 2)
                 ;; Foo->bar return "Foo->bar"
                 (cons (match-beginning 0) (match-end 0))
               ;; $_[0]->bar return "bar" alone
               (cons (match-beginning 3) (match-end 3))))))))

(put 'wx-lookup-symbol 'bounds-of-thing-at-point
     'wx-lookup-symbol-bounds-of-thing-at-point)


;;-----------------------------------------------------------------------------
;; completing-help.el tie-in

(defvar completing-help-groups) ;; quieten the byte compiler

(defvar wx-lookup-completing-help-group
  '(:predicate wx-lookup-completing-help-p
    :get       wx-lookup-completing-help-get
    :info-head " - "
    :info-tail ""))

(defun wx-lookup-completing-help-p ()
  "Return non-nil when completing from `wx-lookup-cache'."
  (and (not (eq wx-lookup-cache 'uninitialized))
       (eq minibuffer-completion-table wx-lookup-cache)))

(defun wx-lookup-completing-help-get (str)
  "Return a help string for wxWidgets symbol STR.
Currently this is merely \"in wx_wxapp.html #wxappctor\", being
the filename and anchor.  Not very informative, but at least a
hint."
  (let ((elem (assoc str (wx-lookup-cache-init))))
    (and elem
         (format "in %s #%s"
                 (cadr elem)     ;; file
                 (cddr elem))))) ;; link

(eval-after-load "completing-help"
  '(if (boundp 'wx-lookup-completing-help-group) ;; in case we're unloaded
       (add-to-list 'completing-help-groups
                    'wx-lookup-completing-help-group)))

;;----------------------------------------------------------------------------
;; icicles.el tie-in

(defvar icicle-candidate-help-fn) ;; quieten the byte compiler

(defun wx-lookup-icicle-help (str)
  "Display help for STR in `icicles-mode'.
Page descriptions are shown with `icicle-msg-maybe-in-minibuffer'
because they're just a single line."
  (let ((desc (wx-lookup-completing-help-get str)))
    (if desc
        (icicle-msg-maybe-in-minibuffer "%s" desc))))


;;-----------------------------------------------------------------------------
;; read and lookup

(defvar wx-lookup-initial-completion-list nil
  "Initial completions to display for `wx-lookup-symbol-interactive-arg'.
This is let-bound by `wx-lookup-symbol-interactive-arg' and is
nil at other times.")

(defun wx-lookup-display-initial-completion-list ()
  "Display initial method completions for `wx-lookup-symbol'."
  (if (>= (length wx-lookup-initial-completion-list) 2)
      (with-output-to-temp-buffer "*Completions*"
        (display-completion-list wx-lookup-initial-completion-list)))
  (setq wx-lookup-initial-completion-list nil))

(add-hook 'minibuffer-setup-hook
          'wx-lookup-display-initial-completion-list)

(defun wx-lookup-symbol-interactive-arg ()
  "Symbol argument read for interactive `wx-lookup-symbol'.
Return a one-element list (\"symbol\") which is the user-selected
symbol name string."
  (let* ((completion-ignore-case t)
	 (enable-recursive-minibuffers t)
         (icicle-candidate-help-fn 'wx-lookup-icicle-help)
	 (default (wx-lookup-canonicalize-symbol
                   (thing-at-point 'wx-lookup-symbol)))
         (wx-lookup-initial-completion-list
          (and default
               (wx-lookup-symbol-method-candidates default))))
    (if (= 1 (length wx-lookup-initial-completion-list))
        ;; one method match, offer full name as the default
        (setq default (car wx-lookup-initial-completion-list)))

    (let ((symbol (wx-lookup-canonicalize-symbol
                   (completing-read
                    (if default
                        (format "Describe symbol (default %s): " default)
                      "Describe symbol: ")
                    (wx-lookup-cache-init)
                    nil  ;; predicate
                    t    ;; require-match
                    nil  ;; initial-input
                    'wx-lookup-history
                    default))))
      (list (or symbol default "")))))

;;;###autoload
(defun wx-lookup-symbol (symbol)
  "Lookup wxWidgets documentation for SYMBOL.
SYMBOL is a string name of a function, constant, etc in the
wxWidgets 2.8 manual, sought using its wx.htx or wx.hhk index.
The manual is displayed in HTML with `browse-url'.  See
`wx-lookup-manual-directory' for the location of the manual.

The lookup tries first case-sensitively, then insensitively, for
ease of typing a name.

Interactively SYMBOL is prompted for, with completions from the
manual index.  The default is the function, method, etc at point.
Transformations are applied to make a C++ name from forms used in
wxPerl, and wxPython should work too.

For wxPerl, with point on a Perl call like \"Wx::Frame->new\" the
default offered is \"WxFrame::WxFrame\" which is the Wx C++
constructor.  This is independent of the major mode, so you can
have code in Perl and comments in C++, or vice versa.  If
`browse-url' displays in a buffer you can even lookup from the
browser buffer if it's not linked already, such as in sample
code.

When point is on a \"method\" name like just \"AddControl\" the
default is expanded to a full \"wxToolBar::AddControl\" if
unique, or if there's multiple candidates then a *Completions*
window is presented which you can switch to with \\<minibuffer-local-completion-map>\\[switch-to-completions] and select
from in the usual way.

`browse-url' is used to display the documentation.  If it
displays in an Emacs buffer (like `w3m' does) then that's put in
an \"other window\" below the current window, similar to
`info-lookup' on Info docs.  You can customize
`browse-url-browser-function' to select a the viewer.  With
regexps there you can even have one browser for the Wx manual
\"file:///usr/share/doc/wx2.8-doc/...\" and another browser for
other things.

The `completing-read' for the symbol demands a match, since
wx-lookup-symbol can only go to the links available in the
wx.htx/wx.hhk index.  The full set of wxWidgets symbols is fairly
big, so you might try one of the completions add-ons like Icicles
to help searching or browsing.

The wx-look home page is
URL `http://user42.tuxfamily.org/wx-look/index.html'"

  (interactive (wx-lookup-symbol-interactive-arg))
  (wx-lookup-cache-init)
  (let ((entry (or (assoc symbol wx-lookup-cache) ;; exact match preferred
                   (wx-lookup-assoc-string-ignore-case
                    symbol wx-lookup-cache))))    ;; otherwise case-fold
    (or entry
        (error "Unknown symbol %s" symbol))
    (wx-lookup-browse-url-other-window (concat "file://"
                                               (expand-file-name
                                                (cadr entry)     ;; filename
                                                wx-lookup-manual-directory)
                                               "#"
                                               (cddr entry))))) ;; link


;;-----------------------------------------------------------------------------

(defun wx-look-unload-function ()
  "Undo wx-look.el stuff."

  (put 'wx-lookup-symbol 'bounds-of-thing-at-point nil)

  ;; mustn't leave an unbound var in `completing-help-groups' or it will
  ;; error out (in completing-help.el 3.13)
  (if (boundp 'completing-help-groups)
      (setq completing-help-groups
            (remq 'wx-lookup-completing-help-group completing-help-groups)))

  ;; and do normal unload-feature actions too,
  ;; including `minibuffer-setup-hook' addition removed automatically
  nil)
 
;; LocalWords: docstring datatype gz usr canonicalized func wxWidgets
;; LocalWords: regexps el Gtk html filename lookup AddControl SetValue
;; LocalWords: wxTextCtrl wxComboCtrl wxToolBar wxPerl Wx WxFrame wx wxapp
;; LocalWords: wxappctor htx hhk ons wxPython versa autoloaded ized
;; LocalWords: initialize initialized

(provide 'wx-look)

;;; wx-look.el ends here
