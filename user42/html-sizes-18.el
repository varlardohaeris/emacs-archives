;;; html-sizes.el --- maintain file sizes shown for html links

;; Copyright 2007, 2009, 2010, 2011, 2012, 2013, 2015, 2016, 2017 Kevin Ryde
;;
;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 18
;; Keywords: convenience, hypermedia, html
;; URL: http://user42.tuxfamily.org/html-sizes/index.html
;; EmacsWiki: HtmlMode
;;
;; html-sizes.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; html-sizes.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; `M-x html-sizes-update' updates
;;     * file sizes shown in HTML, based on local file sizes
;;     * <img width=123 height=123> image sizes
;;     * <meta name="generator"> header tag to say Emacs
;;     * <a charset=""> attribute for links to text files
;; See the `html-sizes-update' docstring for details.

;;; Install:

;; Put html-sizes.el in one of your `load-path' directories, and in your
;; .emacs add
;;
;;     (autoload 'html-sizes-update "html-sizes" nil t)
;;
;; There's an autoload cookie for this if you use `update-file-autoloads'
;; and friends.

;;; Emacsen:

;; Designed for Emacs 21 up.  Works in XEmacs 21.

;;; History:

;; Version 1 - the first version
;; Version 2 - html-sizes-link-table to help multiple sites
;; Version 3 - insert or update <meta> generator string
;; Version 4 - correction to `throw' args for "q" quit
;; Version 5 - use \\= for single perform-replace
;; Version 6 - add/update charset="" on links to text files
;; Version 7 - add/update <img> width= and height=
;; Version 8 - fix for relative filenames in image links
;; Version 9 - fix the `provide'
;;           - trap xemacs21 file-truename errors
;; Version 10 - fixes for non-existent image files and cached images
;; Version 11 - href allow other attributes and tags
;; Version 12 - fixes for adding new width,height and for meta generator
;; Version 13 - .ps and .pdf not text files
;; Version 14 - size of .html count its <img> sizes too
;; Version 15 - sizes in megabytes, preserve existing decimals
;; Version 16 - fix for filesize regexp
;; Version 17 - add pdf page counts
;; Version 18 - fix for size update when decrease in length

;;; Code:

(eval-when-compile
  (unless (and (fboundp 'declare)
               (fboundp 'dolist)        ;; for emacs20
               (fboundp 'ignore-errors))
    (require 'cl)))

;;;###autoload
(defgroup html-sizes nil "Html Sizes."
  :prefix "html-sizes-"
  ;; chucked in the sgml group along with `html-mode-hook' for want of
  ;; somewhere better
  :group 'sgml
  :link  '(url-link
           :tag "html-sizes.el home page"
           "http://user42.tuxfamily.org/html-sizes/index.html"))

(defcustom html-sizes-update-query t
  "Whether `html-sizes-update' should ask before changing the buffer.
t is recommended so that you can see what it proposes to change."
  :type  'boolean
  :group 'html-sizes)

(defcustom html-sizes-link-table nil
  "Alist of (BASEURL . LOCALDIR) for mapping link URLs.
For example if you keep files for ftp://foo.org/downloads/ in
local directory /my/download/stuff/ then add an entry

    (\"ftp://foo.com/downloads/\" . \"/my/download/stuff/\")

`html-sizes-update' will then look in that local directory for
any such ftp links it finds.  This is good if you maintain more
than one site or you direct downloads to ftp instead of http."

  :type  '(alist :key-type string :value-type directory)
  :group 'html-sizes)

(defcustom html-sizes-generator
  (let ((str (emacs-version)))
    (if (string-match "\\`.*?[0-9.]+" str)
        (match-string 0 str)
      str))
  "META generator string to insert in the HTML.
The default is the first part of variable `emacs-version', like

    \"GNU Emacs 23.1.1\"

This is then added or updated in the HTML as

    <meta name=\"generator\" content=\"GNU Emacs 24.1.1\">

An empty string (or nil) means don't insert or change any meta
generator tag.

Putting Emacs like this is a bit of fun.  Nothing much will look
at it.  If you use one of the fancy html modes you could work its
version string in as well or instead."

  :type  'string
  :group 'html-sizes)

;;-----------------------------------------------------------------------------

(eval-when-compile
  (put     'html-sizes-with-file 'lisp-indent-function 1))
(eval-when-compile
  (defmacro html-sizes-with-file (filename &rest body)
    ;; checkdoc-params: (filename body)
    "An internal part of html-sizes.el.
This macro does not exist when running byte compiled.

Execute BODY in a buffer containing the contents of FILENAME.
If FILENAME is already in a buffer then use it, otherwise read
into a temporary buffer."

    ;; like `lm-with-file' from lisp-mnt.el, but find-file-noselect here looks
    ;; at more things for the coding system, maybe, perhaps ...

    (declare (indent 1)
             (debug t))
    `(let ((html-sizes-with-file--file ,filename)
           html-sizes-with-file--killbuf)
       (unwind-protect
           (with-current-buffer
               (or (get-file-buffer html-sizes-with-file--file)
                   (setq html-sizes-with-file--killbuf
                         (find-file-noselect html-sizes-with-file--file)))
             (save-excursion
               ,@body))
         (if (buffer-live-p html-sizes-with-file--killbuf)
             (kill-buffer html-sizes-with-file--killbuf))))))

(eval-when-compile
  (put 'html-sizes-warn 'byte-compile-format-like t))
(defun html-sizes-warn (format &rest args)
  "Show a warning message formatted per FORMAT and ARGS.
This is `lwarn' if available, or just `message' if not."
  (if (eval-when-compile (fboundp 'lwarn))
      ;; emacs22 up
      (apply 'lwarn 'html-sizes :error format args)
    ;; emacs21
    (apply 'message format args)))

(defun html-sizes-file-charset (filename)
  "An internal part of html-sizes.el.
Return the MIME charset for FILENAME, or nil if unknown or all ascii.

An existing buffer visiting FILENAME is used, or a temporary
visit if not in a buffer.  The coding system is taken from
`buffer-file-coding-system', which means Emacs coding cookies etc
in the file take effect.

Compressed files under `auto-compression-mode' get the charset of
the uncompressed contents.  Not sure if that's wanted, but it
doesn't affect anything as yet."

  (html-sizes-with-file filename
    (goto-char (point-min))
    (and (re-search-forward "[^\000-\177]" nil t)
         (let ((symbol (coding-system-get buffer-file-coding-system
                                          :mime-charset)))
           (and symbol
                (symbol-name symbol))))))

(defun html-sizes-text-filename-p (filename)
  "An internal part of html-sizes.el.
Return non-nil if FILENAME is probably a text file, just from its
name.

Currently this is done by asking `find-operation-coding-system'
what coding system it would use for `insert-file-contents'.  If
any of the following then reckon it not a text file,

    binary
    raw-text-unix
    no-conversion

Compressed files .gz etc are not text.  .html and .deb are
specifically excluded.

.ps and .pdf are specifically excluded.  PDF is often binary
compressed but the coding system in Emacs 24.3 is `undecided'.
Visiting either for a charset check may start and stop
`doc-view-mode' and the messages from that can hide a
`query-replace' in the minibuffer."

  (let ((case-fold-search nil))
    (and (not (string-match "\\.\\(deb\\|html\\|pdf\\|ps\\)" filename))
         ;; car is the read coding system
         (not (memq (car (find-operation-coding-system
                          'insert-file-contents filename))
                    '(no-conversion binary raw-text-unix))))))

(defun html-sizes-link-apply (link)
  "An internal part of html-sizes.el.
Apply `html-sizes-link-table' to possibly change LINK."
  (dolist (elem html-sizes-link-table)
    (when (string-match (concat "\\`" (regexp-quote (car elem)))
                        link)
      (setq link (concat (cdr elem)
                         (substring link (match-end 0))))))
  link)

(eval-when-compile
  (put 'html-sizes-u32be-to-number 'side-effect-free t))
(eval-when-compile
  (put 'html-sizes-u32be-to-number 'pure t))
(defun html-sizes-u32be-to-number (str)
  "An internal part of html-sizes.el.
Convert 4-char big-endian byte STR to an integer."
  (+ (* (aref str 0) 256 256 256)
     (* (aref str 1) 256 256)
     (* (aref str 2) 256)
     (aref str 3)))

(defun html-sizes-image-size-png (filename)
  "An internal part of html-sizes.el.
Return a list (WIDTH HEIGHT) which is the size of a PNG image FILENAME.
If FILENAME is not PNG format then return nil."
  (with-temp-buffer
    (if (eval-when-compile (fboundp 'set-buffer-multibyte))
        (set-buffer-multibyte nil))  ;; emacs, not in xemacs
    (insert-file-contents-literally filename)
    ;;                              1  2           3           4           5              6
    (when (re-search-forward "IHDR\\(\\(.\\|\n\\)\\(.\\|\n\\)\\(.\\|\n\\)\\(.\\|\n\\)\\)\\(\\(.\\|\n\\)\\(.\\|\n\\)\\(.\\|\n\\)\\(.\\|\n\\)\\)" nil t)
      (let ((width  (match-string 1))
            (height (match-string 6)))
        (list (html-sizes-u32be-to-number width)
              (html-sizes-u32be-to-number height))))))

(defun html-sizes-image-size (filename)
  "An internal part of html-sizes.el.
Return a list (WIDTH HEIGHT) which is the size of an image FILENAME.
If FILENAME doesn't exist or is not a recognised image format
then return nil.

This function can always get the size of PNG, but for other
formats it uses `image-size' which means only formats compiled
into Emacs and only running in a GUI.  (The way `image-size'
requires a GUI is a pity since the image libraries can give info
without displaying.)"

  ;; `expand-file-name' so that `create-image' doesn't search under
  ;; `data-directory'/images etc which it normally does to find an icon
  (setq filename (expand-file-name filename))

  ;; check the file exists before trying `create-image'.  `create-image'
  ;; returns an image for a non-existent file with image-size (30 . 30).
  ;; Is that the broken image glyph thing?
  (and (file-exists-p filename)
       (or (let ((size (ignore-errors
                         (let ((image (create-image
                                       (expand-file-name filename))))
                           ;; refresh in case previously loaded up
                           (image-refresh image t)
                           (image-size image t)))))
             (and size (list (car size) (cdr size))))

           (html-sizes-image-size-png filename))))


;;-----------------------------------------------------------------------------

(defun html-sizes-file-size (filename)
  "An internal part of html-sizes.el.
Return the size of FILENAME in bytes, or nil if not found.

If FILENAME is a symlink then it is followed to the size of the
actual target filename."

  ;; xemacs 21.4.22 `file-truename' throws an error if /foo/bar/quux has a
  ;; file "foo" or "bar" instead of directories there.  Treat that as nil
  ;; the same as no such file "quux".
  (let ((attrs (ignore-errors
                 (file-attributes (file-truename filename)))))
    (and attrs
         (nth 7 attrs))))

(defun html-sizes-file-size-aggregate (filename)
  "An internal part of html-sizes.el.
Return the size of FILENAME plus any contained images, in bytes,
or return nil if FILENAME not found.

If FILENAME is a .htm or .html then the sizes of any <img> images
it contains is included, thus giving an aggregate size.
`html-sizes-warn' messages are given for any missing image files."

  (let ((size (html-sizes-file-size filename))
        (case-fold-search t))
    (when (and size
               (string-match "\\.html?\\'" filename))
      (html-sizes-with-file filename
        (goto-char (point-min))
        (while (re-search-forward "<[ \t\r\n]*\
img[ \t\r\n]*[^>]*\
src=\\(\
\\([^'\">][^> \t\r\n]*\\)\
\\|\
'\\([^'> \t\r\n]*\\)\
\\|\
\"\\([^\"> \t\r\n]*\\)\
\\)" nil t)
          (let ((target (or (match-string 2)
                            (match-string 3)
                            (match-string 4))))
            (setq target (html-sizes-link-apply target))
            (when (not (string-match "\\`[a-z]+:" filename))
              (setq target (expand-file-name
                            target (file-name-directory filename)))
              (let ((img-bytes (html-sizes-file-size target)))
                (message "%S %d" target img-bytes)
                (if img-bytes
                    (setq size (+ size img-bytes))
                  (html-sizes-warn "Missing file %S" target))))))))
    size))

(defun html-sizes-file-size-string (filename decimals unit suffix)
  "An internal part of html-sizes.el.
Return a string which is the size of FILENAME.
DECIMALS is an integer how many decimal places (possibly 0).
UNIT is a string \"k\",\"K\",\"m\",\"M\".
SUFFIX is a string to append to the result, usually \"k\" or
\"kbytes\" etc.

The size is rounded to the nearest DECIMALS many places, except
that a non-zero size is never rounded to 0."

  (let* ((size           (html-sizes-file-size-aggregate filename))
         (unit-digits    (or (cdr (assoc unit '(("k" . 3) ("K" . 3)
                                                ("m" . 6) ("M" . 6))))
                             0))
         (trim-digits    (max 0 (- unit-digits decimals)))
         (power     (expt 10 trim-digits))

         (size-mod       (mod size power)))
    (setq size (/ size power))

    (cond ((and (= 0 size) (/= 0 size-mod))
           (setq size 1)) ;; don't round non-zero to zero
          ((>= (* 2 size-mod) power)
           (setq size (1+ size)))) ;; round to nearest

    ;; no more decimals than the unit, eg. max 3 when kbytes
    (setq decimals (min unit-digits decimals))

    (if (= 0 decimals)
        (format "%d%s" size suffix)

      (setq power    (expt 10 decimals))
      (setq size-mod (mod size power))
      (setq size     (/ size power))
      (format (concat "%d.%0" (number-to-string decimals) "d%s")
              size size-mod suffix))))

(defun html-sizes-file-pages (filename)
  "An internal part of html-sizes.el.
Return the number of pages in pdf FILENAME, as a string \"123\".
If FILENAME doesn't exist or is not a recognised pdf then return
nil.

Tries pdfinfo, qpdf or Perl Image::ExifTool as described in
`html-sizes-update'."

  (or
   (with-temp-buffer
     (ignore-errors ;; if pdfinfo program doesn't exist or pdf bad
       (let ((coding-system-for-read 'utf-8)) ;; pdfinfo sends utf-8 by default
         (call-process "pdfinfo"
                       nil               ;; stdin /dev/null
                       (current-buffer)  ;; stdout+stderr
                       nil               ;; redisplay
                       filename))
       ;; (message "%s" (buffer-string))
       (goto-char (point-min))
       (re-search-forward "Pages:\\s-*\\([0-9]+\\)")
       (match-string 1)))

   (with-temp-buffer
     (ignore-errors ;; if qpdf program doesn't exist or pdf bad
       (call-process "qpdf"
                     nil               ;; stdin /dev/null
                     (current-buffer)  ;; stdout+stderr
                     nil               ;; redisplay
                     "--show-npages"
                     filename)
       ;; (message "%s" (buffer-string))
       (goto-char (point-min))
       (and (looking-at "\\([0-9]+\\)")
            (match-string 1))))

   (with-temp-buffer
     (ignore-errors ;; if perl or module doesn't exist or pdf bad
       (call-process "perl"
                     nil               ;; stdin /dev/null
                     (current-buffer)  ;; stdout+stderr
                     nil               ;; redisplay
                     "-MImage::ExifTool=:Public"
                     "-e"
                     "print values %{ImageInfo($ARGV[0],['PageCount'])}"
                     filename)
       ;; (message "%s" (buffer-string))
       (goto-char (point-min))
       (and (looking-at "\\([0-9]+\\)")
            (match-string 1))))))

;;-----------------------------------------------------------------------------

;; dynamic bindings for counts
(defvar html-sizes--total)
(defvar html-sizes--differ)

(defun html-sizes-replace (beg end str)
  "An internal part of html-sizes.el.
Query replace text at BEG to END with string STR.
Throw 'quit if user answers \"q\"."
  (setq html-sizes--total (1+ html-sizes--total))
  (let ((got (buffer-substring beg end)))
    (unless (equal got str)
      (save-excursion
        (setq html-sizes--differ (1+ html-sizes--differ))
        (goto-char beg)
        (unless (perform-replace
                 (concat "\\=" (regexp-quote got)) ;; old
                 str                               ;; new
                 html-sizes-update-query
                 t     ;; regexp
                 nil)  ;; don't demand word boundary
          ;; `perform-replace' returns nil for "q" to quit, if querying
          (if html-sizes-update-query
              (throw 'quit nil)))))))


;;;###autoload
(defun html-sizes-update ()
  "Update file sizes for downloadable links, and more.
In a web page a download can be

    <a href=\"foo.tar.gz\">foo.tar.gz</a> (6.5mbytes, gzipped tar)
or
    <a href=\"bar.txt\">bar.txt</a> (text, 8k)

which shows how big the link target is.  `html-sizes-update'
updates the \"6.5m\" or \"8k\" size based on local files.  This
is good when updating the HTML for a new version of a document or
download offered.

If the target is HTML then the size is that HTML itself plus any
local <img> files it contains.

    <a href=\"gallery.html\">Gallery</a> (total about 300k)

Page counts of a pdf document can be shown, either with file size
or alone.  The page count is currently found by running the local
file through whichever of \"pdfinfo\", \"qpdf\", or Perl
\"Image::ExifTool\" is available.

    <a href=\"foo.pdf\">foo.pdf</a> (10 pages, 300k)

Tags around displayed sizes are allowed, so for example some
schema.org microdata

    ... (<span itemprop=\"fileSize\">8.5 mbytes</span>)

In addition, html-sizes-update will update

  * <meta name=\"generator\"> header tag update or insert, from
    `html-sizes-generator'.

  * <a charset=\"...\" attribute update or insert for links to
    non-ascii text files.  This might help a browser show the
    right characters.

  * <img src=\"...\" width=123 height=456> image size update or
    insert for image files.  The size helps a browser format
    the page before the image file arrives.  PNG image sizes
    are always updated, other image formats must be displayable
    in Emacs to get their size.

See `html-sizes-link-table' to map ftp or other absolute links to
local directory locations so you can update things like

    <a href=\"ftp://foo.com/x.el\">x.el</a> (10k)

Changes are queried in the style of `query-replace' so that it's
clear what is updated.  Customize `html-sizes-update-query' to
run without querying.  \(Or afterwards check changes with
`diff-buffer-with-file' or `vc-diff' in usual ways.)

If a local file doesn't exist then it's reported in the
*Warnings* buffer and the \"missing\" count for the final
message.  Prior to Emacs 22 there is no *Warnings* buffer, only
the final message.

For file sizes, only a link style like the href above followed by
\"(...123k...\" in parens is recognised.  It can be \"k\" for
kilobytes or \"m\" for megabytes.  Any existing decimal places
are preserved in the update.

----
The html-sizes.el home page is
URL `http://user42.tuxfamily.org/html-sizes/index.html'

\"imgsizer\" can do similar img width/height update, using Python
URL `http://www.catb.org/~esr/software.html#imgsizer'"

  (interactive)
  (save-excursion
    (catch 'quit
      (let ((case-fold-search t)
            (html-sizes--total   0)
            (html-sizes--differ  0)
            (missing 0))

        (goto-char (point-min))
        (let (want)
          (when (and html-sizes-generator
                     (not (equal "" html-sizes-generator))
                     (or (and (re-search-forward "<meta[ \t\r\n]+name=[\"']?generator[\"']?[ \t\r\n]+content=\\([\"'].*?[\"']\\)>" nil t)
                              (setq want (concat "\"" html-sizes-generator "\"")))
                         (and (re-search-forward "\\(</head>\\)" nil t)
                              (setq want (concat "<meta name=\"generator\" content=\"" html-sizes-generator "\">\n" (match-string 1))))))

            (html-sizes-replace (match-beginning 1)
                                (match-end 1)
                                want)))

        (goto-char (point-min))
        (while (re-search-forward "<a\\s-\\(.\\|\n\\)*?href=\"\\([^\"\n]+\\)\"[^>]*>\\(.\\|\n\\)*?</a>" nil t)
          (save-excursion
            (let* ((link     (match-string-no-properties 2))
                   (filename link)
                   (beg      (match-beginning 0))
                   (end      (copy-marker (match-end 0)))
                   attrs)

              ;; drop #anchor target for filename
              (if (string-match "#" filename)
                  (setq filename (substring filename 0 (match-beginning 0))))

              (setq filename (html-sizes-link-apply filename))
              ;; local files only, not http: etc links
              (when (not (string-match "\\`[a-z]+:" filename))

                ;; xemacs 21.4.22 `file-truename' throws an error if
                ;; /foo/bar/quux has files "foo" or "bar" instead of
                ;; directories, treat that as nil attrs for missing "quux"
                (setq attrs (ignore-errors
                              (file-attributes (file-truename filename))))
                (if (not attrs)
                    ;; file not found
                    (progn
                      (setq missing (1+ missing))
                      (html-sizes-warn "Missing file %S" filename))

                  ;; "charset=" on ftp text files
                  (when (html-sizes-text-filename-p filename)
                    (let ((charset (html-sizes-file-charset filename)))
                      (when charset
                        (goto-char (+ beg 2))
                        (unless (re-search-forward "charset=[\"']?\\([^ \t\r\n\"']*?\\)[\"']" end t)
                          ;; new attribute
                          (looking-at "\\(\\s-\\)")
                          (setq charset (concat (match-string 1)
                                                "charset=\"" charset "\" ")))
                        (html-sizes-replace (match-beginning 1)
                                            (match-end 1)
                                            charset))))

                  ;; "(..123k...)"
                  (goto-char end)
                  (when (looking-at "[ \t\r\n]*(")
                    (while (let ((case-fold-search t))
                             (looking-at "\\(<[^>]*>\\|[^<)]\\)*?\
\\([0-9]*\\(\\.\\([0-9]+\\)\\)\
\\|[0-9]+\\(\\.\\([0-9]*\\)\\)?\
\\)\
\\(\\([mk]\\)\\|\\s-*\\([mk]\\)bytes?\\|\\s-+pages?\\)"))
                      (let* ((beg      (match-beginning 2))
                             (end      (copy-marker (match-end 0)))
                             (decimals (length (or (match-string 4)
                                                   (match-string 6))))
                             (unit     (or (match-string 8)
                                           (match-string 9)))
                             (suffix   (match-string 7))
                             (size     (if unit
                                           (html-sizes-file-size-string
                                            filename decimals unit suffix)
                                         (concat (html-sizes-file-pages filename)
                                                 suffix))))
                        (html-sizes-replace beg end size)
                        (goto-char end)))))))))

        (goto-char (point-min))
        (while (re-search-forward "<img\\s-\\(.\\|\n\\)*?src=\"\\([^\"\n]+\\)\"\\(.\\|\n\\)*?>" nil t)
          (save-excursion
            (let* ((link      (match-string-no-properties 2))
                   (filename  link)
                   (beg       (match-beginning 0))
                   (end       (copy-marker (match-end 0)))
                   (after-src (copy-marker (1+ (match-end 2))))
                   width-height)

              (dolist (elem html-sizes-link-table)
                (when (string-match (concat "\\`" (regexp-quote (car elem)))
                                    filename)
                  (setq filename (concat (cdr elem)
                                         (substring filename (match-end 0))))))

              (setq filename (html-sizes-link-apply filename))
              ;; local files only, not http: etc links
              (when (not (string-match "\\`[a-z]+:" filename))

                (setq width-height (html-sizes-image-size filename))
                (if (not width-height)
                    ;; file not found, or cannot get size
                    (progn
                      (setq missing (1+ missing))
                      (html-sizes-warn "Missing or unknown file %S" filename))

                  (dolist (attname '("width" "height"))
                    (let ((want (number-to-string (pop width-height)))
                          got)
                      (goto-char (+ beg 2))
                      (if (re-search-forward (concat attname "=[\"']?\\([^ \t\r\n\"'>]*\\)[\"']?") end t)
                          ;; existing attribute
                          (setq got (match-string 1))
                        ;; new attribute, position after src="", or for
                        ;; the height try to position after "width="
                        (or (and (equal attname "height")
                                 (goto-char (+ beg 2))
                                 (re-search-forward "width=[\"']?\\([^ \t\r\n\"'>]*\\)[\"']?" end t))
                            (goto-char after-src))
                        (looking-at "\\(.\\|\n\\)")
                        (setq want (concat " " attname "=" want
                                           (match-string 1)))
                        (set-marker after-src (1- (point))))

                      (unless (equal want got)
                        (html-sizes-replace (match-beginning 1)
                                            (match-end 1)
                                            want)))))))))

        (message "Total %d file sizes, %d differed, %d missing"
                 html-sizes--total html-sizes--differ missing)))))

;; LocalWords: charset docstring http htm html ascii gz gzipped endian
;; LocalWords: downloadable href txt img src parens imgsizer el tty
;; LocalWords: microdata itemprop fileSize unix ps pdf minibuffer
;; LocalWords: kbytes mbytes foo org customize customized
;; LocalWords: pdfinfo qpdf ExifTool

(provide 'html-sizes)

;;; html-sizes.el ends here
