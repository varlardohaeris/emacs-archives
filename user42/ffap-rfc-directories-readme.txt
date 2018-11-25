;;; Commentary:

;; This spot of code makes M-x ffap look in some local directories for RFC
;; files before offering the ftp download in `ffap-rfc-path'.  It's good if
;; you keep copies of some RFCs locally, including perhaps from a packaged
;; distribution etc.
;;
;; This feature has been merged into Emacs 23.  The code notices that and
;; does nothing there.  But there's no default value in
;; `ffap-rfc-directories' in emacs23, so if you were relying on
;; "/usr/share/doc/RFC/links" from the code here then add that back from
;; your .emacs etc.
;;
;; If you keep local RFCs as compressed .gz etc you can enable jka-compr
;; with `(auto-compression-mode 1)' in the usual way to read them.  ffap
;; will fallback on the `ffap-rfc-path' download if there's a compressed
;; file but jka-compr is not enabled.
;;
;; There's a lot of Emacs RFC download/cache/search packages.  Several are
;; listed at
;;
;;     http://www.emacswiki.org/cgi-bin/wiki/RFC
;;
;; ffap-rfc-directories.el is intentionally minimal, just saving you from
;; hitting ftp.rfc-editor.org every time.

