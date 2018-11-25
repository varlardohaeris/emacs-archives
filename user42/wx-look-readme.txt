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

