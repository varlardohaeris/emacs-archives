;;; Commentary:

;; M-x ffap normally only recognises an include like
;;
;;     #include <foo.h>
;;
;; when point is on the filename part.  This spot of code lets it work when
;; point in the "#include" part.  The following forms are supported,
;;
;;     #include <foo.h>       C language
;;     #include "foo.h"       C language
;;     @include "foo.awk"     GNU Awk
;;     include foo.make       GNU Make
;;     include "foo.rc"       Gtk RC file
;;
;; You can always move point to the filename and M-x ffap from there, but
;; it's handy to have it work from the start of the line too, especially
;; when just browsing rather than editing.
;;
;; GNU Make can do a multiple-file include.  The first filename is offered
;; when point is on the include.  Move point to the second name to get that.
;;
;;     include foo.make bar.make
;;
;; This code works with ffap-makefile-vars.el.  If you load that package
;; then a GNU Make include can have variables to expand,
;;
;;     include $(HOME)/mystuff/foo.make

