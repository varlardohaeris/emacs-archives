;;; Commentary:

;; M-x unicode-disp changes the display table to show a few otherwise
;; undisplayable unicode chars as ASCII equivalents or near equivalents.
;;
;; The characters handled are a personal selection of the worst bits of
;; unicode encountered, with the aim of making them displayable on an ASCII
;; or Latin-1 tty.  If nothing else it might give you ideas for display
;; table mangling of your own.
;;
;; See latin1-disp.el for similar display table setups for otherwise
;; undisplayable characters from the iso-8859-N charsets and some cyrillic.
;;
;; Quite what to transform and how prominent it should be is a matter of
;; personal preference.  Displaying an arrow char as a sequence like "->"
;; can make text lines come out too long, or tables etc not align, sometimes
;; very badly.  A face like `escape-glyph' can make it clear you're looking
;; at non-ascii, except it becomes distracting if the screen is littered
;; with it.
;;
;; The variant unicode hyphens and quotes currently treated by
;; `unicode-disp' are on the whole pointless and are better displayed as
;; plain ascii "-" etc where necessary, with no special highlighting.

