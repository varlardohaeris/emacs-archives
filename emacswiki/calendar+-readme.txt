   Extensions and corrections to `calendar.el', `cal-french.el',
   and `diary-lib.el'.  Calendar, diary, appointments stuff.
   See doc strings of functions `calendar-mode' and `calendar'.

NOTE: This code is quite old, and is likely obsolete now.  You
      might find it useful in some way - or not. ;-)

-------------------------------------------------------------------

 Main new functions defined here:

   `calendar-count-weekdays-region', `calendar-local-holiday-list',
   `calendar-mouse-3-menu', `calendar-mouse-drag-region',
   `mark-local-holidays', `monday-after-easter',
   `set-calendar-region-from-region', `show-calendar'.

 New user options defined here:

   `cal-mode-line-key-help', `calendar-local-holiday-marker',
   `calendar-region-marker', `weekend-face'.


 ***** NOTE: The following functions defined in `calendar.el' have
             been REDEFINED HERE:

 `calendar-basic-setup' -
    1) `list-calendar-holidays' is now called by `calendar' on the
       3-month period starting with the current month, not with the
       previous month.  The rationale is that you want to be
       informed of future, not past, holidays.  This means that the
       holidays listed by `calendar', when
       `view-calendar-holidays-initially', are one month later than
       those listed by `list-calendar-holidays'.
    2) Fixed bug: ARG's value & sign was not being taken into
       account previously.  Now, distinguish between the case where
       ARG is explicitly numeric (in which case, use its value as
       is), and the C-u-only case (in which case, prompt for month
       and year).
 `calendar-count-days-region' - Marks days in region if visible.
 `calendar-mode' - New doc string.
 `generate-calendar-month' - Puts weekend days in `weekend-face'.
 `mark-visible-calendar-date' -
    Doesn't add a new overlay if one is already present with face
    MARK. (To allow for overlapping overlays.) Arg PRIORITY is new.


 ***** NOTE: The following function defined in `solar.el'
             has been REDEFINED HERE:

 `solar-sunrise-sunset-string' - Avoid being seen as appt time.


 ***** NOTE: The following function defined in `diary-lib.el'
             has been REDEFINED HERE:

 `insert-diary-entry' -
    When `european-calendar-style', this inserts `26 Jan 1995 :'
    instead of `26 Jan 1995'.  NOTE: This definition is coupled
    with that of `european-date-diary-pattern' (which also has " :"
    at the end of a diary entry).


 The following binding is made here for mode `calendar-mode'.

   `='              `calendar-count-days-region'


This file redefines a few standard calendar functions.  It should
be loaded after loading any of the GNU files `calendar.el',
`cal-french.el', or `diary-lib.el'.  So, in your `~/.emacs' file,
do this:
       (eval-after-load "calendar" '(require 'calendar+))
       (eval-after-load "cal-french" '(require 'calendar+))
       (eval-after-load "diary-lib" '(require 'calendar+))

Alternatively, you can put these autoloads in your `~/.emacs' file:

   (autoload 'calendar "calendar+"
             "Display a 3-month calendar in another window." t)
   (autoload 'insert-diary-entry "calendar+"
             "Insert a diary entry for date indicated by point." t)


The companion file `cal-opts.el' sets a number of options regarding
the calendar, diary, etc.  Put (require 'cal-opts.el) in your
`~/.emacs' file if you like most of what is there, then modify what
you don't like.  Here are some things you might want to do in your
`~/.emacs' file, to counter individual settings from `cal-opts.el':

(setq mark-holidays-in-calendar nil) ; Don't mark holidays.  Faster.
(setq mark-diary-entries-in-calendar nil) ; Don't mark diary entries.
(setq holidays-in-diary-buffer nil)  ; Don't list holidays in diary.
(setq general-holidays nil)          ; Get rid of U.S. holidays.
(setq christian-holidays nil)        ; Get rid of religious holidays.
(setq hebrew-holidays nil)           ;              "              "
(setq islamic-holidays nil)          ;              "              "
(setq calendar-latitude LAT)   ; Set LAT to your latitude.
(setq calendar-longitude LONG) ; Set LONG to your longitude.
