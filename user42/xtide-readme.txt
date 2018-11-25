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

