;;; Commentary:

;; This is a spot of code to fix sentence spacing in text, ensuring two
;; spaces between sentences.  Such spacing is standard, and aids readability
;; by giving a visual separation between separate ideas in sentences.
;;
;; `M-x sentence-spacing' fixes the current buffer, eg. if you have some bad
;; text cut and pasted from elsewhere which needs fixing.
;;
;; `sentence-spacing-display' function fixes the current buffer, even when
;; read-only.  This is good in a hook of a viewing mode.
;;
;; HTML viewing usually needs fixing since HTML is notorious for lacking a
;; proper way to indicate sentence ends.  In emacs-w3m this can be done as
;; follows.  eval-after-load is since the hook doesn't start empty.
;;
;;   (eval-after-load "w3m"
;;     '(add-hook 'w3m-display-hook 'sentence-spacing-display))
;;
;; Man page or Perl POD viewing ought not need spacing fixes, since nroff
;; and the POD tools put through the right thing, but an author might have
;; failed to do so in the source.  The source would be the right place to
;; fix but the output can be fixed from a hook or by
;; `M-x sentence-spacing-display' until then.
;;
;; Mail and news shown by Gnus can be fixed in its articl washing hook.
;; That only changes the display, not the underlying message or anything
;; saved.  It's disappointingly common to see posters who should know better
;; only putting one space.
;;
;;   (add-hook 'gnus-part-display-hook 'sentence-spacing-display)
;;
;; Some English abbreviations are recognised as not the end of a sentence,
;; but in general there's always some ambiguity.  The attempt here is to
;; make a best guess.  The intention would be to have some configurations
;; for sentence end rules, abbreviations, etc.

