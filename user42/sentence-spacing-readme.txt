;;; Commentary:

;; This is a spot of code to fix sentence spacing in text, ensuring two
;; spaces between sentences.  Such spacing is standard, and aids readability
;; by giving a visual separation corresponding to logical separation of
;; ideas in sentences.
;;
;; `M-x sentence-spacing' fixes the current buffer, eg. for bad text from
;; elsewhere.
;;
;; Function `sentence-spacing-display' fixes the current buffer, even when
;; read-only.  This is good in the hook of a viewing mode.  It can be used
;; interactively `M-x sentence-spacing-display' for a one-off fix too.
;;
;; HTML viewing usually needs fixing since HTML is notorious for lacking a
;; proper way to indicate sentence ends.  In emacs-w3m, this can be done
;; from w3m-display-hook.  eval-after-load is because the hook doesn't start
;; empty.
;;
;;   (eval-after-load "w3m"
;;     '(add-hook 'w3m-display-hook 'sentence-spacing-display))
;;
;; Man page or Perl POD viewing ought not need spacing fixes, since nroff
;; and the POD tools put through the right thing, but an author might have
;; failed to do so in the source, and a few manpage conversion tools go out
;; of their way to do the wrong thing.  The source would be the place to fix
;; it for everyone, but just the display is helped from a hook or by
;; `M-x sentence-spacing-display' until then.
;;
;; Gnus mail and news articles can be fixed from the article "washing" hook.
;; This only changes the display, not the underlying message or anything
;; saved.  It's disappointingly common to see posters who should know better
;; only putting one space, and HTML mail (generally the domain of the
;; spammer of course) is nearly certain to be wrong.
;;
;;   (add-hook 'gnus-part-display-hook 'sentence-spacing-display)
;;
;; Some English abbreviations are recognised as not the end of a sentence,
;; but in general there's always some ambiguity.  The attempt here is to
;; make a best guess.  Perhaps some configurations could give sentence end
;; rules, desired abbreviations, etc.

