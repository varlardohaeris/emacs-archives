;;; jcuken-fix.el --- Map Modifier-CyrillicLetter to the underlying Modifier-LatinLetter.
;;
;; Filename: jcuken-fix.el
;; Description: Map Modifier-CyrillicLetter to the underlying 
;; Modifier-LatinLetter, so that control sequences can be used 
;; when keyboard mapping is changed outside of Emacs.
;; For this to work correctly, .emacs must be encoded in the default coding
;; system.
;; Author: nikitadanilov
;; Version: 1.0
;; URL: https://bitbucket.org/qsimpleq/emacs-wiki-fixed-packages
;; Original: http://ru-emacs.livejournal.com/82512.html?replyto=545872
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(require 'cl)

;;;###autoload
(defun nepowerset (S)
  (let ((x (car S)))
    (if (cdr S)
        (let ((y (nepowerset (remove x S))))
          (append (list (list x))
                  (mapcar (lambda (e) (cons x e)) y)
                  y))
      (list (list x)))))

;;;###autoload
(defun jcuken-fix nil
  (interactive)
  (mapcar*
   (lambda (r e)
     ;; R and E are matching Russian and English keysyms
     ;; iterate over modifier subsets
     (mapc (lambda (mod)
             (define-key input-decode-map
               (vector (append mod (list r))) (vector (append mod (list e)))))
           (nepowerset '(control meta super hyper)))
     ;; finally, if Russian key maps nowhere, remap it to the English key without
     ;; any modifiers
     (define-key local-function-key-map (vector r) (vector e)))
   "Q\";:?9&F#C:5=3(H)I7%E*J$D+K20? @>;46-M/O'G!A<8"B,L1.N.,"
   "~`@#$^&QqWwEeRrTtYyUuIiOoPp{[}]AaSsDdFfGgHhJjKkLl:;\"'ZzXxCcVvBbNnMm<,>./?"))

(setq default-input-method 'russian-computer)
(add-hook 'after-make-frame-functions 'jcuken-fix)

(provide 'jcuken-fix)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; jcuken-fix.el ends here
