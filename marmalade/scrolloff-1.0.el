;;; scrolloff.el --- This mode ofers vim-like scrolloff function

;; Version: 1.0
;; Author: Marcin Koziej <marcin@koziej.info>

(define-minor-mode scrolloff-mode
  "Keeps the screen scrolloff by `scrolloff-lines' lines"
      ;; The initial value.
      nil
      ;; The indicator for the mode line.
      nil
      ;; The minor mode bindings.
      nil
      
      (when scrolloff-mode
	(ad-enable-advice 'line-move 'after 'scrolloff)
	(ad-activate 'line-move)
	))
;;      :group 'hunger)


(defcustom scrolloff-lines 0
  "Keep this number of lines between frame and cursor"
  :type '(integer)
  :group 'scrolling)

(defvar scrolloff-lines 0 "Keep this number of lines between frame and cursor")

(defadvice line-move (after scrolloff last (arg &rest r) disable )
  (when scrolloff-mode
    (if (and (> arg 0) (< (window-end) (point-max)))
	(let ((do-dolu 
	       (- scrolloff-lines 
		  (count-screen-lines (window-point) (window-end)))))
	  (if (< 0 do-dolu)
	      (scroll-up do-dolu))
	  )
      (if (and (< arg 0) (> (window-start) (point-min)))
	  (let ((do-gory 
		 (- scrolloff-lines 
		    (count-screen-lines (window-start) (window-point)))))
	    (if (< 0 do-gory)
		(scroll-down do-gory))
	    )

	))))

(provide 'scrolloff)

;;; scrolloff.el ends here
