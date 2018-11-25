;;; areabrowse.el --- browse diku mud .are area files

;; Copyright 2005, 2007, 2009, 2013, 2015 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 4
;; Keywords: games, mud
;; URL: http://user42.tuxfamily.org/areabrowse/index.html

;; areabrowse.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; areabrowse.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This is a mode to browse Diku MUD .are dungeon area files.  It's pretty
;; minimal, just helping to let you walk the area north, south, etc.

;;; Install:

;; Put areabrowse.el somewhere in your load path, and in your .emacs add
;;
;;     (autoload 'areabrowse-mode "areabrowse" nil t)
;;     (add-to-list 'auto-mode-alist '("\\.are\\'" . areabrowse-mode))

;;; History:

;; Version 1 - the first version
;; Version 2 - autoload cookie for the mode
;; Version 3 - use define-derived-mode so run after-change-major-mode-hook
;; Version 4 - new email


;;; Code:

(defconst areabrowse-font-lock-keywords
  '("^#.*")  ;; object IDs and section markers
  "`font-lock-keywords' for `areabrowse-mode'.")

(defvar areabrowse-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?d]  'areabrowse-go-down)
    (define-key map [?e]  'areabrowse-go-east)
    (define-key map [?l]  'areabrowse-go-last)
    (define-key map [?n]  'areabrowse-go-north)
    (define-key map [?s]  'areabrowse-go-south)
    (define-key map [?u]  'areabrowse-go-up)
    (define-key map [?w]  'areabrowse-go-west)
    (define-key map [? ]  'areabrowse-go-next)
    (define-key map [?\d] 'areabrowse-go-prev)
    map)
  "Keymap for `areabrowse-mode'.")

(defvar areabrowse-room-history nil
  "History of rooms visited by `areabrowse-mode'.
This is a list of integers.  It's buffer local because room
numbers are only applicable to a given file.")
(make-variable-buffer-local 'areabrowse-room-history)

(defun areabrowse-go-north ()
  "Go north."
  (interactive)
  (areabrowse-go-direction 0))
(defun areabrowse-go-east ()
  "Go east."
  (interactive)
  (areabrowse-go-direction 1))
(defun areabrowse-go-south ()
  "Go south."
  (interactive)
  (areabrowse-go-direction 2))
(defun areabrowse-go-west ()
  "Go west."
  (interactive)
  (areabrowse-go-direction 3))
(defun areabrowse-go-up ()
  "Go up."
  (interactive)
  (areabrowse-go-direction 4))
(defun areabrowse-go-down ()
  "Go down."
  (interactive)
  (areabrowse-go-direction 5))

(defun areabrowse-go-direction (direction)
  "Go in the given DIRECTION.
0=north, 1=east, 2=south, 3=west, 4=up, 5=down."
  (save-excursion
    (end-of-line)
    (re-search-forward (format "^\\(\\(D%d\\)\\|#\\)" direction))
    (if (not (match-string 2))
        (error "No exit that way"))
    (re-search-forward "^[0-9-]+ [0-9-]+ \\([0-9-]+\\)"))
  (areabrowse-go-room (string-to-number (match-string 1))))

(defun areabrowse-go-room (roomnum)
  "Go to ROOMNUM (an integer) in the buffer."
  (goto-char (point-min))
  (re-search-forward "^#ROOMS")
  (re-search-forward (format "#%d" roomnum))
  (beginning-of-line)
  (set-window-start (selected-window) (point))
  (setq areabrowse-room-history (cons roomnum areabrowse-room-history)))

(defun areabrowse-go-last ()
  "Go back to the previously visited room (from `areabrowse-room-history')."
  (interactive)
  (if (not areabrowse-room-history)
      (error "No last room"))
  (let ((new-history (cddr areabrowse-room-history)))
    (areabrowse-go-room (cadr areabrowse-room-history))
    (setq areabrowse-room-history new-history)))

(defun areabrowse-go-first-room ()
  "Go to the first room in the buffer."
  (interactive)
  (goto-char (point-min))
  (re-search-forward "^#ROOMS")
  (forward-line 1)
  (set-window-start (selected-window) (point)))

(defun areabrowse-go-next ()
  "Go forward to the next room (sequentially in the buffer)."
  (interactive)
  (end-of-line)
  (re-search-forward "^#")
  (beginning-of-line)
  (set-window-start (selected-window) (point)))

(defun areabrowse-go-prev ()
  "Go back to the previous room (sequentially in the buffer)."
  (interactive)
  (re-search-backward "^#")
  (beginning-of-line)
  (set-window-start (selected-window) (point)))

(defun areabrowse-count-rooms ()
  "Show a count of the number of rooms in the buffer."
  (interactive)
  (save-excursion
    (areabrowse-go-first-room)
    (let ((count 0))
      (while (and (re-search-forward "^#\\([0-9]+\\)" nil t)
                  (not (equal "0" (match-string 1))))
        (setq count (1+ count)))
      (message "Total %d rooms" count))))

;;;###autoload
(define-derived-mode areabrowse-mode fundamental-mode "areabrowse"
  "A major mode for viewing diku mud \".are\" dungeon area files.

\\{areabrowse-mode-map}

`areabrowse-mode-hook' is run after initializations are
complete."

  (setq buffer-read-only t)
  (set (make-local-variable 'font-lock-defaults)
       '(areabrowse-font-lock-keywords
         t      ;; no syntactic fontification (of strings etc)
         nil    ;; no case-fold
         nil))) ;; no changes to syntax table

;; LocalWords: diku

(provide 'areabrowse)

;;; areabrowse.el ends here
