;;; mine-sweeper-auto.el --- automated mine sweeping for mine-sweeper.el

;; Copyright 2005, 2007, 2009, 2010, 2011, 2013, 2014, 2016, 2019 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 7
;; Keywords: games, mine-sweeper
;; URL: http://user42.tuxfamily.org/mine-sweeper-auto/index.html

;; mine-sweeper-auto.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; mine-sweeper-auto.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This package extends mine-sweeper.el with a `mine-auto' function to go
;; around the board marking or opening squares the same as you might do
;; manually, but automated.
;;
;; Only safe moves are made, but the algorithms are fairly basic and
;; probably don't detect all possible safe moves.

;;; Install:

;; Put mine-sweeper-auto.el in one of your `load-path' directories and the
;; following in your .emacs to make M-x mine-auto available,
;;
;;     (autoload 'mine-auto "mine-sweeper-auto" nil t)
;;
;; The suggested key binding is "a", with
;;
;;     (eval-after-load "mine-sweeper"
;;       '(define-key mine-sweeper-mode-map "a" 'mine-auto))
;;
;; There's an autoload cookie below for the function, if you use
;; `update-file-autoloads' and friends; but the key binding is left as a
;; per-user preference.

;;; History:

;; Version 1 - the first version.
;; Version 2 - safe move messages, customizable delays
;; Version 3 - undo defadvice on unload-feature
;; Version 4 - express dependency on 'advice
;; Version 5 - add autoload cookie, add defvars to quieten byte compiler
;; Version 6 - also check subset clear squares
;; Version 7 - use push for local variables


;;; Code:

(eval-when-compile
  (unless (and (fboundp 'dolist)
               (fboundp 'dotimes)
               (fboundp 'push))
    (require 'cl))) ;; for macros in emacs20

;; Explicit dependency on advice.el since `mine-sweeper-auto-unload-function'
;; needs `ad-find-advice' macro when running not byte compiled, and that
;; macro is not autoloaded.
(require 'advice)

;;------------------------------------------------------------------------------

(defvar mine-field-wsize) ;; in mine-sweeper.el
(defvar mine-field-hsize)
(defvar mine-mark-bomb)
(defvar mine-mark-field)
(defvar mine-point)

;;;###autoload
(defgroup mine-sweeper-auto nil "Mine-Sweeper-Auto"
 :prefix "mine-sweeper-auto-"
 :group 'games ;; in absense of a group for mine-sweeper itself
 :link  '(url-link :tag "mine-sweeper-auto.el home page"
                   "http://user42.tuxfamily.org/mine-sweeper-auto/index.html"))

(defcustom mine-auto-travel-delay 0.15
  "Time in seconds to pause when moving point.
Set this to zero to jump between sweeping positions without
visual walking."
  :type  'number
  :group 'mine-sweeper-auto)

(defcustom mine-auto-mark-delay   0.25
  "Time in seconds to pause when marking a cell.
Set this to zero to instantly auto mine sweep without visual
progression."
  :type  'number
  :group 'mine-sweeper-auto)

(defvar mine-auto-pending nil
  "An internal part of mine-sweeper-auto.el.
List ((ACTION ROW COL) (ACTION ROW COL) ...) of pending work.
ACTION is a function to call, currently a symbol either
`mine-open' or `mine-mark'.

This variable is only used let-bound within `mine-auto'.  The
global value is unused.")

(defvar mine-auto-stop nil
  "An internal part of mine-sweeper-auto.el.
Non-nil when `mine-auto' should stop.
This can be due to either user keypress to stop or game-over.
This variable is only used let-bound within `mine-auto', the
global value is unused.")

(defsubst mine-auto-rowcol-pos (row col)
  "An internal part of mine-sweeper-auto.el.
Return buffer position for mine grid ROW,COL.
Currently the return is garbage if ROW,COL is outside the valid range."
  (+ (point-min) col (* row (1+ mine-field-wsize))))
(eval-when-compile
  (put   'mine-auto-rowcol-pos 'side-effect-free t))

(defun mine-auto-ref (row col)
  "An internal part of mine-sweeper-auto.el.
Return the visible char at ROW,COL in the minefield.
The return is nil if ROW,COL is outside the valid range."
  (and (<= 0 row) (< row mine-field-hsize)
       (<= 0 col) (< col mine-field-wsize)
       (char-after (mine-auto-rowcol-pos row col))))
(eval-when-compile
  (put 'mine-auto-ref 'side-effect-free t))

(defun mine-auto-char-to-number (char)
  "An internal part of mine-sweeper-auto.el.
If CHAR is a digit ?0 to ?9 then return a number 0 to 9.
If not then return nil."
  (cdr (assoc char
              '((?0 . 0)
                (?1 . 1)
                (?2 . 2)
                (?3 . 3)
                (?4 . 4)
                (?5 . 5)
                (?6 . 6)
                (?7 . 7)
                (?8 . 8)
                (?9 . 9)))))
(eval-when-compile
  (put 'mine-auto-char-to-number 'pure t))

(defun mine-auto-ref-number (row col)
  "An internal part of mine-sweeper-auto.el.
Return the char at ROW,COL as a number 0 to 9.
If the char is a bomb or background field or ROW,COL is outside
the grid then return nil."
  (mine-auto-char-to-number (mine-auto-ref row col)))
(eval-when-compile
  (put 'mine-auto-ref 'side-effect-free t))

(defun mine-auto-around (row col)
  "An internal part of mine-sweeper-auto.el.
Return the squares around ROW,COL in the minefield.
The return is a list
     ((CHAR ROW COL)
      (CHAR ROW COL)
       ...)
There are 8 elements in the list, being each of the 8 squares
surrounding the given ROW,COL.  CHAR is nil when the square is
off the edge of the grid."
  (list (list (mine-auto-ref row (1- col))      row (1- col))      ;; N
        (list (mine-auto-ref (1+ row) (1- col)) (1+ row) (1- col)) ;; NE
        (list (mine-auto-ref (1+ row) col)      (1+ row) col)      ;; E
        (list (mine-auto-ref (1+ row) (1+ col)) (1+ row) (1+ col)) ;; SE
        (list (mine-auto-ref row (1+ col))      row (1+ col))      ;; S
        (list (mine-auto-ref (1- row) (1+ col)) (1- row) (1+ col)) ;; SW
        (list (mine-auto-ref (1- row) col)      (1- row) col)      ;; W
        (list (mine-auto-ref (1- row) (1- col)) (1- row) (1- col)) ;; NW
        ))
(eval-when-compile
  (put 'mine-auto-around 'side-effect-free t))

(defun mine-auto-count (lst char)
  "An internal part of mine-sweeper-auto.el.
LST is a list of ((CHAR ROW COL) ...) per `mine-auto-around'.
Return the number of those surrounding squares which are CHAR.
CHAR can be `mine-mark-bomb' to count bombs or `mine-mark-field'
to count background field."
  (let ((ret 0))
    (dolist (elem lst)
      (if (equal char (car elem))
          (setq ret (1+ ret))))
    ret))

(defun mine-auto-check-one (row col)
  "An internal part of mine-sweeper-auto.el.
Check ROW,COL in the minefield for safe actions that its count implies.
Actions found are added to `mine-auto-pending'."

  (let ((n (mine-auto-ref-number row col)))
    (when n
      (let* ((around (mine-auto-around row col))
             (bombs  (mine-auto-count around mine-mark-bomb))
             (fields (mine-auto-count around mine-mark-field))
             (remain (- n bombs)))

        ;; if no remaining bombs around this square
        ;; then can safely open all its surrounding field squares
        (when (zerop remain)
          (dolist (elem around)
            (when (eq (car elem) mine-mark-field)
              (add-to-list 'mine-auto-pending
                           (cons 'mine-open (cdr elem))))))

        ;; if the surrounding field squares are exactly the number of
        ;; remaining bombs then mark them all as bombs
        (when (= remain fields)
          (dolist (elem around)
            (when (eq (car elem) mine-mark-field)
              (add-to-list 'mine-auto-pending
                           (cons 'mine-mark (cdr elem))))))))))


(defun mine-auto-check-one-harder (row col)
  "An internal part of mine-sweeper-auto.el.
Check ROW,COL in the minefield for safe actions that its count implies.
Actions are added to `mine-auto-pending'.

This is a harder check, looking for squares which are subsets of
others."

  (let ((n (mine-auto-ref-number row col)))
    (when n
      (let* ((around   (mine-auto-around row col))
             (bombs    (mine-auto-count around mine-mark-bomb))
             (remain   (- n bombs))
             fields-list
             other-list)
        (when (> remain 0)

          ;; fields-list is all the background field squares around us
          (dolist (elem around)
            (if (equal (car elem) mine-mark-field)
                (push elem fields-list)))

          ;; other-list is other squares which have been opened and are
          ;; adjacent to one or more of our fields-list squares
          (dolist (field-elem fields-list)
            (dolist (other-elem (mine-auto-around (nth 1 field-elem)
                                                  (nth 2 field-elem)))
              (when (and (not (and (equal (nth 1 other-elem) row)
                                   (equal (nth 2 other-elem) col)))  ;; not us
                         (mine-auto-char-to-number (car other-elem)) ;; opened
                         (not (member other-elem other-list))) ;; no duplicates
                (push other-elem other-list))))

          (dolist (other-elem other-list)
            (let* ((other-n      (mine-auto-char-to-number (nth 0 other-elem)))
                   (other-around (mine-auto-around (nth 1 other-elem)
                                                   (nth 2 other-elem)))
                   (other-bombs  (mine-auto-count other-around
                                                  mine-mark-bomb))
                   (other-remain (- other-n other-bombs))
                   other-fields-list
                   (subset-p t))

              ;; other-fields-list is the field squares around other-elem
              (dolist (elem other-around)
                (if (equal (car elem) mine-mark-field)
                    (push elem other-fields-list)))

              ;;
              (dolist (elem other-fields-list)
                (and (not (member elem fields-list))
                     (setq subset-p nil)))

              (when subset-p
                ;; other-fields-list is a subset of our fields-list

                (when (= remain other-remain)
                  ;; All our remaining bombs are within the
                  ;; other-fields-list, so the rest of our fields-list are
                  ;; clear.
                  (dolist (elem fields-list)
                    (when (not (member elem other-fields-list))
                      (add-to-list 'mine-auto-pending
                                   (cons 'mine-open (cdr elem))))))

                (when (= (- (length other-fields-list) other-remain)
                         (- (length fields-list)       remain))
                  ;; All our remaining clear squares are within the
                  ;; other-fields-list, so the rest of our fields-list are
                  ;; bombs.
                  (dolist (elem fields-list)
                    (when (not (member elem other-fields-list))
                      (add-to-list 'mine-auto-pending
                                   (cons 'mine-mark (cdr elem))))))))))))))

(defun mine-auto-check-all ()
  "An internal part of mine-sweeper-auto.el.
Check the whole minefield for safe actions to perform.
Actions are added to `mine-auto-pending'."

  (dotimes (row mine-field-hsize)
    (dotimes (col mine-field-wsize)
      (mine-auto-check-one row col)))

  (unless mine-auto-pending
    (dotimes (row mine-field-hsize)
      (dotimes (col mine-field-wsize)
        (mine-auto-check-one-harder row col)))))

(defun mine-auto-distance (a b)
  "An internal part of mine-sweeper-auto.el.
A and B are two-element lists (ROW COL).
Return the hypotenuse distance between those ROW,COL positions."
  (sqrt (+ (expt (- (car a)  (car b))  2)
           (expt (- (cadr a) (cadr b)) 2))))
(eval-when-compile
  (put 'mine-auto-distance 'pure t))

(defun mine-auto-find-minimum (proc lst)
  "An internal part of mine-sweeper-auto.el.
PROC is a function.
Return the element of LST for which (PROC elem) is smallest.
PROC must return a number."
  (let* ((ret (car lst))
         (val (funcall proc ret)))
    (dolist (elem lst)
      (let ((this-val (funcall proc elem)))
        (when (< this-val val)
          (setq ret elem)
          (setq val this-val))))
    ret))

(defun mine-auto-travel (row col)
  "An internal part of mine-sweeper-auto.el.
Move to ROW,COL in the minefield, walking square by square.
This is for a cute display."
  (let ((targ (list row col)))
    (while (and (not mine-auto-stop)
                (let ((curr (list (aref mine-point 0)
                                  (aref mine-point 1))))
                  (and (not (equal curr targ))
                       (let ((next (mine-auto-find-minimum
                                    (lambda (elem)
                                      (mine-auto-distance elem targ))
                                    (mapcar 'cdr
                                            (apply 'mine-auto-around curr)))))
                         (apply 'mine-move-cursor next)

                         ;; stop on keypress
                         (or (sit-for mine-auto-travel-delay)
                             (setq mine-auto-stop t))

                         t)))))))

(defun mine-auto-diff (old new)
  "An internal part of mine-sweeper-auto.el.
OLD and NEW are strings of minefield buffer contents.
Return a list ((ROW COL) (ROW COL) ...) which are the places
where OLD and NEW differ."
  (let ((pos 0)
        ret)
    (while (< pos (length old))
      (unless (= (elt old pos) (elt new pos))
        (setq ret (cons (list (/ pos (1+ mine-field-wsize))
                              (% pos (1+ mine-field-wsize))) ret)))
      (setq pos (1+ pos)))
    ret))

(defadvice mine-over (before mine-sweeper-auto activate)
  "Let `mine-auto' notice when the game is over."
  (setq mine-auto-stop 'mine-over))

(defun mine-sweeper-auto-unload-function ()
  "Remove defadvice from `mine-over'.
This is called by `unload-feature'."
  (when (ad-find-advice 'mine-over 'before 'mine-sweeper-auto)
    (ad-remove-advice   'mine-over 'before 'mine-sweeper-auto)
    (ad-activate        'mine-over))
  nil) ;; and do normal unload-feature actions too

(defun mine-auto-one ()
  "An internal part of mine-sweeper-auto.el.
Process one action from `mine-auto-pending'."
  (let* ((curr   (list (aref mine-point 0)
                       (aref mine-point 1)))
         (elem   (mine-auto-find-minimum ;; closest to `curr'
                  (lambda (elem)
                    (mine-auto-distance (cdr elem) curr))
                  mine-auto-pending))
         (action (nth 0 elem))
         (row    (nth 1 elem))
         (col    (nth 2 elem)))
    ;; could be duplicates of this element, remove all of them
    (setq mine-auto-pending (delete elem mine-auto-pending))

    ;; if still a blank field
    (when (eq mine-mark-field (mine-auto-ref row col))
      (mine-auto-travel row col)

      ;; Crib note: calling by `funcall' avoids byte compiler warnings about
      ;; `mine-mark' and `mine-open' not known to be defined (want to be
      ;; able to byte-compile without mine-sweeper.el available).
      (if (eq action 'mine-mark)
          (progn
            (funcall action)

            ;; After `mine-mark' surrounding squares might have have their
            ;; count filled, etc, so check surrounding squares.
            (dolist (elem (mine-auto-around row col))
              (when (car elem)
                (apply 'mine-auto-check-one (cdr elem)))))

        ;; action is `mine-open'
        (let ((old (buffer-string)))
          (funcall action)

          ;; `mine-open' can possibly change large areas of the grid.
          ;; Examine all changed squares for new actions.
          (dolist (rc (mine-auto-diff old (buffer-string)))
            (apply 'mine-auto-check-one rc))))

      ;; stop on keypress
      (or (sit-for mine-auto-mark-delay)
          (setq mine-auto-stop t)))))

(defun mine-auto-all-field-p ()
  "An internal part of mine-sweeper-auto.el.
Return non-nil if all cells are the unexamined background field."
  (save-excursion
    (goto-char (mine-auto-rowcol-pos 0 0))
    (not (re-search-forward
          (concat "[^" (string mine-mark-field) "\n]")
          nil
          (1+ (mine-auto-rowcol-pos (1- mine-field-hsize)
                                    (1- mine-field-wsize)))))))

;;;###autoload
(defun mine-auto ()
  "Automated mine sweeping.
Mark mines or open squares automatically.  These are the sorts of
moves you might make manually, but automated.  Only safe moves
are made.  If no certainly safe mark is found then nothing is
done.

Your very first mine mark must be manual, since there's no safe
move on an entirely blank board.  The default `mine-field-hsize'
and `mine-field-wsize' sizes and `mine-amount' bombs is fairly
easy and a good initial guess can sweep to just a few unexamined
patches for `mine-auto' to go through.

With more bombs you instead usually get a blob of swept area.
`mine-auto' then works around the edges expanding it
progressively.

Only the visible board is examined, there's no cheating with the
underlying secret minefield contents.

----
The simplest check is for any any cell with its bomb count
already met so remaining unopened squares must be clear.

   +--------
   | 1 _         <-- 1 bomb already
   | * _             remaining squares must be clear
   |

Or conversely cell with a remaining bomb count equal to its
remaining unopened squares must have all bombs there.

   +--------
   | 3 _         <-- 2 bombs in 2 squares
   | * _             those squares must be bombs
   |

These simple checks often go in chains where resolving one
determines the status of other squares nearby.  The automation
goes roughly in order along such chains.

If there's no more simple cases then a more sophisticated check
is made.  Two count squares are sought where the unopened squares
adjacent to one are a subset of the unopened squares adjacent to
the other.

If the remaining bomb count of the subset equals the remaining
bomb count of the superset then all the bombs are in the subset.
The other squares of the superset are clear.

   +--------
   | 1           <-- this square's neighbours are a subset
   | 1           <-- of this square's neighbours
   | _ _         so these squares are clear

Conversely if the remaining clear count of the subset equals the
remaining clear count of the superset then all the clear squares
are in the subset.  The other squares of the superset are bombs.

   +--------
   | 1           <-- 1 clear square is a subset of
   | 3           <-- 1 clear square
   | _ _         so these others squares are bombs


----
In the current code no use is made of the total number of bombs
in the grid.  Occasionally the last square or group of squares
are surrounded by bombs so there's no counts known.  The
remaining bombs from the total might determine whether these are
all bombs or all clear.

----
The mine-sweeper-auto home page is
URL `http://user42.tuxfamily.org/mine-sweeper-auto/index.html'

mine-sweeper.el can be found archived at
URL `http://web.archive.org/web/20030204205410/http://www.stanford.edu/~tshm/mine-sweeper.el'"

  (interactive)
  (if (mine-auto-all-field-p)
      (message "Start with a manual guess, there's no safe moves when all blank!")

    (message "Automated sweeping, press arrow key to stop ...")
    (let (mine-auto-stop
          (msg "No safe moves"))
      (while (and (not mine-auto-stop)
                  (let (mine-auto-pending)
                    (mine-auto-check-all)
                    (and mine-auto-pending
                         (progn
                           (while (and mine-auto-pending
                                       (not mine-auto-stop))
                             (mine-auto-one))
                           (setq msg "No more safe moves")
                           t)))))
      (unless mine-auto-stop
        (message "%s" msg)))))

;; LocalWords: el keypress unexamined col elem

(provide 'mine-sweeper-auto)

;;; mine-sweeper-auto.el ends here
