;;; rcirc-ucomplete.el --- Unambiguous non-cycling completion for rcirc

;; Copyright © 2007, 2011 Phil Hagelberg

;; Author: Phil Hagelberg <technomancy@gmail.com>
;; Version: 1.0.1
;; Keywords: rcirc, irc

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Code:

(require 'rcirc)
(require 'ring)
(require 'cl)

(defun rcirc-ucomplete-start ()
  (- (save-excursion
       (if (search-backward " " rcirc-prompt-end-marker t)
           (1+ (point))
         rcirc-prompt-end-marker))
     rcirc-prompt-end-marker))

(defun rcirc-ucomplete-trim (current candidate)
  (let ((current (if (> (length current) (length candidate))
                     (substring current 0 (length candidate))
                   current)))
    (if (equalp current (substring candidate 0 (length current)))
        current
      (rcirc-ucomplete-trim
       (substring current 0 (- (length current) 1)) candidate))))

(defun rcirc-ucomplete-insert (nick incomplete?)
  (delete-region (+ rcirc-prompt-end-marker (rcirc-ucomplete-start)) (point))
  (insert nick)
  (when (not incomplete?)
    (insert (if (zerop (rcirc-ucomplete-start))
                ": " " "))))

;;;###autoload
(defun rcirc-ucomplete ()
  "Complete nick from list of nicks in channel."
  (interactive)
  (let* ((completion-ignore-case t)
         (input (buffer-substring (+ rcirc-prompt-end-marker
                                     (rcirc-ucomplete-start)) (point)))
         (all-nicks (rcirc-channel-nicks (rcirc-buffer-process) rcirc-target))
         (targets (all-completions input all-nicks)))
    (when targets
      (let ((unambiguous (reduce 'rcirc-ucomplete-trim targets)))
        (if (string= unambiguous input)
            (message (mapconcat 'identity targets " "))
          (if unambiguous
              (rcirc-ucomplete-insert unambiguous (cadr targets))))))))

;;;###autoload
(eval-after-load 'rcirc
  '(define-key rcirc-mode-map (kbd "TAB") 'rcirc-ucomplete))

(provide 'rcirc-ucomplete)
;;; rcirc-ucomplete.el ends here
