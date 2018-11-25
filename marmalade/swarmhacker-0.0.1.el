;;; swarmhacker.el --- simple swarm chat

;; Copyright (C) 2013  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: comm, tools
;; Version: 0.0.1
;; Created: 1st April 2013

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Helping facilitate swarms by making a mode to handle chatting.

;;; Code:

(defvar swarmhacker-username nil
  "The buffer local username.")

(make-variable-buffer-local 'swarmhacker-username)

(defvar swarmhacker/user-colors (make-hash-table :test 'equal)
  "Map of usernames to colors.")

(defvar swarmhacker/buffer (get-buffer-create "*swarm-chat*")
  "The buffer we use for chat.")

(defun swarmhacker/propertize ()
  "Hook to add properties."
  (let ((color (gethash
                swarmhacker-username
                swarmhacker/user-colors)))
    (add-text-properties
     (- (point) 1) (point)
     (list
      'face `(:foreground ,color)
      'help-echo swarmhacker-username))))

(defun swarmhacker (username)
  "Start swarmhacker for USERNAME."
  (interactive "Mwhat's your username: ")
  (let* ((base (get-buffer-create swarmhacker/buffer))
         (existing-color (gethash username swarmhacker/user-colors))
         (color (if existing-color
                    existing-color
                    (let ((new-color (format "#%x" (random #xffffff))))
                      (puthash username new-color swarmhacker/user-colors)
                      new-color)))
         (indirect-buffer-name (format
                                "%s-%s*"
                                (buffer-name
                                 (get-buffer swarmhacker/buffer))
                                username))
         (indirect-buf (if (get-buffer indirect-buffer-name)
                           (error "That buffer exists...")
                           (make-indirect-buffer
                            (get-buffer swarmhacker/buffer)
                            indirect-buffer-name))))
    (with-current-buffer indirect-buf
      (setq swarmhacker-username username)
      (add-hook
       'post-self-insert-hook
       'swarmhacker/propertize t t))
    (switch-to-buffer indirect-buf)))

(provide 'swarmhacker)

;;; swarmhacker.el ends here
