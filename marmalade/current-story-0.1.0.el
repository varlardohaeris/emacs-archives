;;; current-story.el --- Track and insert current Pivotal Tracker
;;; story id

;; Copyright © 2013 Charlie Tanksley

;; Author: Charlie Tanksley
;; URL: https://github.com/charlietanksley/current-story
;; Version: 0.1.0
;; Created: 2013-05-24
;; Keywords: productivity

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; I follow the convention of ending commit messages with:
;;
;; [#story-id]
;;
;; Keeping track of the story id (or going to find it for every
;; commit) is annoying. This plugin gives you a command to set the
;; current story and a command to insert the current story id. That's
;; it.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(defun set-current-story ()
  "Set the story ID and title (or description) for the current
story."
  (interactive)
  (setq current-story-title "")
  (setq current-story-id
        (read-from-minibuffer "Current story id: "))
  (setq current-story-title
        (read-from-minibuffer "Current story title: ")))

(defun insert-current-story ()
  "Insert '[#id]' into the buffer at the cursor's position. This
command also displays the story title in the minibuffer."
  (interactive)
  (current-story)
  (insert (concat "[#" current-story-id "]")))

(defun current-story ()
  "Display the current story's title in the minibuffer."
  (interactive)
  (print current-story-title))

(defun set-current-story-title ()
  "Set the title of the current story."
  (setq current-story-title
        (read-from-minibuffer "Current story title: ")))

(provide 'current-story)

;;; current-story.el ends here
