;;; comint-better-defaults.el --- better defaults for comint-derived modes

;; Copyright (C) 2013 Nathaniel Flath <nflath@gmail.com>

;; Author: Nathaniel Flath <nflath@gmail.com>
;; URL: http://github.com/nflath/comint-better-defaults
;; Version: 1.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Comint is a mode used for mcommunicating with external processes such as
;; shells or REPLs.  You normally use a derived mode of comint instead of just
;; it.  The following makes it easier to use by not displaying garbage, making
;; the prompt read-only, and showing the maximum amount of output and scrolling
;; properly.

;;; Installation:

;; To install, put this file somewhere in your load-path and add the following
;; to your .emacs file:
;; (require 'comint-better-defaults)

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
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

(setq-default comint-process-echoes t)
(setq comint-prompt-read-only t)
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

(setq comint-scroll-show-maximum-output t)
(setq comint-scroll-to-bottom-on-input t)

;; Many times in a shell you perform the same action over and over again.  The
;; following will make repeated commands only add one item to the history of the
;; process, making it easier to access previous commands.
(setq-default comint-input-ignoredups t)

(provide 'comint-better-defaults)
;;; comint-better-defaults.el ends here
