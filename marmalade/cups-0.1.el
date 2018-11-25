;;; cups.el --- CUPS features for Emacs

;; Time-stamp: <2012-05-21 12:15:56 arne_bab@web.de>

;; Copyright (C) 2005 Luca Capello <luca@pca.it> http://luca.pca.it
;; Copyright (C) 2012 Arne Babenhauserheide <arne_bab@web.de> http://draketo.de
;;
;; Author:    Luca Capello / Gismo <luca@pca.it> and Arne Babenhauserheide / ArneBab (minor fixes)
;; URL:       http://luca.pca.it/projects/elisp/cups.el
;; Version:   0.1
;; Keywords:  cups, emacs, printing

;;; License:
;; ----------
;; This file is NOT part of GNU Emacs, but it is distributed under
;; the same terms as GNU Emacs.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; -------------
;; * A collection of functions to deal with the CUPS server.
;;
;; The basic functions are:
;; * cups-list-printers
;; * cups-print-buffer

;;; Installation:
;; ---------------
;; Just put the following into in your .emacs:
;;
;;  (load "/pathname/cups.el")

;;; History:
;; ----------
;;  2005/05/01: First release

;;; Defs and vars:
(defvar cups-printers-buffer nil)
(make-variable-buffer-local 'cups-printers-buffer)

;;; Code:
(defun cups-available-printers ()
  "Return a list of the available CUPS printers."
  (loop
   for cups-lpstat-line in (split-string (shell-command-to-string "LANG=C lpstat -p") "\n")
   when (equal (nth 0 (split-string cups-lpstat-line " ")) "printer")
   collect (nth 1 (split-string cups-lpstat-line " "))))

(defun cups-list-printers ()
  "List the available CUPS printers."
  (interactive)
  (setq cups-printers-buffer (get-buffer-create (format "*CUPS printers*")))
  (let ((buf cups-printers-buffer))
    (set-buffer buf)
    (setq buffer-read-only nil) 
    (erase-buffer)
    (insert "The current available CUPS printers are:\n\n")
    (loop
     for cups-printer in (cups-available-printers)
     do (insert "- " cups-printer "\n\n"))
    (setq buffer-read-only t)
    (pop-to-buffer buf)))

(defun cups-print-buffer ()
  "Print the current buffer using the CUPS server.

Before printing, it asks for the printer to be used. If no printer
is selected (basically, pressing RET), it uses the CUPS default."
  (interactive)
  (let* ((printer
	  (completing-read "Printer [RET for CUPS default]: "
			   (cups-available-printers) nil t))
	 (printer-name printer))
    (print-buffer)))

(provide 'cups)
;;; cups.el ends here
