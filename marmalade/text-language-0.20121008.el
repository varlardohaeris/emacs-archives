;;; text-language.el --- tracking, setting, guessing language of text

;; Copyright (C) 2012 Peter Eisentraut

;; Author: Peter Eisentraut <peter@eisentraut.org>
;; URL: https://github.com/petere/emacs-text-language
;; Version: 0.20121008
;; Keywords: i18n wp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package tracks the human language of text.  The main use case
;; for this is setting the spell checking dictionary, but there are
;; other possibilities, such as adjusting the syntax table according
;; to the language.
;;
;; Additionally, the language of the text can also be guessed, using
;; external tools.  Currently, the only tool that is supported is
;; <https://bitbucket.org/spirit/guess_language>.
;;
;; Call text-language-set-language to set the language text.  The
;; hooks text-language-set-functions and
;; text-language-guessed-functions are for hooking in actions that
;; make use of the language information.  A hook that adjusts the
;; ispell dictonary is installed by default.

;;; Code:

(defvar text-language-current nil
  "Used to track the current language.")

(defvar text-language-set-functions nil
  "List of functions to be called when the text language has been
set, with a string for the language as argument.")

(defvar text-language-guessed-functions nil
  "List of functions to be called when the text language has been
guuessed, with the string for the language as argument.  A
function can return a nil value to cancel the setting of the
current language based on the guess.")

(defun text-language-set-language (lang)
  "Set the text language of the current buffer.
Run the (abnormal) hook text-language-set-functions with it."
  (interactive)
  (setq text-language-current lang)
  (run-hook-with-args 'text-language-set-functions lang))

(defun chomp (str)
  (replace-regexp-in-string "\n+\\'" "" str))

(defun text-language-guess ()
  "Guess the language of the current buffer and set it."
  (interactive)
  (let* ((cb (current-buffer))
         (gl (chomp
              (with-temp-buffer
                (call-process "python3" (buffer-file-name cb) t nil "-m" "guess_language" "-")
                (buffer-string)))))
    (when (run-hook-with-args-until-failure 'text-language-guessed-functions gl)
      (text-language-set-language gl))))

(add-hook 'text-language-guessed-functions
          (lambda (lang) (message "Guessed language: %s" lang)))

(define-minor-mode text-language-mode
  "Toggle minor mode that tracks the text language."
  :lighter (:eval (format " TL:%s" text-language-current))
  (make-local-variable 'text-language-current))

(define-minor-mode text-language-guess-mode
  "Turn on or off hooks that automatically guess the text language."
  :lighter " GL"
  (add-hook 'find-file-hook 'text-language-guess)
  (add-hook 'after-save-hook 'text-language-guess))

(add-hook 'text-mode-hook 'text-language-mode)
(add-hook 'text-mode-hook 'text-language-guess-mode)

(when (fboundp 'ispell-change-directory)
  (add-hook 'text-language-set-functions 'ispell-change-dictionary))

(provide 'text-language)

;;; text-language.el ends here
