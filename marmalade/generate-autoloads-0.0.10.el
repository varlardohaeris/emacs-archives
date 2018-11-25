;;; generate-autoloads.el --- A package to help you lazy-load everything
;;
;; Author: Eric Crosson
;; URL: https://github.com/EricCrosson/generate-autoloads
;; Version: 0.0.10
;;
;; A method of generating a set of autoloads for a given file, to
;; speed up init time without loss of functionality. Instead of
;; (require)-ing a file in your configs, run \\[generate-autoloads]
;; from within the file to generate a list of quickly evaluating
;; autoloads. Replace the require in your configs with the generated
;; output for a faster boot. Only interactive functions are considered
;; in this process.
;;
;; Take special note of \\[maintain-autoloads-file]. Use this file to
;; maintain a provided autoloads file for a project. Calling this
;; function will force up-to-date the autoload file accompanying a
;; project.


(defun generate-autoloads (&optional buffer)
  "This method parses files in a directory and creates the
  corresponding autoloads for the defined functions.

  When nil, `use-all-defuns' only includes interactive functions in the generated
  output. When non-nil, the output will contain an autoload for every defun."
  (interactive)

  (defconst generate-autoloads-regexp (concat "(def" "un")
    "This is a little hack to prevent the search for
    \\[generate-autoloads-regexp] from matching its own
    definition. Splitting up the word avoids any regex matches.")

  (defconst interactive-regexp "(interactive"
    "This is the regex used to determine if a function is interactive
    or not")

  (defconst use-all-defuns t
    "This controls the inclusion of non-interactive defuns in the
    generated output. I recommend that it always be true, as I have
    experience erratic behavior when it was non-true.")

  (if (equal buffer nil)
      (setq buffer (generate-new-buffer (name-generate-autoloads-temp-buffer))
	    parsing-file (file-name-sans-extension (buffer-name)))
    (setq parsing-file (file-name-sans-extension buffer)))

  (save-excursion
    ;; Erase the contents of the temp buffer, if populated
    ;; already. Also fill with comments, for ease of copying.
    (switch-to-buffer buffer)
    (let ((end (progn (end-of-buffer) (point))))
      (if (not (re-search-backward (head-comment-identifier parsing-file) nil t))
	  (erase-buffer)
	(delete-region (point) end))) ;; clear out the old output

    (insert (head-comment-identifier parsing-file)))

  (save-excursion
    ;; This is the main loop, identifying functions and transcribing
    ;; their respective autoload commands in a temp buffer
    (beginning-of-buffer)
    (while (not (equal (re-search-forward generate-autoloads-regexp nil t) nil))
      (forward-char)
      (let ((end-defun (save-excursion (end-of-defun) (point))))
	(when (or (not (equal nil use-all-defuns))
		  (save-excursion (re-search-forward interactive-regexp end-defun t)))
	  (let ((function (format-autoload (current-word)
					   (file-name-sans-path-extension))))
	    (save-excursion
	      (switch-to-buffer buffer)
	      (insert function "\n")))))))

  (save-excursion
    ;; Clear inappropriate characters from final buffer, and wrap with tail
    (switch-to-buffer buffer)
    (end-of-buffer)
    (insert (tail-comment-identifier parsing-file)))
  )

(defun maintain-autoloads-file ()
  "For the current file, maintain an autoloads-file for quick loading."
  (interactive)
  (let ((file (name-autoloads-file-from-file-name (buffer-name))))
    (save-excursion
      (find-file file))
    (generate-autoloads file)
    (save-excursion
      (switch-to-buffer file)
      (insert (concat "\n\n(provide '" (file-name-sans-extension file) ")")))))

(defun name-autoloads-file-from-file-name (file)
  "Return a name of a maintained autoloads file generated from a given file name."
  (replace-regexp-in-string ".el$" "-autoloads.el" file))

(defun head-comment-identifier (parsing-file)
  "Returns the comment used to mark the beginning of the generated code"
  (concat ";; Beginning of " parsing-file
	  ".el automatically generated autoloads\n"))

(defun tail-comment-identifier (parsing-file)
  "Returns the comment used to mark the end of the generated code"
  (concat ";; End of " parsing-file ".el autoloads"))

(defun name-generate-autoloads-temp-buffer ()
  "Decides upon a name for the temp (output) buffer to
be used for generate-autoloads."
  (concat "*autoloads:" (file-name-sans-path-extension) "*"))

(defun file-name-sans-path-extension ()
  "Returns the name of the file open in the current
buffer, sans path and extension."
  (setq parsing-file (file-name-sans-extension
		      (file-name-nondirectory (buffer-file-name)))))

(defun format-autoload (function file)
  "Formats a command to autoload the current function
and returns the command as a string."
  (concat "(autoload '" function " \"" file "\" nil t)"))

;;; generate-autoloads.el ends here
