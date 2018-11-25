A help system that displays help information when you're idle.
Modeled after eldoc, but extensible.

An example I use for learning some important AucTeX key shortcuts:

(setq idlehelp-help-list
      (append `((latex-mode
                 my-LaTeX-help-in-itemize-p
                 "M-RET inserts \\item, C-c C-c compiles, C-c C-v views")
                (latex-mode
                 bolp
                 ,(concat "C-c C-s inserts section, C-c C-e inserts environment,"
                          " C-c C-c compiles, C-c C-v views")))
              idlehelp-help-list))

(defun my-LaTeX-help-in-itemize-p ()
  "Return non-nil when we're in an itemize environment."
  (save-excursion
    (and (re-search-backward "\\\\\\(begin{\\(.*\\)}\\|end{itemize}\\)")
         (string= (match-string 2)
                  "itemize"))))

Code:
