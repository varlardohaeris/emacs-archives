This set of functions are used to process queries with
placeholders like ':var'. Statement is parsed for those, then user
asked for values and placeholders are replaced with values quoted
with apostrophes. Not the smartest way to do it, but something.

You can replace ordinary sql-send-* function with my
*-with-placeholders variants:

(eval-after-load "sql"
 '(load-library "sql-with-placeholders"))

(add-hook 'sql-mode-hook (lambda nil
                           (local-set-key [(control c) (control b)] 'sql-send-buffer-with-placeholders)
                           (local-set-key [(control c) (control c)] 'sql-send-paragraph-with-placeholders)
                           (local-set-key [(control c) (control r)] 'sql-send-region-with-placeholders)))
