   Functions dealing with accented characters.

 New functions defined here:

   `accented-char-p', `unaccent-char', `unaccent-region', `unaccent-word'.

 New variable defined here:

   `reverse-iso-chars-alist'.

 Suggested bindings:

  (global-set-key [(meta ?\")] 'unaccent-word)
  (define-key ctl-x-map [\"] 'unaccent-region)
