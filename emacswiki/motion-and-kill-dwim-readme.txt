A set of motion and kill do-what-I-mean commands.

Here's my setup:

(global-set-key [(meta down)] 'forward-paragraph-dwim)
(global-set-key [(control down)] 'forward-block-dwim)
(global-set-key [(control meta down)] 'up-list)

(global-set-key [(meta up)] 'backward-paragraph-dwim)
(global-set-key [(control up)] 'backward-block-dwim)
(global-set-key [(control meta up)] 'backward-up-list)

(global-set-key [(meta right)] 'forward-word-dwim)
(global-set-key [(control right)] 'forward-word-section-dwim)
(global-set-key [(shift right)] 'forward-word-end-dwim)
(global-set-key [(shift meta right)] 'forward-to-char-dwim)
(global-set-key [(control meta right)] 'forward-sexp)

(global-set-key [(meta left)] 'backward-word-dwim)
(global-set-key [(control left)] 'backward-word-section-dwim)
(global-set-key [(shift left)] 'backward-word-end-dwim)
(global-set-key [(shift meta left)] 'backward-to-char-dwim)
(global-set-key [(control meta left)] 'backward-sexp)

(global-set-key [(meta delete)] 'forward-kill-dwim)
(global-set-key [(control delete)] 'forward-kill-section-dwim)
(global-set-key [(shift meta delete)] 'kill-to-char-dwim)

(global-set-key [(meta backspace)] 'backward-kill-dwim)
(global-set-key [(control backspace)] 'backward-kill-section-dwim)
(global-set-key [(shift meta backspace)] 'backward-kill-to-char-dwim)

(global-set-key "\C-w" 'kill-region-dwim)
(global-set-key "\M-w" 'copy-region-as-kill-dwim)
