1. Setup
Please install either aspell or hunspell and their dictionaries.

2. Usage
Insert below code into ".emacs",
  (add-hook 'prog-mode-hook 'wucuo-start)
  (add-hook 'text-mode-hook 'wucuo-start)

The spell checking starts when current buffer is saved.

Please note `flyspell-prog-mode' and `flyspell-mode' should be turned off
before using this program.

User's configuration for the package flyspell still works.
Flyspell provides two minor modes, `flyspell-prog-mode' and `flyspell-mode'.
They are replaced by this program.  But all the other commands and configuration
for flyspell is still valid.

3. Tips
If `wucuo-flyspell-start-mode' is "normal", `wucuo-start' runs `flyspell-buffer'.
If it's "normal", `wucuo-start' runs `flyspell-region' to check visible region
in current window.

The interval of checking is set by `wucuo-update-interval'.

See `wucuo-check-nil-font-face' on how to check plain text (text without font)

Use `wucuo-current-font-face' to detect font face at point.

You can define a function in `wucuo-spell-check-buffer-predicate'.
If the function returns t, the spell checking of current buffer will continue.
If it returns nil, the spell checking is skipped.

Here is sample to skip checking in specified major modes,
  (setq wucuo-spell-check-buffer-predicate
        (lambda ()
          (not (memq major-mode
                     '(dired-mode
                       log-edit-mode
                       compilation-mode
                       help-mode
                       profiler-report-mode
                       speedbar-mode
                       gud-mode
                       calc-mode
                       Info-mode)))))
