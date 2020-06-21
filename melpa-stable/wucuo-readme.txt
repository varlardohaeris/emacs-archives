1. Setup
Please install either aspell or hunspell and their dictionaries.

2. Usage
Insert below code into ".emacs",
  (add-hook 'prog-mode-hook 'wucuo-start)
  (add-hook 'text-mode-hook 'wucuo-start)

The spell checking starts when current buffer is saved.

Please note `flyspell-prog-mode' and `flyspell-mode' should be turned off
before using this program.

3. Tips
If `wucuo-flyspell-start-mode' is "normal", `wucuo-start' runs `flyspell-buffer'.
If it's "normal", `wucuo-start' runs `flyspell-region' to check visible region
in current window.

The interval of checking is set by `wucuo-update-interval'.

See `wucuo-check-nil-font-face' on how to check plain text (text without font)
