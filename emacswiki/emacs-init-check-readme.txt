Do error check your ~/.emacs.el automatically by:
  emacs -batch --eval '(setq debug-on-error t)' -l ~/.emacs
It is desirable after VC check-in.

If your init file contains error, pop up backtrace.
