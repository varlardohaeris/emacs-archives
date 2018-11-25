This package allows Emacs to copy to and paste from the GUI clipboard
when running in text terminal.

It relies on external command-line tools for that, which you may need
to install in order for the package to work.
More specifically, it can use the following tools:
- Under X11: `xclip' or `xsel' (http://xclip.sourceforge.net and
  http://www.vergenet.net/~conrad/software/xsel/ respectively).
- MacOS: `pbpaste/pbcopy'
- Cygwin: `getclip/putclip'

To use, just add (xclip-mode 1) to your ~/.emacs or do `M-x clip-mode'
after which the usual kill/yank commands will use the GUI selections
according to `select-enable-clipboard/primary'.