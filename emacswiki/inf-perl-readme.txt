   This is a customisation of comint-mode (see comint.el)

Put this file into your load-path and the following into your ~/.emacs:
  (require 'inf-perl-mode)
  In windows expand-file-name is require because perl don't where is ~/psh.pl
  (setq perl-shell-program (expand-file-name "~/psh.pl"))

You may get this `psh.pl' from my homepage or write it yourself:
http://learn.tsinghua.edu.cn:8080/2005211356/src/psh.zip
