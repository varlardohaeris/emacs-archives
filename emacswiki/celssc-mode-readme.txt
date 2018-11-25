Commentary:
Major mode for editing celestia catalog files.
This minor mode is based on
tutorial that can be found here:
http://two-wugs.net/emacs/mode-tutorial.html

If celssc-mode is not part of your distribution, put this file into your
load-path and the following into your ~/.emacs:

(autoload 'celssc-mode "celssc-mode" "Major mode for editing celesita's solar system catalog files" t)
(setq auto-mode-alist (cons '("\\.ssc" . celssc-mode)
                                  auto-mode-alist))
(autoload 'celssc-mode "celssc-mode" "Major mode for editing celesita's star catalog files" t)
(setq auto-mode-alist (cons '("\\.stc" . celssc-mode)
                                  auto-mode-alist))
(autoload 'celssc-mode "celssc-mode" "Major mode for editing celesita's deep sky catalog files" t)
(setq auto-mode-alist (cons '("\\.dsc" . celssc-mode)
                                  auto-mode-alist))
