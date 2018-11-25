This program provides some functions which are compatible with the
ones provided by popup.el. User can easily replace popup.el's tooltip
functions by defining advice (see below).

Settings:

Save this file as popup-pos-tip.el in a directory which is listed
in load-path, and add the following in your .emacs file:

(require 'popup-pos-tip)
(defadvice popup-tip
  (around popup-pos-tip-wrapper (string &rest args) activate)
  (if (eq window-system 'x)
      (apply 'popup-pos-tip string args)
    ad-do-it))

History:
2010-04-29  S. Irie
        * Changed `popup-pos-tip' to hide tooltip after waiting key event
        * Fixed incorrect tooltip width for multibyte character string
        * Added constant `popup-pos-tip-version'
        * Version 0.1.2

2010-04-16  S. Irie
        * Changed `popup-pos-tip' not to fill paragraph unless exceeding :width
        * Version 0.1.1

2010-03-29  S. Irie
        * First release
        * Version 0.1.0
