This extension of linum can smart control width of line number
displayed, If visible line number  of current buffer is from 1
to 50, then width of line number is 2, and visible line number
of  current buffer  is from  100 to  150, then  width  of line
number is 3. and use  it, you can customize line number format
with linum+-dynamic-format  even if linum-format  is 'dynamic.
Its                       screenshots                       is
http://emacser.com/screenshots/linum+/linum+1.png           and
http://emacser.com/screenshots/linum+/linum+2.png  .  For more
details,  see article  Use  linum+ smart  display line  number
http://emacser.com/linum-plus.htm

Installation:

Copy linum+.el to your load-path and add to your .emacs:

(require 'linum+)

then use M-x linum-mode to turn on line number displaying
