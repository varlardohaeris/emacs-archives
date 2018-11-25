Installation

Add kokopelli.el to your load path
add your .emacs

(require 'kokopelli)
(define-key global-map [f12] 'kokopelli-sing)


Usage

Type F12 to list up functions.

To jump to it, type SPACE or ENTER or double left click on any function in the list.

Type q to quit kokopelli.


Options

If you want to close kokopelli window with your selecting the function, add your .emacs

(setq kokopelli-auto-quit t)

If you want to change the position where the function you selected will appear, use

kokopelli-margin-top

eg. With 0 , the function will appear at the top of the window.
(setq kokopelli-margin-top 0)
