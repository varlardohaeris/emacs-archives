If you use images instead of letters, you should set the varible:

  (setq eight-puzzle-use-image t)
  (setq eight-puzzle-pics-path "path/to/images/")
  (setq eight-puzzle-pics-name "name")

The eight-puzzle-pics-name is the oringal image name, such as
"x.png". And this image split to nine part, which the first in the
top left corner is substitued by a blank image that has the same
width and height. These nine images should name in order of number,
such as "x0.png", "x1.png", ... "x8.png".

Put this file into your load-path and the following into your ~/.emacs:
(autoload 'eight-puzzle "eight-puzzle" nil t)
