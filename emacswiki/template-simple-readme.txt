* Why not template?
  A template.el is already exists, and it does everything well.
  But I hate to read the code to use it in my extension. I need
  simple thing to get work done. template-simple is designed
  to compatible with template. The two useful features are
  implemented, expand template in file and update file header.
  And with addtional, you can use this to write simple skeleton
  and tempo template. Or you can implement other expand function
  to expand the parsed templates.

* Where to use it?
  You can use it with autoinsert, tempo, skeleton or other related
  extensions. I hope this help you to write template for tempo or
  skeleton without any knowledge with emacs lisp.

* Tips
  If you don't like the (>>> and <<<) for open and close paren,
  you can overwrite it like file variable in template, for example:

  (template-simple-expand
   ";; -*- template-parens: (\"{\" . \"}\"); template-expand-function: template-tempo-expand -*-
  (defun {p} ({p})
    \"{p}\"
    {p}
    )")

  The template is expand by template-tempo-expand and use {} as paren inside
  template string.

Dependencies:
 no extra libraries is required

Installation:
Put this file into your load-path and the following into your ~/.emacs:
  (require 'template-simple)
