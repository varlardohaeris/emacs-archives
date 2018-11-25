* Who need it?
 If you only write hundreds lines in .emacs, you won't consider this
 extension. But if you have thousands lines in .emacs, you may want
 such thing to organize configuration like me.

* How to use it?
 Put this file into your load-path and the following into your ~/.emacs:
  (require 'dot-emacs-helper)

 I recommend write this in .emacs for port config to other emacs that
 don't have this extension:

    (unless (require 'dot-emacs-helper nil t)
      (defmacro deh-require-maybe (feature &rest forms)
        (declare (indent 1))
        `(progn (when (require ,feature nil t) ,@forms)))
      (defalias 'deh-require 'deh-require-maybe)
      (put 'deh-require 'lisp-indent-function 1)
      (defmacro deh-section (section &rest forms)
        (declare (indent 1))
        `(progn ,@forms)))

 And rewrite you .emacs as:

   (deh-require 'feature-name
     configuration-for-the-feature)
   (deh-section "section-name"
     some-configuration)

 And when you want edit some configuration, use M-x
 `deh-customize-inplace' or M-x `deh-customize' to make changes. It
 knows where you put the configuration. The former maybe more
 reliable, but I like `deh-customize' more.
