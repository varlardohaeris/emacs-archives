auto-indent-mode keeps source code indented automatically.  When
enabled, various editing commands are rebound to variants that
treat a newline along with any following indentation as a single
unit and make sure that the current line is correctly indented.
If, for example, you have

   (defun foo ()|)                                              [1]

(where | is the cursor) and press <return>, you get

   (defun foo ()                                                [2]
     |)

If you then press <backspace>, you get [1] again.  C-o and C-d
behave in the same manner.

In the same way that moving up and down lines with the point at the
end of the line warps the point at the last character of the line,
moving up and down lines with auto-indent-mode enabled and the
point at the beginning of the line warps the point to the
first non-whitespace character.

Yanks are also automatically indented.

If you don't like this mode, check out this unrelated
implementation of the same concept:
  http://www.emacswiki.org/emacs/auto-indent-mode.el
