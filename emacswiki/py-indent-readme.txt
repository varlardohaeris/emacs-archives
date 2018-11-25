This package provides support for annotating Python code with C-like block
delimiters (in Python comments) that allow the indentation of the file to
be reconstructed after modification or damage.  Annotations may be
generated automatically from the current indentation; the user may freely
add or remove annotations to change the extent of blocks, and then apply
the annotations to update or restore the indentation.  Entry points:

`pyi-annotate' - add annotations to the buffer based on indentation
`pyi-unannotate' - remove all annotations
`pyi-apply' - recalculate all indentation from annotations
`pyi-line' - recalculate one line's indentation

The automatic annotation and unannotation try to be smart about placement
of the annotations and cleaning up blank spots where they are removed.

Warning: annotations, if used, must exist for a whole buffer, because a
region with none added can't be distinguished from one that should be
evenly indented!

This code attempts to work with both "python-mode.el" and "python.el".
