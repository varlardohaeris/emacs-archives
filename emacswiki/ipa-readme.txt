With this package you can add annotations to your files without
modifying them. Each file can have multiple annotations at various
buffer positions. The annotation texts are not parts of the files,
they are stored separately.

All annotations are stored in a common file, so searching
annotations is trivial.


Installation:

  (require 'ipa)


The following commands can be used:

  ipa-insert   - insert annotation at point

  ipa-edit     - edit the first annotation after point
                 (with universal argument: before point)

  ipa-next     - goes to the next annotation in the buffer

  ipa-previous - goes to the previous annotation in the buffer

  ipa-move     - move the first annotation after point
                 (with universal argument: before point)

  ipa-toggle   - hide/show annotations

  ipa-show     - show all saved annotations for the current file
                 (in the storage buffer you can press Enter on any
                  annotation to go to its location)

  ipa-jump     - jump to any annotation with id completion

                 Annotations can optionally have ids in their
                 text with the following format: [id]annotation-text

                 The id itself doesn't appear in the annotated
                 buffer. It only serves the purpose of giving a
                 unique id to the annotation, so that you can jump
                 to it quickly.

                 If an annotation has an id, but no other text
                 then it is effectively the same as a usual
                 bookmark in emacs.

                 Only annotations appearing in `ipa-file' can be
                 jumped to, so unsaved annotations does not count.
                 If there are more annotations defined with the
                 same id then the first one found in `ipa-file' is
                 used.


Annotations are saved when the file itself is saved. If the file
is not modified annotations are saved immediately when
added/changed.


Tested on Emacs 22.
