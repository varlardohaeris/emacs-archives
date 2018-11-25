Overview
========
This library provides an easy way of adding annotations
to source files and track them in an org file.


INSTALLING
==========
To install this library, save this file to a directory in your
`load-path' (you can view the current `load-path' using "C-h v
load-path RET" within Emacs), then add the following line to your
.emacs startup file:

   (require 'devel-notes)


USING
=====
To add an annotation to a source file run the command
`develnotes-add-annotation', this will let you choose the type and comment
for the annotation, and it will also be saved in `develnotes-file'.
In case there isn't such file you will be prompted the base directory
where to create it.

Note there are more `develnotes-add-TYPE' commands,
look at `develnotes-types-list' variable to find out existing options.

You can create the file `develnotes-file' manually running
`develnotes-create-file'.

To visit `develnotes-file' file in current window, run `develnotes-visit-file',
execute with a C-u to open in other window.


Customizing
===========
You can configure some variables to customize the package behaviour.

`develnotes-file': name of the `devel-notes' tracking file, default:
                   "TODO.org".
`develnotes-types-list': list of accepted types for entries, defaut:
                         ("TODO" "FIXME" "BUG" "WARNING" "NOTE" "HACK" "XXX").
`develnotes-timestamp-format': tTimestamp format, default "%Y/%m/%d %H:%M".


Key map Examples
================
(global-set-key "\C-cza" 'develnotes-add-annotation)
(global-set-key "\C-czv" 'develnotes-visit-file)
(global-set-key "\C-czt" 'develnotes-add-TODO)
(global-set-key "\C-czf" 'develnotes-add-FIXME)


TODO and KNOWN PROBLEMS
=======================
. why tags are not aligned properly?
. add-entry-into-develnotes-file: timestamp as org-timestamp, as org-property?
