Simple package to convert remind's `simple calendar' output to Emacs diary
format and save the diary entries to a file.

Installation:

Remind can be obtained from http://www.roaringpenguin.com/products/remind.

To use this modest library, put this file in your Emacs-Lisp load path and
add the following to your startup file

   (require 'remind-to-diary)

Description:

Generate a diary file from remind with the interactive function

   (remind-to-diary-run)

This will save diary entries, one for each line of remind's simple calendar
output, to the file REMIND-TO-DIARY-DIARY-FILE (~/.diary-remind, by default).

The following hook automates this process.

   (add-hook 'diary-list-entries-hook 'remind-to-diary-run)


To configure the diary facility to use this file, use the following hook

   (add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)

and add the following line to your diary-file (by default ~/diary)

   #include "~/.diary-remind"
