 emacs-lock+ - extensions to standard library `emacs-lock.el'

 Features:

 - When locking any buffer, you are prompted for a little note that
   helps you to remember why you locked the buffer.

 - If any buffer prevents Emacs from exiting, pops up this buffer
   and displays your note if there is one.  Also displays the note
   if you try to kill any locked buffer (and, of course, prevents
   from killing)

 - Adds a mode-line indicator which displays a capital "L" for
   locked buffers (and a lowercase "l" for non-locked buffers, if
   this is enabled.  You can deactivate the indicator if you don't
   want it.)
   If you move the mouse over the indicator, the locking note
   is displayed, and (un-)locking can be done with the mouse.

 Installation: Put this file into a directory in your load-path and
 byte compile it.  Add

    (require 'emacs-lock+)

 to your Emacs init file (usually ~/.emacs).

 See (customize-group "emacs-lock") for options.


 ***** NOTE: The following user option defined in
             `emacs-lock.el' has been REDEFINED HERE:

 `emacs-lock-from-exiting' - string values are handled specially


 ***** NOTE: The following variable defined in
             `bindings.el' has been REDEFINED HERE:

 `mode-line-modified' - buffer lock indicator appended


 ***** NOTE: The following functions defined in `emacs-lock.el'
             have been REDEFINED HERE:

 `check-emacs-lock' -
    - pop to locked buffer
    - handle string values of `emacs-lock-from-exiting' ("locking notes")
 `emacs-lock-check-buffer-lock' -
    - handle string values of `emacs-lock-from-exiting'
 `toggle-emacs-lock' -
    - allows specification of notes
    - uses prefix arg
    - forces mode-line update



This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth
Floor, Boston, MA 02110-1301, USA.


Change log:

2010_05_18 heerd
  - added some comments and doc for `emacs-lock-from-exiting'
  - corrected wrong URL
2010_05_14 heerd
  created

TODO:

- make this a minor mode?
