1. Why I write it?
   Sometimes, I use glade draw a interface, I forget what signal I
   used for a widget, I have to swap Emacs from coding to Glade to
   look up for the signal. The other way is open the glade file to
   search. But I hate the markup label, and I want a easy way for
   me to browse the whole file. So I think use tree-widget to
   represent the glade interface is necessary.

2. Can it modify glade file?
   Currently not. But I think it is not hard to edit current
   content in the xml. It really hard to add something to the xml.
   Because I don't know what is valid to add to file.

3. How to invocate glade-mode automaticly when open .glade file?
   Because magic-mode-alist is used before auto-mode-alist, So you
   have to add a regexp to magic-mode-alist:
   (add-to-list 'magic-mode-alist
     '("<\\?xml[ \t\r\n]+[^>]*>[ \t\r\n]*<!DOCTYPE glade-interface" . glade-mode))

4. Where to get and put the icons?
   The icons used can be download from http://glade.gnome.org/download.html.
   For current release, they are in directory pixmaps/16x16. I
   extract the map between the icon and GtkWidget type from
   widgets/gtk+.xml. tree-widget library find icon in directories
   list `tree-widget-themes-load-path'. You can copy pixmaps/16x16
   to one of the directories with name "glade". For example, If you
   add "~/.emacs.d/" to the `tree-widget-themes-load-path', the
   icons should in "~/.emacs.d/tree-widget/glade".

At last, Note this lib use `tree-mode', which can be download from:
http://www.emacswiki.org/cgi-bin/emacs/tree-mode.el

Put this file into your load-path and the following into your ~/.emacs:
  (require 'glade-mode)
