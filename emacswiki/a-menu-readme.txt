This package provides `a-menu' function.
It enables you to build your own menu in the menu bar,
based on the structure of the specified directory.

Let us assume the following files and directories:

    ~/directory/
    ~/directory/Test
    ~/directory/Test/Item_1.el
    ~/directory/Test/Item_2.el.

The code

    (a-menu "~/directory/Test")

will create "Test" menu in the menu bar, and the names of
the *.el files are shown in it. Choosing one of menu items loads
the relevant *.el file, and so emacs executes lisp codes in it.
To create a hierarchical structure in the menu, just make subdirectories.
