The best explanation of what this program does is to test it, but
I'll try to explain the best I can.

A more clear explanation of the concept itself can be seen on my
homepage: http://mathias.dahl.net/gqsmc.html

It's only useful (I think) if you are running NTEmacs with
Win95/98/NT4/2000. I now that in W98 and W2000, the system does
some smart placing of the shortcuts depending on how often you use
one.  I don't know how this will affect the behaviour of this
program, but I'll guess you have to test and see. I've heard that
you can turn off this feature in W98.

mss uses the standard windows Start Menu as the root for a shortcut
tree from where you can call your favourite shortcuts without
fiddling around with the mouse (of course you can use the mouse if
you want to).

As most of you know, you can activate the Start Menu with the
WIN-key (if you haven't got such a key, C-ESC does the same thing)
and the press the key that matches the menu choise you want.

The problem is when two or more menu choices begin with the same
key. This is why I place the shortcut tree under the folder "-". A
hyphen is not likely the first character of some other menu
choice. Of course you can use another valid character as the root
tree but I think the hyphen is easy to access on my swedish
keyboard.

Note that it doesn't matter if the root directory s called "-", "-
My smart shortcuts" or something, the important thing is that it
starts with an unusual character.

The program doesn't do anything that you cannot do yourself, it's
just a little more convenient. It uses standard windows Start Menu
functionality, just in a little different way.

This program will place a standard windows-shortcut file (actually
any kind of file) at the right place in your Start Menu "shortcut
tree". It will place it and name it in such a way that for you to
call your shortcut you only have to type Win - (the windows key
plus a hyphen) and the first few characters of the name of the
shortcut.

These characters depends on how many other shortcuts with similar
starting name you have. I.e if you only have one shorctut named
"Emacs.lnk", it will be placed just below the root of the tree and
to call it you'd type: Win - E (Windows key, a hyphen and an E).

As I said, the easiest way to see what it does is to test it. Make
a couple of shortcuts on your desktop (as this is the default path
for fetching shortcuts to place in the tree) and run the function
mss.

An example of how a shortcut tree can look like:

\(START = language dependent path to start menu, taken from the
registry, for example d:\winnt\profiles\mathias\Start Menu\)

START\
      - My shorcuts\
                    C\H - cHaracter map
                      A - cAlc
                      Y - cYgwin
                    E - Emacs
                    N\O - nOtepad
                      E - nEtscape
                    O - Opera
                    W\I - wIndow blinds
                      E\
                        B\W - webWasher
                          T - webTime

Asuming you have a tree in the example above, if I wanted to place
a shortcut named Explorer, I'd have to create a subdirectory and
rename the Emacs shortcut and then place this new shortcut so that
the new tree would look like this:

 E\M - eMacs
   X - eXplorer

And maybe later, I want a shortcut to Excel, this would look like
this:

 E\M - eMacs
   X\C - exCel
     P - exPlorer

The result is a unique sequence of characters for each shortcut. To
start Excel you'd type: Win - e x c

This means that you dont have to remember strange key combinations,
just remember the name of the shortcut and type on until it starts.

To run the mss-function itself, I have a shortcut called mss that
uses gnudoit to raise or start Emacs and call mss.

Example of shortcut command line for starting mss:

%HOME%\bin\gnudoit.exe [continued on the next line] (progn
(load-library \"mss\") (raise-frame (selected-frame)) (mss))

History

 2000-04-01  Mathias Dahl       0.0.1

                                First release

 2000-04-03  Mathias Dahl       0.0.2

                                Made all free variables local (I think)

                                Bug corrected: the user could
                                choose and invalid path for the
                                shortcut tree and this gave
                                *strange* effects.

                                The user can now choose whether to
                                move or copy the shortcut with the
                                variable mss-move-shortcut-flag.

                                Handles the situation when the user
                                wants both these shortcuts
                                (example): hello.lnk and helloo.lnk

 2000-04-13  Mathias Dahl       0.0.3

                                Noticed problems with
                                null-characters (ascii 0) in
                                mss.tmp. These characters were not
                                there in NT4.  Had to rewrite parts
                                of mss-get-shell-folder-path.  All
                                in all, some things dont seem to
                                work in W2k

2000-04-14   Mathias Dahl       0.0.4

                                Fixed minor problems (found some
                                more free vars)

2000-04-15   Mathias Dahl       0.0.5

                                Added simple drag-n-drop
                                functionality. This is provided
                                with the mss-drag-n-drop-mode

Bugs

There are probably situations with unknown outcome that can
arise. Not much error catching for now.
