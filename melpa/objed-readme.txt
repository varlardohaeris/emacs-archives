A global minor-mode to navigate and edit text objects. Objed enables modal
editing and composition of commands, too. It combines ideas of other Editors
like Vim or Kakoune and tries to align them with regular Emacs conventions.

For more information also check my blog
https://www.with-emacs.com/categories/objed/.

Text objects are textual patterns like a line, a top level definition, a
word, a sentence or any other unit of text. When `objed-mode' is enabled,
certain editing commands (configurable) will activate `objed' and enable its
modal editing features. When active, keys which would usually insert a
character are mapped to objed commands. Other keys and commands will continue
to work as they normally would and exit this editing state again.

By default important self inserting keys like Space or Return are not bound
to modal commands and will exit `objed' on insertion. This makes it
convenient to move around and continue adding new text.

With activation `objed' shows the current object type in the mode-line. The
textual content of the object is highlighted visually in the buffer and the
cursor color is changed, too. The user can now navigate by units of this
object, change the object state or switch to other object types.

The object state is either "inner" or "whole" and is indicated in the
modeline by (i) or (w) after the object type. With inner state, anything that
would be considered delimiters or padding around an object is excluded.

The user can apply operations to objects. By marking objects before applying
an operation, s?he can even operate on multiple objects at once. This works
similar to the way you interact with files in `dired'. When marking an object
the point moves on to the next object of this type.

The object type used for initialization is determined by the mapping of the
entry command (see `objed-cmd-alist'). For example using `beginning-of-defun'
will activate `objed' using the `defun' object as initial object type. With
command `next-line', `objed' would initialize with the `line' object.

Objeds modal state provides basic movement commands which move by line, word
or character. Those switch automatically to the corresponding object type,
otherwise they work the same as the regular Emacs movement commands. Other
commands only activate the part between the initial position and the new
position moved to. By repeating commands you can often expand/proceed to
other objects. This way you can compose movement and editing operations very
efficiently.

The expansion commands distinguish between block objects (objects built out
of lines of text) and context objects (programming constructs like strings,
brackets or textual components like sentences). This way you can quickly
expand to the desired objects.

For example to move to the end of the paragraph, the user would first move to
the end of the line with "e". This would activate the text between the
starting position and the end of the line. The user can now continue to the
end of the paragraph by by pressing "e" again. Now s?he is able to proceed
even further by pressing "e" again OR to continue by adding new text to the
end of the paragraph OR to continue by acting on the text moved over, for
example killing it by pressing "k".

As often with text editing, the explanation sounds more complicated than
using it. To get a better impression of the editing workflow with `objed'
have look at https://github.com/clemera/objed where you can find some
animated demos.

To learn more about available features and commands have a look at the
descriptions below or the Docstrings and bindings defined in `objed-map'. To
define your own operations and text objects see `objed-define-op' and
`objed-define-object'.

Although some features are still experimental the basic user interface will
stay the same. The following gives an overview of available keys and
commands:

Basic movement commands (switch the object type on movement):

[`objed-map']
f/b: Move forward/backward one character and activate the char object.
s/r: Move forward/backward one word and activate the word object (*).
S/R: Move forward/backward one symbol and activate the symbol object.
     Skips strings and comments.
n/p: Move to the next/previous line and activate the line object.

(*): Emacs uses the f/b keys for word AND character movement (depending on
     the used modifier). I made the decision to remap M-f, M-b to M-s, M-r in
     my personal configuration (using `key-translation-map'). This is by no
     means necessary but might be something to think about. Afterwards you
     are able to start word movement with M-s/r and continue moving by words
     with s/r which is more convenient than switching between M-b/f and s/r.
     You can still access M-s/r regular bindings by using Meta-shift (M-S/R).

Commands for block objects (objects built out of lines of text):

[`objed-map']
l  : Activate (line based) object at point and move to its start.
     On repeat proceed to beginning of the indentation block, comment block,
     paragraph or other block objects.
a  : Move to beginning of line and activate the text moved over.
     On repeat proceed to beginning of blocks like explained above.
e  : Move to end of line and activate the text moved over.
     On repeat proceed to end of blocks like explained above.

Commands for context objects. Those objects are common programming constructs
like defuns, strings, parentheses but also sentences inside comments for
example. Any whitespace after point is skipped before determine the context:

[`objed-map']
t/h: If coming from basic movement the object at point is guessed
     based on context. Point is moved  to the beginning/end of object.
     This is useful to "jump out" of the current context.
     On repeat move to the previous/next instance of current object type
o  : Activate the inner part of the object at point and move to the start.
     This is useful to act on the content of the string, brackets and so on.
     On repeat expand to other objects around current position.
u  : Move to end of the inner object at point and activate the text moved over.
     On repeat toggle between beginning/end inside the object.
q  : Move point to the other side of the current object.
i  : Toggle the state of the current object (inner/whole).

Commands to switch to other objects (and move point to its start):

[`objed-map']
c  : Prefix to switch to other objects,
     see `objed-object-map' for available objects
     and `objed-define-object' to add your own.

Some objects are directly accessible without a prefix:

.  : Switch To identifier object.
_  : Switch to symbol object.
%  : Switch to content object (defined by textual content of current object).

Commands to edit objects (applying operations to them). When the region is
active the operation acts on the current region. To act on multiple objects
at once you can mark them first (see the "Misc commands" below):

[`objed-map']
x  : Prefix to access other operations,
     see `objed-op-map' for available operations
     and `objed-define-op' to add your own.

Some operations are accessible without a prefix:

k  : Kill current object(s).
     Merges marked objects if they are only separated by whitespace.
d  : Delete current object(s).
     Merges marked objects if they are only separated by whitespace.
w  : Copy current object(s).
y  : Yank last killed text at point.
TAB: Indent object(s).
 : Un/comment object(s).
:  : Comment and duplicate object.
$  : Flyspell region.

`      : Prefix to surround object(s) with any pair using `electric' (built-in).
"([{   : Surround object with corresponding chars using `electric',

Misc commands:

[`objed-map']
,    : Pop to last state, which restores the last position and any object data.
j    : Choose an instance of current object type with completion,
       using the contents of the first line for completion.
z    : Choose an instance of current object type on the screen with `avy'.
m    : Add/Remove current object to marked objects and move to next.
M    : Add/Remove current object to marked objects and move to previous.
v    : Scroll up.
V/M-v: Scroll down.
/    : Undo last edit command.
C-SPC: Set mark.
C-h  : Get key binding help (uses `which-key' if available).
g/C-g: Exit and deactivate `objed'.

Dispatch keys (dispatch to any object defined in `objed-object-map'):

[You can add your own prefix bindings using `objed-define-dispatch']
 *  : Mark all instances of current object inside another object type.
 <  : Activate part from point backward until boundary of some object.
 >  : Activate part from point forward until boundary of some object.


CONTRIBUTE:

I'm happy to receive pull requests or ideas to improve this package. Some
parts suffer from the bottom up approach of developing it, but this also
allowed me to experiment a lot and try ideas while working on them, something
that Emacs is especially good at. Most of the features are tested using
`emacs-lisp-mode' but hopefully there aren't to many problems using modes for
other languages, I tried my best to write text objects in a language agnostic
way by using Emacs syntax information. Testing this and writing tests in
general would be an important next step.

This package would never been possible without the helpful community around
Emacs. Thank you all and see you in parendise...Share the software!
