Introduction
------------

This package implements a dialog box interface using widgets,
frames and windows.

`dialog' package uses the widget package which handle all low level
stuff for deal with buttons, fields, etc.  The buttons, fields,
etc. are created in a buffer.

`dialog' package simplifies the use of the widget package, as the
easymenu package simplifies the menu creation/management.

`dialog' package provides a mechanism to navigate through dialogs.

This package was tested on Emacs 22 and 23.


Using dialog
------------

As an example, here is a very simple dialog specification:

   (require 'dialog)

   (dialog-define hello1
     '(:style window
              [navigation 1 1 :tag "Navigation"]
              [text 3 1 "Hello World 1!!"]
              [button-quit 5 1]
              [button-previous 5 10 :tag "Hello :("])
     "This is a Hello World example.")

   (dialog-define hello2
     '(:style window
              [navigation 1 1 :tag "Navigation"]
              [text 3 1 "Hello World 2!!"]
              [button-quit 5 1]
              [button-next 5 10 hello1 :tag "Hello :)"])
     "This is another Hello World example.")

   (hello2)  ; or (dialog-run 'hello2) run dialog hello2

The following screen is displayed when hello2 executes:

------------------------------------------------------------ hello2
 Navigation: hello2

 Hello World 2!!

 [Quit]   [Hello :)]
------------------------------------------------------------ hello2

If [Quit] button is pressed (by mouse ou keyboard), the dialog box
quits.  If [Hello :)] button is pressed, the dialog hello executes
as seen below.

------------------------------------------------------------- hello
 Navigation: [hello2] :: hello

 Hello World 1!!

 [Quit]   [Hello :(]
------------------------------------------------------------- hello

If [Hello :(] or [hello2] button is pressed, the dialog hello2
executes as seen above.


Interface Functions
-------------------

-- Macro: dialog-define dialog spec doc
	Declare a dialog called DIALOG with items described in SPEC.
	DIALOG does not need to be quoted.

	Second argument SPEC is the dialog specification.

	Third argument DOC is the dialog documentation.

	See _Defining a Dialog Box_ section for SPEC documentation.
	See also _Dialog Derivation_ section.

-- Function: dialog-doc-string dialog
-- Function: dialog-documentation dialog
	Get the documentation string for DIALOG.

-- Function: dialog-make-empty dialog
	Define a new, empty dialog with name DIALOG.
	If the dialog already exists, it is left unmodified.
	Return DIALOG.

-- Function: dialog-run dialog
	Execute DIALOG.  See `dialog-define'.

-- Function: dialog-spec dialog
	Get the DIALOG specification.  See `dialog-define'.

-- Function: dialog-update-text sym
	Update text field associated with symbol SYM.
	See `dialog-define'.

-- Function: dialogp object
	Return t if OBJECT is a dialog object.

-- Function: set-dialog-doc-string dialog doc
-- Function: set-dialog-documentation dialog doc
	Set the documentation string for DIALOG to DOC.

-- Function: set-dialog-spec dialog spec
	Set the DIALOG specification.  See `dialog-define'.


Defining a Dialog Box
---------------------

A dialog box is defined by a list which has the following form:

   (STYLE FIELD...)

Where STYLE specifies how dialog will be opened and dialog
derivation (more about this below), and FIELD is a vector which
specifies a dialog field.

Valid values for STYLE are:

   :style window
	Use the current frame with only one window.

   :style split-window-horizontally
   :style (split-window-horizontally . ARG)
	Split current window horizontally and select the window at
	left.  ARG is optional; if specified, it is passed as argument
	to `split-window-horizontally' function (which see).
	ARG must be an integer.

   :style split-window-vertically
   :style (split-window-vertically . ARG)
	Split current window vertically and select the window above.
	ARG is optional; if specified, it is passed as argument for
	`split-window-vertically' function (which see).
	ARG must be an integer.

   :style frame
   :style (frame . POSITION)
	Make a new frame.  POSITION is optional; it specifies the
	position of the upper left corner of the new frame.
	POSITION can have the following values:

	(X . Y)	the position in pixels.

	point	the current point position.

	mouse	the current mouse position.

	center	the new frame is centralized in the selected frame.

	frame	the upper left corner of the selected frame.

	If POSITION is omitted, the frame position is given by the
	system where Emacs is running.

	If there isn't a windowing system, it behaves as `window'.

   :parent DIALOG
   :parent (DIALOG . DECORATION)
	This dialog derives from dialog DIALOG (the parent dialog).
	DECORATION specifies what to do with decorations (box, hline
	and vline fields).  DECORATION can have the following values:

	keep		keep all parent decoration.

	kill		kill all parent decoration.

	kill-overlap	kill parent decoration only when overlaps with
			some derived dialog field (decoration or not).

	The DECORATION default value is keep.

	See _Dialog Derivation_ section.

STYLE can be omitted, the default value is `:style window'.

The window configuration is saved just before the dialog box
activation and it is restored just after dialog box termination.

There exist the following FIELD types:

   box
   button
   button-cancel
   button-next
   button-ok
   button-previous
   button-quit
   button-reset
   checkbox
   editable
   hline
   menu
   navigation
   radio
   text
   vline

FIELD has the following forms:

   [box LINE COLUMN LINE2 COLUMN2
	:tag TAG]

	Draw a box which diagonal vertices are at LINE and COLUMN, and
	at LINE2 and COLUMN2.
	LINE(2) starts from 1.  COLUMN(2) starts from 0.
	TAG contains the characters used to draw the box border.
	If TAG is omitted, the default value is ".-|++++".
	The TAG string specifies:

	".-|++++"
	 :::::::
	 ::::::+--- bottom left corner
	 :::::+---- bottom right corner
	 ::::+----- top left corner
	 :::+------ top right corner
	 ::+------- vertical
	 :+-------- horizontal
	 +--------- null box (LINE = LINE2 and COLUMN = COLUMN2)

   [button LINE COLUMN
	:tag TAG :notify FUNCTION :help-echo HELP]

	Specify a button at LINE and COLUMN.
	LINE starts from 1.  COLUMN starts from 0.
	If TAG is omitted, "Button" is used.
	When pressed, it executes FUNCTION, if FUNCTION is
	specified.  If FUNCTION is omitted, nothing happens.
	See _Field Keywords_ section below.

   [button-cancel LINE COLUMN
	:tag TAG :notify FUNCTION :help-echo HELP]

	Specify a cancel button at LINE and COLUMN.
	LINE starts from 1.  COLUMN starts from 0.
	If TAG is omitted, "Cancel" is used.
	When pressed, it takes the following steps:
	1. Discard all temporary dialog values;
	2. Execute FUNCTION, if FUNCTION is specified;
	3. Finish the current dialog, that is, return to previous
	   dialog, if exists one.
	See _Field Keywords_ section below.

   [button-next LINE COLUMN DIALOG
	:tag TAG :notify FUNCTION :help-echo HELP]

	Specify a next button at LINE and COLUMN.
	LINE starts from 1.  COLUMN starts from 0.
	If TAG is omitted, "Next" is used.
	If DIALOG is not a dialog, nothing happens.
	If DIALOG is a dialog, when pressed, it takes the following
	steps:
	1. Execute FUNCTION, if FUNCTION is specified;
	2. Go to next DIALOG.
	See _Field Keywords_ section below.

   [button-ok LINE COLUMN
	:tag TAG :notify FUNCTION :help-echo HELP]

	Specify an ok button at LINE and COLUMN.
	LINE starts from 1.  COLUMN starts from 0.
	If TAG is omitted, "Ok" is used.
	When pressed, it takes the following steps:
	1. All temporary dialog values are saved into
	   corresponding variables;
	2. Execute FUNCTION, if FUNCTION is specified;
	3. Finish the current dialog, that is, return to previous
	   dialog, if exists one.
	See _Field Keywords_ section below.

   [button-previous LINE COLUMN
	:tag TAG :notify FUNCTION :help-echo HELP]

	Specify a previous button at LINE and COLUMN.
	LINE starts from 1.  COLUMN starts from 0.
	If TAG is omitted, "Previous" is used.
	If there isn't a previous dialog, nothing happens.
	If there isn a previous dialog, when pressed, it takes the
	following steps:
	1. Execute FUNCTION, if FUNCTION is specified;
	2. Go to previous dialog.
	See _Field Keywords_ section below.

   [button-quit LINE COLUMN
	:tag TAG :notify FUNCTION :help-echo HELP]

	Specify a quit button at LINE and COLUMN.
	LINE starts from 1.  COLUMN starts from 0.
	If TAG is omitted, "Quit" is used.
	When pressed, it takes the following steps:
	1. Discard all temporary dialog values;
	2. Execute FUNCTION, if FUNCTION is specified;
	3. Finish all dialog chain.
	See _Field Keywords_ section below.

   [button-reset LINE COLUMN
	:tag TAG :notify FUNCTION :help-echo HELP]

	Specify a reset button at LINE and COLUMN.
	LINE starts from 1.  COLUMN starts from 0.
	If TAG is omitted, "Reset" is used.
	When pressed, it takes the following steps:
	1. Reset all temporary dialog values, that is, restore the
	   original value for each temporary dialog variable;
	2. Execute FUNCTION, if FUNCTION is specified.
	See _Field Keywords_ section below.

   [checkbox LINE COLUMN VAR
	:tag TAG :notify FUNCTION :help-echo HELP]

	Specify a checkbox at LINE and COLUMN.
	LINE starts from 1.  COLUMN starts from 0.
	VAR is a symbol variable which will hold the checkbox value.
	If TAG is omitted, it is created only the checkbox.
	If TAG is specified, the first character indicates if the TAG
	is positioned at left or right of the checkbox.  If the first
	character is `?-', the TAG is positioned at left of the
	checkbox, that is:

	TAG []

	If the first character is not `?-', the TAG is positioned at
	right of the checkbox, that is:

	[] TAG

	The first character of the TAG is discarded, so, the minimum
	TAG length is 2.
	When pressed, it takes the following steps:
	1. Store VALUE into a temporary dialog variable;
	2. Execute FUNCTION passing VALUE as argument, if
	   FUNCTION is specified.
	See _Field Keywords_ section below.

   [editable LINE COLUMN KIND VAR
	:tag TAG :notify FUNCTION :help-echo HELP
	:size SIZE :action FUNCTION :secret CHAR]

	Specify an editable field at LINE and COLUMN.
	LINE starts from 1.  COLUMN starts from 0.
	VAR is a symbol variable which will hold the editable value.
	KIND specifies the kind of editable field, it can have the
	following values:

	character	a character field.

	coding-system	a MULE coding-system field.

	color		choose a color name (with sample).

	directory	a directory name field.

	file		a file name field.

	float		a floating point number field.

	integer		an integer number field.

	key-sequence	a key sequence field.

	number		a number (floating point or integer) field.

	regexp		a regular expression field.

	sexp		an arbitrary Lisp expression field.

	string		a string field.

	symbol		a Lisp symbol field.

	text		a multiline text area field.

	variable	a Lisp variable field.

	See _Field Keywords_ section below.

   [hline LINE COLUMN LENGTH
	:tag TAG]

	Draw a horizontal line starting at LINE and COLUMN until LINE
	and (COLUMN + LENGTH - 1).
	LINE starts from 1.  COLUMN starts from 0.
	TAG is a string which the very first character is used to draw
	the line.  If TAG is omitted, the default value is "-".

   [menu LINE COLUMN VAR ALIST
	:tag TAG :notify FUNCTION :help-echo HELP]

	Specify a menu at LINE and COLUMN.
	LINE starts from 1.  COLUMN starts from 0.
	VAR is a symbol variable which will hold the menu value.
	ALIST is an association list which has the following form:

	(VALUE . MENU-ITEM)

	Where VALUE is the value which will be stored in VAR when this
	menu item is selected; MENU-ITEM is a string shown as the menu
	item.  VALUE can be a symbol or a string.
	When a menu item is selected, it takes the following steps:
	1. Store VALUE into a temporary dialog variable;
	2. Execute FUNCTION passing VALUE as argument, if FUNCTION is
	   specified.
	See _Field Keywords_ section below.

   [navigation LINE COLUMN
	:tag TAG :help-echo HELP]

	Specify a navigation field bar at LINE and COLUMN which shows
	all dialogs before the current one.
	LINE starts from 1.  COLUMN starts from 0.
	It has the following generic form:

	TAG: [dialog1] :: [dialog2] :: ... :: [dialogN-1] :: dialogN

	Where TAG, if specified, is given by :tag keyword; [dialog1],
	[dialog2] until [dialogN-1] are buttons which go to dialog
	correspondent when the button is pressed.
	See _Field Keywords_ section below.

   [radio LINE COLUMN VAR VALUE
	:tag TAG :notify FUNCTION :help-echo HELP]

	Specify a radio at LINE and COLUMN.
	LINE starts from 1.  COLUMN starts from 0.
	VAR is a symbol variable which will hold the radio value.
	VALUE is the value used when this radio is selected.
	If TAG is omitted, it is created only the radio.
	If TAG is specified, the first character indicates if the TAG
	is positioned at left or right of the radio.  If the first
	character is `?-', the TAG is positioned at left of the
	radio, that is:

	TAG ( )

	If the first character is not `?-', the TAG is positioned at
	right of the radio, that is:

	( ) TAG

	The first character of the TAG is discarded, so, the minimum
	TAG length is 2.
	When pressed, it takes the following steps:
	1. Store VALUE into a temporary dialog variable;
	2. Update all radio which share the same VAR;
	3. Execute FUNCTION passing VALUE as argument, if
	   FUNCTION is specified.
	See _Field Keywords_ section below.

   [text LINE COLUMN TEXT
	:size SIZE]

	Specify a TEXT string to be inserted at LINE and COLUMN.
	LINE starts from 1.  COLUMN starts from 0.
	TEXT can be a string, a symbol or a list.  If TEXT is a symbol
	variable, the variable value must be a string.  If TEXT is a
	symbol function or a function, the function will be evaluated
	without parameters and should returns a string.  If TEXT is a
	list, the list header should be a function, this function will
	be evaluated and the list tail will be the parameters for this
	function; this function should return a string.
	If TEXT is a symbol, `dialog-update-text' can be used by a
	function updates this field.
	See _Field Keywords_ section below.

   [vline LINE COLUMN LENGTH
	:tag TAG]

	Draw a vertical line starting at LINE and COLUMN until (LINE +
	LENGTH - 1) and COLUMN.
	LINE starts from 1.  COLUMN starts from 0.
	TAG is a string which the very first character is used to draw
	the line.  If TAG is omitted, the default value is "|".


Field Keywords
--------------

The keywords specified in a field are optionals.
Below is the keyword documentation.

   :action FUNCTION
	Specify a function FUNCTION which is activated when RET key is
	pressed.  It is passed as argument the value of the editable
	field.  FUNCTION must return a value.
	If the returned value is nil, it means that something goes
	wrong, so the point stays in the current editable field.
	If the returned value is not nil, the point goes to the next
	field.

   :help-echo HELP
	Specifies how to display a message whenever you move to the
	field via keyboard or move the mouse over it.  HELP is either
	a string to display, a function of one argument, the field
	widget, which should return a string to display, or a form
	that evaluates to such a string.

   :notify FUNCTION
	Specify a function FUNCTION which is activated at each change
	of the editable field.  It is passed as argument the value of
	the field.

   :secret CHAR
	Character used to display the value.  You can set this to
	e.g. `?*' if the field contains a password or other secret
	information.  By default, this is `nil', and the value is not
	secret.

   :size SIZE
	Specify the SIZE of string to be displayed.
	It can have the following values:

	integer			the size of string.

	(COLUMNS . LINES)	rectangular text area, both values
				are integers greater than zero.

   :tag TAG
	Usually, specify a field label.
	Some fields use TAG differently, see the field documentation
	above.


Dialog Derivation
-----------------

Sometimes you need to create a dialog B which is almost the same as
another dialog A, but it should add some extra fields in A, or it
should remove some fields from A.  This is what the dialog
derivation do, that is, a way to add/remove some fields from a
dialog in order to create a new one.

To derive a dialog from another one, just specify the :parent in a
dialog definition.  For example:

   (dialog-define example2
     '(:style window
	      [navigation 1 1 :tag "Navigation"]
	      [text 3 1 "Hello World 1!!"]
	      [button-quit 5 1]
	      [button-previous 5 10 :tag "Hello :("])
     "This is the parent dialog.")

   (dialog-define example1
     '(:style (frame . mouse) :parent example2
	      ;; this is a new button
	      [button-quit 7 1 :tag "New Quit Button"]
	      ;; this text field removes the "Hello :(" button
	      [text 5 10 " "])
     "This is the derived dialog.")

So, if the new dialog element overlaps one of parent dialog
elements, the parent dialog element is removed.

The :parent specification have the following values:

   :parent DIALOG
   :parent (DIALOG . DECORATION)

Where DIALOG is the parent dialog and DECORATION specifies what to do
with decoration fields, that is, box, hline and vline fields.

DECORATION can have the following values:

   keep		keep all parent decoration.

   kill		kill all parent decoration.

   kill-overlap	kill parent decoration only when overlaps with
			some derived dialog field (decoration or not).

The DECORATION default value is keep.


Options
-------

Below it's shown a brief description of `dialog' options, please,
see the options declaration in the code for a long documentation.

`dialog-frame-min-width'	Specify frame minimum width, measured
				in characters.

`dialog-frame-min-height'	Specify frame minimum height, measured
				in lines.

`dialog-extra-columns'	Specify extra number of columns,
				measured in characters.

`dialog-extra-lines'		Specify extra number of lines,
				measured in lines.

To set the above options you may:

a) insert the code in your ~/.emacs, like:

	 (setq dialog-frame-min-width 50)

   This way always keep your default settings when you enter a new
   Emacs session.

b) or use `set-variable' in your Emacs session, like:

	 M-x set-variable RET dialog-frame-min-width RET 50 RET

   This way keep your settings only during the current Emacs
   session.

c) or use customization, for example:
	 click on menu-bar *Options* option,
	 then click on *Customize Emacs*,
	 then click on *Browse Customization Groups*,
	 expand *Convenience* group,
	 expand *Dialog* group
	 and then customize `dialog' options.
   Through this way, you may choose if the settings are kept or not
   when you leave out the current Emacs session.

d) or see the option value:

	 C-h v dialog-frame-min-width RET

   and click the *customize* hypertext button.
   Through this way, you may choose if the settings are kept or not
   when you leave out the current Emacs session.


Todo List
---------

- output a rectangular text area
- edit a rectangular text area
- hints and tips section
- scrolling list/vector (probably like a button which opens another
  frame/window)


Acknowledgements
----------------

Thanks to Christoph Conrad <christoph.conrad@gmx.de> for dialog
derivation suggestion.

Thanks to Per Abrahamsen <abraham@dina.kvl.dk> (and to all people
who contributed with him) for developing widget and custom
packages.
