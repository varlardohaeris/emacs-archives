Purpose of this package: This is a mode for editing programs written
  in Visual Basic .NET (VB.NET).  This mode features automatic
  indentation of VB.NET syntax; font locking; automatic keyword
  capitalization; integration with compile.el, flymake.el, and
  imenu.el; built-in snippets for ya-snippet.el; and some minor
  convenience functions.

Installation instructions
--------------------------------

 Put vbnet-mode.el somewhere in your load path, optionally byte-compile
 it, and add the following to your .emacs file:

   (autoload 'vbnet-mode "vbnet-mode" "Mode for editing VB.NET code." t)
   (setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|vb\\)$" .
                                 vbnet-mode)) auto-mode-alist))

 Optionally, add a mode-hook function.  To do so, use something
 like this to your .emacs file:

   (defun my-vbnet-mode-fn ()
     "My hook for VB.NET mode"
     (interactive)
     ;; This is an example only.
     ;; These statements are not required to use VB.NET, but
     ;; you might like them.
     (turn-on-font-lock)
     (turn-on-auto-revert-mode)
     (setq indent-tabs-mode nil)
     (require 'flymake)
     (flymake-mode 1)
     ...other mode-setup code here...
   )
   (add-hook 'vbnet-mode-hook 'my-vbnet-mode-fn)


 General
 ----------------------------

 Mostly VB.NET mode will "just work."  Use `describe-mode' to see the
 default keybindings and the highlights of the mode.

 Some points of interest:

    `vbnet-mode-indent' - customizable variable setting the indent size,
         in spaces.  The default is 4.

    `vbnet-mark-defun' marks the current function, if there is one.

    `vbnet-split-line' splits the current line at point, and inserts a
         continuation character.

    `vbnet-join-continued-lines' does the converse.

    `vbnet-new-sub' - inserts a subroutine template into the buffer at
         point.

    `vbnet-moveto-beginning-of-defun'
    `vbnet-moveto-end-of-defun'
    `vbnet-moveto-beginning-of-block'
    `vbnet-moveto-end-of-block'
         Functions to move within the VB.NET buffer.  The first two
         move to the beginning and end, respectively, of a Function
         or Sub.  The latter two move to the beginning and end,
         respectively, of the innermost containing block, whatever it
         is - a Function, Sub, Struct, Enum, While, etc.

    `vbnet-close-current-block' - intelligently closes a block, For
         example, it inserts "End Class" when invoked if point is
         after a Class declaration.  This fn is naive: it
         will insert an "End Class" even if an "End Class" is present
         on the next line, and likewise for any other type of block.

 Some of these functions are by default, bound to keystrokes when
 vbnet-mode loads.  Consult the documentation for each of these
 functions for more information.


 Fontification
 ----------------------------

 There are a couple font-face name variables you can use to control
 the appearance of fontified vb.net buffers.  In
 `vbnet-namespace-face', store the name of the face for namespaces
 (from the Imports statement).  In `vbnet-funcall-face', store the
 name of the font to use for function calls.  There are also faces
 with the same names as these variables; they represent the default
 faces for these classes of code syntax.

 You can set `vbnet-want-fontification' (via customize) to nil to
 skip the fontification.


 Flymake Integration
 ----------------------------

 You can use flymake with vb.net mode to automatically check the
 syntax of your vb.net code, and highlight errors.  To do so, add a
 comment line like this to each .vb file that you use flymake with:

  '  flymake-command: c:\.net3.5\vbc.exe /t:module /nologo

 That lines specifies a command "stub".  Flymake appends the name of
 the file to compile, and then runs the command to check
 syntax.  Flymake assumes that syntax errors will be noted in the
 output of the command in a form that fits one of the regexs in the
 `compilation-error-regexp-alist-alist'.  Check the flymake module for
 more information on that.

 Some rules for the command:

   1. it must appear all on a single line.

   2. vbnet-mode generally looks for the marker line in the first N
      lines of the file, where N is set in
      `vbnet-cmd-line-limit'.  See the documentation on that
      variable for more information.

   3. the command SHOULD NOT include the name of the source file
      currently being edited.  This is because flymake saves a copy of
      the buffer into a temporary file with a unique name, and then
      compiles that temporary file.  The name of the temp file is
      automatically appended to the end of the command .

 If you use vbc.exe as the syntax check tool (as almost everyone
 will), the /t:module is important.  vbnet-mode assumes that the
 syntax-check compile command will produce a file named
 NAME.netmodule, which is the default when using /t:module.  (Remember
 than NAME is dynamically generated).  vbnet-mode will remove the
 generated netmodule file after the syntax check is complete.  If you
 don't specify /t:module, then vbnet-mode won't know what file to
 delete.

 vbnet-mode also fiddles with some other flymake things.  In
 particular it: adds .vb to the flymake "allowed filename masks"; adds
 parsing for vbc error messages; redefines the process sentinel to
 not treat non-zero exit status as an error (because vbc.exe returns
 non-zero status when syntax errors are present); and adds advice to
 the error parsing logic.  This all should be pretty benign for all other
 flymake buffers.  But it might not be.

 You can explicitly turn the flymake integration for VB.NET off by
 setting `vbnet-want-flymake-fixup' to nil.


 Imenu integration
 ----------------------------

 imenu provides the capability to scan a buffer to produce an "index"
 of the highlights or points of interest in that buffer, and then
 present that index as a popup menu (for example on the menubar) or
 completion buffer.

 vbnet-mode integrates with imenu to produce the index for a vb.net
 buffer.  Automatically, there will be an "index" menu item in the
 menubar when you open a VB.NET buffer.  It will contain menu items
 corresponding to namespaces, classes, methods, functions and
 properties defined in that source module.

 To turn off the vbnet+imenu integration, set vbnet-want-imenu to
 nil, in your vbnet mode hook function.

 Compile Integration
 ----------------------------

 vbnet-mode binds the function `vbnet-invoke-compile-interactively'
 to "\C-x\C-e" .  This function attempts to intellgently guess the
 format of the compile command to use for a buffer.  It looks in the
 comments at the head of the buffer for a line that begins with
 compile: .  If found, vbnet-mode suggests the text that follows as
 the compilation command when running `compile' .  If such a line is
 not found, vbnet-mode falls back to a msbuild or nmake command.
 See the documentation on `vbnet-cmd-line-limit' for further
 information.

 vbnet-mode now installs an error regexp for vbc.exe into
 `compilation-error-regexp-alist-alist', which allows `next-error'
 and `previous-error' (defined in compile.el) to navigate to the next
 and previous compile errors in the vb buffer.


 YASnippet integration
 -----------------------------

 vbnet-mode (as of v1.5b) defines some built-in snippets for
 convenience.  For example, if statements, ifelse, for, foreach, and
 so on.  You can see them on the YASnippet menu that is displayed
 when a vbnet-mode buffer is opened.  vbnet-mode defines this
 snippets happens only if ya-snippet is available.  (It is done in an
 `eval-after-load' clause.)  The builtin snippets will not overwrite
 snippets that use the same name, if they are defined in the normal
 way (in a compiled bundle) with ya-snippet.

 You can explicitly turn off ya-snippet integration.  See the var,
 `vbnet-want-yasnippet-fixup'.





Revisions:
1.0 18-Apr-96  Initial version

1.1 Accomodate emacs 19.29+ font-lock-defaults
    Simon Marshall <Simon.Marshall@esrin.esa.it>

1.2 Rename to visual-basic-mode

1.3 Fix some indentation bugs.

1.3+ Changes by Dave Love: [No attempt at compatibility with
     anything other than Emacs 20, sorry, but little attempt to
     sanitize for Emacs 20 specifically.]
     Change `_' syntax only for font-lock and imenu, not generally;
     provide levels of font-locking in the current fashion;
     font-lock case-insensitively; use regexp-opt with the font-lock
     keywords; imenu support; `visual-basic-split-line', bound to
     C-M-j; account for single-statement `if' in indentation; add
     keyword "Global"; use local-write-file-hooks, not
     write-file-hooks.

1.4 September 1998

1.4 KJW Add begin..end, add extra keywords
    Add customisation for single line if.  Disallow by default.
    Fix if regexp to require whitespace after if and require then.
    Add more VB keywords.  Make begin..end work as if..endif so
    that forms are formatted correctly.

1.4.1 KJW Merged Dave Love and KJW versions.
    Added keywords suggested by Mickey Ferguson
    <MFerguson@peinc.com>
    Fixed imenu variable to find private variables and enums

    Changed syntax class of =, <, > to punctuation to allow dynamic
    abbreviations to pick up only the word at point rather than the
    whole expression.

    Fixed bug introduced by KJW adding suport for begin...end in
    forms whereby a single end outdented.

    Partially fixed failure to recognise if statements with
    continuations (still fails on 'single line' if with
    continuation, ugh).

1.5 DPC changes February 2011
    Moved the `provide' statement to conventional spot, end of file.

    Modified various `defvar' statements to be `defcustom',
    to allow these settings to be customized interactively.

    Changed modeline label to the more concise "VB.NET", instead of
    "Visual Basic .NET"

    Tweaked the `vbnet-continuation-regexp' slightly nto be more
    correct.  It needed a space before the continuation char.

    Fixed `vbnet-defun-start-regexp' to handle Public Shared
    Functions.  Also renamed it to `vbnet-block-start-regexp' to
    reflect its true meaning, and renamed `vbnet-defun-end-regexp'
    similarly.  Actually, those names are irrelevant, because I put
    all the defconst regexps into an alist for simpler access.

    Added "Namespace" as a keyword, and added a regexp and fn for
    handling namespace statements.  Also modified `vbnet-calculate-indent'
    to properly handle namespaces and their children.

    Enhanced the logic for fontifying, with changes to
    `vbnet-font-lock-keywords-1', so that things like variables,
    constructor invocation, import declarations, and using statements
    get fontified.

    Removed keyword fontification of String, Trim, etc.  In VB.NET,
    these are no longer keywords.  The whole list of keywords needs a
    thorough going-over.  I think it is no longer necessary with
    VB.NET, which has many fewer keywords.

    Implemented indenting of VB.NET properties, getters and setters.

    Integration with compile.el . VBnet-mode now installs an error
    regexp for vbc.exe into `compilation-error-regexp-alist-alist',
    which allows next-error to navigate to the next compile error in
    the vb buffer.

    Integration with ya-snippet.el . Defines some built-in
    snippets for convenience.  This works only if ya-snippet is available.
    The builtin snippets will not overwrite snippets defined
    in the normal way with ya-snippet (in a compiled bundle).
    See also the var, `vbnet-want-yasnippet-fixup'.

    Integration with flymake.el .  Tweaks some defuns
    and vars to allow flymake to work with VB.NET.  This happens only
    if flymake is in use.  See also the var, `vbnet-want-flymake-fixup'.

    Removed the find-matching-* fns, they were simple and called
    from only one place, so added nothing.

    New function, `vbnet-join-continued-lines', a companion to
    `vbnet-split-line' Also, fixed the latter to work in an edge
    case.

1.5a DPC changes February 2011

     Added the feature where vbnet-mode scans the buffer for a line that
     specifies the flymake command to use.  For example, specify
     flymake-command: c:\.net3.5\vbc.exe /t:module /nologo
     in the comment at the top of the file, to tell flymake to use
     that command.  For info, see the var `vbnet-cmd-line-limit'.

1.5b DPC changes February 2011

     Fixed bug with missing fn vbnet-find-matching-try .
     I missed that one during the prior refactoring.

1.5c DPC changes April 2011

     Fixed imenu integration.  The imenu stuff was for old-syntax VB6,
     and did not handle namespaces, classes, structures, interfaces, and
     so on.  Rather than using `imenu-generic-expression', the integration
     for VB.NET uses `imenu-create-index-function'.

     Corrected the documentation and behavior of the flymake
     integration.  In practice, vbnet-mode now uses the name of the
     temporary file, as the thing to compile.

     Corrected indentation to handle .net attributes (eg,
     <DllImport>) which precede a line and use a
     line-continuation.  To do this, I modified
     `vbnet--back-to-start-of-continued-statement', which was
     previouisly named `vbnet-find-original-statement', to accept an
     optional arg telling it to NOT backup over attributes.

     Fixed indenting for functions where the decl line is "continued", even
     without a preceding attribute.
     Example:
       Public Shared Function _
              SetForegroundWindow(ByVal handle As IntPtr) As Boolean

     Added indentation support for classes marked
     NotInheritable.  Previously this keyword caused indentation to
     break.  Likewise for Friend functions.

     New interactive fn, `vbnet-close-current-block', to insert the
     "End Xxxx" statement to close the current block.

     New interactive fns, `vbnet-moveto-beginning-of-defun' and
     `vbnet-moveto-end-of-defun', that move to the beginning or end of the
     current Sub or Function.

     Implement indentation and fontification for Structures and Enums.

     Properly fontify reserved words used as variable names.  (in square
     brackets)

     Updated comments on usage.



Notes by Dave Love
BTW, here's a script for making tags tables that I (Dave Love) have
used with reasonable success.  It assumes a hacked version of etags
with support for case-folded regexps.  I think this is now in the
development version at <URL:ftp://fly.cnuce.cnr.it/pub/> and should
make it into Emacs after 20.4.

#! /bin/sh

# etags-vb: (so-called) Visual (so-called) Basic TAGS generation.
# Dave Love <d.love@dl.ac.uk>.  Public domain.
# 1997-11-21

if [ $# -lt 1 ]; then
    echo "Usage: `basename $0` [etags options] VBfile ... [etags options] " 1>&2
    exit 1
fi

if [ $1 = "--help" ] || [ $1 = "-h" ]; then
    echo "Usage: `basename $0` [etags options] VBfile ... [etags options]

"
    etags --help
fi

exec etags --lang=none -c '/\(global\|public\)[ \t]+\(\(const\|type\)[ \t]+\)*\([a-z_0-9]+\)/\4/' \
    -c '/public[ \t]+\(sub\|function\)[ \t]+\([a-z_0-9]+\)/\2/' \
  "$@"

End Notes Dave Love


Known bugs:

 - Doesn't know about ":" separated stmts.

 - Doesn't recognize single line if statements if these are broken by
   line continuation characters.  (not sure what this means; if I did
   I would try to fix it.  -DPC 2011/Feb/26)



Todo?:

 - Handle interfaces.  how did I leave this out?
   need a regexp, and to handle in the imenu helper, as well
   as in the indentation logic.

 - additional flymake work: allow the user to specify the command to
   run, through a buffer-local variable - eg, without requiring a
   comment in the source code.

 - Smart completion over object fields, methods, and properties.  This
   would require a sourcecode analysis engine, something like
   NRefactory.

 - IDE integration - not sure what this might mean.  Need suggestions
 - here.

 - Change behaviour of ESC-q to recognise words used as paragraph
   titles and prevent them being dragged into the previous
   paragraph.(?)

 - others?
