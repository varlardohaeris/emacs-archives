`member-function.el' defines an emacs utility to expand the
member functions declared in a class in a C++ header file into
the corresponding implementation file, provided that they aren't
already there.  The main entry point is the interactive command
`expand-member-functions'.

The code consists of four major parts:

1. A simple lexical analyser for emacs buffers containing C++
sources. The entry point, `mf--tokenize', returns a list of
token-type symbols paired with the string matched for that token.
Note that the whole buffer is lexed, even though, for the
application implemented here, some (possibly large) savings could
be realized by using forward-sexp to skip over the bodies of
member function definitions.  (This is a possible optimization.)
(This optimization has been implemented)

2. Code to analyse the results of a parse.  Used primarily to
identify member function declarations that appear in class
declarations, as well as member function definitions that appear
in the corresponding .C file.  The resulting lists are compared
to find undefined member function declarations.

3. Code to format, and insert, the found, undefined, declarations
as member function definitions into C++ source code.  An initial
block of commentary is also formatted for insertion.

4. Code to interface with emacs, including code to automatically
check out a .C file using `vc-toggle-read-only'.

Usage:

Byte compile this file.

Add the following two lines to .emacs: (customize to suit.)

(autoload 'expand-member-functions "member-functions" "Expand C++ member function declarations" t)
(add-hook 'c++-mode-hook (lambda () (local-set-key "\C-cm" #'expand-member-functions)))

BUGS and Limitations:

Ctor initializers are transferred into the .C file.  The
individual initializer expressions are treated as though they
were the names of member functions, i.e. they are prepended
with class name/scope resolution operator pairs.

There are still some issues with templates, but they are, AFAIK,
limited to advanced uses.  For example, default template
arguments aren't handled properly.  These may occasionally be
problematic, but until I see them appear in actual code, I'm
going to defer working on them.

Templates are not formatted as nicely as they could be.

There is no provision for overloaded operator(), or operator[].

Several warnings are emitted during byte code compilation.  Most
are a consequence of the use of lexical-let from cl.el.  Those
that are not are entirely my fault.

Namespaces and nested classes are not supported.

Inline functions defined outside of a class declaration in the
header file, or included into it, are not supported.

(fixed) When a member function doesn't specify a name for a parameter,
fails to recognise that the member function definition with the
parameter name specified has the same signature.

(fixed) Doesn't properly handle a throw declaration attached to a
member function declaration.

History:

Thu Sep 28 10:06:09 2000 Released under GPL.

Fri Sep 29 09:23:10 2000 Tested on emacs 20.4.1.  Added a hack
after discovering that expand-member-functions didn't work there.

Tue Jan 23 12:55:12 2001 Fixed typo on line 839.  Thanks to Barney Dalton
Barnaby.Dalton@radioscape.com

Wed Jan 24 13:40:18 2001 Added variables for header file and
source file extensions at the suggestion of Barney Dalton.
