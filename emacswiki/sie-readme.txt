This file implements an interpreter of a Scheme-like language.
But unlike real Scheme, it isn't properly tail-recursive.

Scheme functions supported
--------------------------

lambda quote if set! + - * / car cdr

Representation of types
-----------------------

Symbols, numbers, strings and pairs are directly represented as the
corresponding Emacs Lisp types.

Vectors are represented as Emacs Lisp vectors whose first element
is not `eq' to the value of the variable `sie-magic'.

Other types are represented as vectors whose first element is `eq'
to the value of the variable `sie-magic'.  Cons cells could be used
instead, but vectors are rarer so we don't have to check them as
often.

Booleans:
  #f:  `[,sie-magic sie-false]
  #t:  `[,sie-magic sie-true]
  These vectors are kept in variables `sie-false' and `sie-true' so
  SIE Booleans can be compared with `eq'.

Characters:
  `[,sie-magic sie-char ,code]

Lambda procedures:
  `[,sie-magic sie-lambda ,args+body ,environment]

Subroutines:
  `[,sie-magic sie-subr ,elisp-function]
  The elisp-function is called with the evaluated arguments.

Syntaxes:
  `[,sie-magic sie-syntax ,elisp-function]
  The elisp-function is called with two arguments: list of
  unevaluated arguments in the call, and the environment.

Environments are represented by simple alists.  Scheme code cannot
access them directly.
