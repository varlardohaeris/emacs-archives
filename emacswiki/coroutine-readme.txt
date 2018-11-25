This file allows the definition of coroutines in Emacs Lisp.  A
coroutine is a function that can return ("yield") a value and still
retain its execution state so that at the next call it can pick up
where it left off.

Coroutines are very difficult to implement without the aid of
first-class continuations or a similar control feature, and this
imposes certain limits: for example, `yield' is only valid at
top-level in the coroutine, or inside a specially tweaked macro.

This file requires `tagbody.el' which defines the Common Lisp
`tagbody' form for Emacs Lisp.
