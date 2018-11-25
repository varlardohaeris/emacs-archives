pylit.el
========

This package implements the command ``pylit-toggle`` to switch
between alternate views (code or documentation) of the same
PyLit literate source.

When invoked on a buffer, ``pylit-toggle`` will determine (based on
the buffer file name extension) whether it's a code or documentation
buffer, then it will invoke the ``pylit`` shell command to translate
the buffer contents to the other form, and display the new buffer.


Invocation of PyLit_
--------------------

.. _PyLit: http://pylit.berlios.de/

*Note:* ``pylit.el`` assumes that the PyLit_ tool is available as
shell command ``pylit`` on the current execution path.  To alter
this, customize the ``pylit-command`` variable.

As of 2007-02-09, the PyLit_ distribution does not include a script
to invoke PyLit's functionality from the command line.  On any
UNIX-like system, this can be easily worked around by creating a
file ``pylit`` somewhere in your executable search path (the
``PATH``) with the following contents:

| #!/bin/sh
| exec env PYTHONPATH=/path/to/pylit/repository/src \
|   python /path/to/pylit/repository/src.pylit.py "$@"


Issues with specific language modes
===================================

Python
------

By default, ``python-mode`` will take comments into account when
computing indentation; so, any PyLit literate comment will reset
the indentation.  If the Emacs customization variable
``py-honor-comment-indentation`` is set to ``nil``, then
``python-mode`` will ignore any comments for code indentation
purposes.


TO-DO
=====

- SHOULD support different doc files extensions, not just ".txt"
  (at least, ".rst" ".rest" ".text")
- Provide a way to specify the direction of conversion:
  code2txt or txt2code.
  - add a prefix argument to ``pylit-toggle``?
  - or provide explicit conversion functions ``pylit-code2txt``
    and ``pylit-txt2code``?
- Provide a function to strip documentation comments (``pylit -s -c``)
  - either provide function ``pylit-extract-code``,
  - or make this behavior selectable by a prefix argument
    to ``pylit-txt2code`` (see above)
- add unit tests using regress.el
