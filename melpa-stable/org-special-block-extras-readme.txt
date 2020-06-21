This library provides common desirable features using the Org interface for
blocks and links:

1. Colours: Regions of text and inline text can be coloured using 19 colours;
 easily extendable; below is an example.

            #+begin_red org
            /This/
                  *text*
                         _is_
                              red!
            #+end_red

2. Multiple columns: Regions of text are exported into multiple side-by-side
columns

3. Edcomms: First-class visible editor comments

4. Details: Regions of text can be folded away in HTML

5. Badges: SVG badges have the pleasant syntax
badge:key|value|colour|url|logo; only the first two are necessary.

6. Tooltips: Full access to Lisp documentation as tooltips, or any other
documentation-backend, including user-defined entries; e.g., doc:thread-first
retrives the documentation for thread-first and attachs it as a tooltip to
the text in the HTML export and as a glossary entry in the LaTeX export

Finally, the system is extensible: Users just define a method
ORG-SPECIAL-BLOCK-EXTRAS--TYPE for a new custom block TYPE, which is then
invoked.  The handler takes three arguments: - CONTENTS: The string contents
delimited by the custom block.  - BACKEND: The current exportation backend;
e.g., 'html or 'latex.  The handler must return a string.

This file has been tangled from a literate, org-mode, file; and so contains
further examples demonstrating the special blocks it introduces.

Full documentation can be found at
https://alhassy.github.io/org-special-block-extras
