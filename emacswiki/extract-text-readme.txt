Commentary:

Bitcoin donations gratefully accepted: 1FgnGFwRES9MGzoieRDWLmLkjqMT3AsjQF

This library provides functions for programmatically extracting text from buffers.
The `extract-matching-strings' and `extract-matching-rectangles' allow you to perform
simple extractions, but for more complex tasks there is the `extract-text' macro.
This macro allows complex text extraction specifications using a kind of dsl for text extraction.
You can use included wrapper functions for picking out individual bits of text, or define your own
and save them in `extract-text-wrappers'. You can specify how many times to repeat each individual extraction,
how to handle errors or missing values, restrict the buffer for finding certain extractions, how to structure
&/or transform the output, etc. See the docstring for `extract-text' for more details.
