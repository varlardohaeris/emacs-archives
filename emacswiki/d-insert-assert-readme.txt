Make D language's assert expression verbose automatically when
saving file.  If you are familiar with xUnit, you may write many
assertEquals expressions.

  assertEquals(EXPECTED, ACTUAL);

The D language provides assert expression, but not assertEquals.
This small elisp provides assertEquals emulation. To use this,
you must write assert expression for equality:

  assert(EXPECTED == ACTUAL);

When saving D source file, this elisp transforms assert equality
expression into verbose format. For example:

BEFORE
  assert(100 == a+8);
AFTER
  assert(100 == a+8, format("\n<%s> expected but was\n<%s>.\n", 100, a+8));

If you edit EXPECTED or ACTUAL, you NEED NOT EDIT format's arguments.
If you edit the above verbose assert statement such as:

  assert(1 == a+8, format("\n<%s> expected but was\n<%s>.\n", 100, a+8));

The elisp automatically adjusts it!

  assert(1 == a+8, format("\n<%s> expected but was\n<%s>.\n", 1, a+8));

So the elisp is DRY(Don't repeat yourself) compliant.
