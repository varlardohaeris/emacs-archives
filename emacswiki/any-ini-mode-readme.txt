When a file is visted in <any>.ini mode the _valid_ section and parameter names
are highlighted as keywords, the comments are highlighted as comments and everything
else is displayed in your normal font.

The list of valid section and parameter names is built dynamically, based on a
canonical file ('source of truth') that contains all valid options.

In this way, you can easily spot a mis-typed name when you're editting your files,
rather than having to wait for your application to misbehave.

It's also useful if, like me, you have users who can't spell 'log_file_path' or
colleagues who insist on creating parameters called 'defaulttofirstbackupdirectory'.

You may also define, among other things, a valid comment character and a
valid assignment charcter.

Collectively, the definition of a canonical file and a comment character etc
define a 'style' of .ini file.

You may set up a default style for all <any>.ini mode buffers, or, more usefully,
you may set up several styles that will be automatically applied, based on the name
of the file being visited.

any-ini-mode now includes support for `imenu'.

`imenu-generic-expression' is set up for each buffer to find the section names
and, by default, a menu of section names is automatically added to the menubar.

If you setup `speedbar' correctly, this means that you can also navigate the
sections with `speedbar'.

See the `any-ini-imenu' customization group and the documentation for `imenu'
and `speedbar' for more details.

Finally, you may want to try running the `any-ini-toggle-errorcheck-mode' command.
See the docstring for this command for an explantion.

Customization is via the `any-ini' group in the `local' customization section.


To load any-ini-mode on startup, copy this file to your load-path and add this to
your .emacs file -

   (require 'any-ini-mode)

You may also want to specify that <any>.ini mode should apply by default
to all .ini and .conf files, for example. Here's how -

   (add-to-list 'auto-mode-alist '(".*\\.ini$" . any-ini-mode))
   (add-to-list 'auto-mode-alist '(".*\\.conf$" . any-ini-mode))
