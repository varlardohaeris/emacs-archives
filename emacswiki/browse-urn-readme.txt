This package provides functionality for browsing URNs - Uniform Resource
Names as defined in <urn:ietf:rfc:2141>.

The EMACS 21.4.2 packages browse-url.el and thingatpt.el served as a basis
for this mode, i.e. code was taken from these packages and modified.

Example installation:

1. Put this package into a directory in your EMACS load path and optionally
  compile it.

2. Load the package in your "~/.emacs" and set up key bindings:

    (load "browse-urn")
    (define-key global-map [(control q)] 'browse-urn-at-point)
    (define-key global-map [(control shift q)] 'browse-urn)

3. Customize the package (customization group: browse-urn).
