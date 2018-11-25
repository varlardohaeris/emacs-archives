Reformat parameter lists easily from single line to multiple
lines and back again. E.g. between this:

    API::Link->new({rel => 'meta:version', href => $uri->as_string, type => 'application/xml'}),

and this:

    API::Link->new({
        rel => 'meta:version',
        href => $uri->as_string,
        type => 'application/xml',
    }),

The parameters are specified by the enclosing element, with either
() or {} braces.

The multiline format can also be aligned properly within the
enclosing braced element, to end up like this:

    API::Link->new({
        rel  => 'meta:version',
        href => $uri->as_string,
        type => 'application/xml',
    }),



The user interface for this package consists of the following commands:

    jpl-reformat-mark-enclosing-block
    jpl-reformat-align-enclosing-block
    jpl-reformat-parameter-list-to-single-line
    jpl-reformat-parameter-list-to-multi-line
    jpl-reformat-parameter-list-toggle-multiple-single
