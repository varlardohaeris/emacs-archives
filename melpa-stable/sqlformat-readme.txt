Provides commands and a minor mode for easily reformatting SQL
using external programs such as "sqlformat" and "pg_format".

Install the "sqlparse" (Python) package to get "sqlformat", or
"pgformatter" to get "pg_format".

Customise the `sqlformat-command' variable as desired, then call
`sqlformat' or `sqlformat-buffer' as convenient.

Enable `sqlformat-mode' in SQL buffers like this:

    (add-hook 'sql-mode-hook 'sqlformat-mode)

The `sqlformat' command will then be bound to "C-c C-f" by default.
If `sqlformat-mode-format-on-save' is enabled, this mode will apply
the configured `sqlformat-command' to the buffer every time it is
saved.
