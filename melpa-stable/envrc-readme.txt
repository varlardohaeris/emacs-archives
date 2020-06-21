Use direnv (https://direnv.net/) to set environment variables on a
per-buffer basis.  This means that when you work across multiple
projects which have `.envrc` files, all processes launched from the
buffers "in" those projects will be executed with the environment
variables specified in those files.  This allows different versions
of linters and other tools to be installed in each project if
desired.

Enable `envrc-global-mode' late in your startup files.  For
interaction with this functionality, see `envrc-mode-map', and the
commands `envrc-reload', `envrc-allow' and `envrc-deny'.
