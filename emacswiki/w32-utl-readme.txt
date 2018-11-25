This package contains a couple of utility functions implemented
partly in elisp and partly in VBScript:

- `w32-utl-special-folder'
    Get the path to a "special folder" (Desktop, StartMenu, Recent, etc.)
- `w32-utl-lnk-get-target-and-args'
    Get values from Windows shortcuts (.lnk files).

I needed these functions primarily for `w32-exec-predef' in
w32-exec-predef.el but as they could be useful in other situations
I decided to put them in a separate file.

The vbscript files needed will be created by this script,
minimizing the dependancies on another files at installation time.

Prerequisites:

* It needs cscript.exe to be able to execute the vbscript scripts.
