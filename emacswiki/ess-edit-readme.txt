Compatibility: Emacs23 XEmacs21

{{{ usage

load this file, then open some R-code, put
the point inside the a call to an R function, e.g.

 c(1,2,3,4)

then do

 M-x ess-edit-indent-call-sophisticatedly

the result is this:

 c(1,
 2,
 3,
 4)

 to get back to the original format just
 call the same command again:

 M-x ess-edit-indent-call-sophisticatedly

 other features:

 M-x ess-edit-insert-vector RET letters[1:10] RET

 M-x ess-edit-insert-vector RET names(mydata) RET

 M-x ess-edit-insert-call RET agrep RET
}}}
{{{Issues

may not work perfectly when there are comments in a function call


}}}
code
{{{ variables
