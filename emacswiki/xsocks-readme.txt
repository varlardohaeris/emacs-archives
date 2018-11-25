 xsocks extends the socks behavior with a new feature `socks-doproxy', which
 is the dual of `socks-noproxy'. Notice how `socks-doproxy' gets considered
 only in case `socks-doproxy' is nil (i.e. this last one does have the
 precedence over the first).

Installation:

 Put this file on your Emacs-Lisp load path.
