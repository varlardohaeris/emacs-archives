Simple interface for paste2.org

This extension to offer a quickly paste on `http://paste2.org'.

* Send paste:
     Switch `erc' (or `rcirc') channel buffer, then M-x paste2-buffer-create,
     will popup a buffer named `*paste2-send*'.

     Then write everything you want paste to buffer `*paste2-send*',
     type "C-c C-c" when you complete.

     Then will prompt for paste title, type title name,
     then will upload your paste to `paste2.org',
     and will show paste title and paste link in `erc' (or `rcirc') channel buffer.

     You can use command `paste2-send-paste-with-command-output' send special
     command output to the server.

* Get paste:
     If someone paste something in `paste2.org'.
     You can use command `paste2-get-paste' to get paste,
     then will prompt paste number id.
     Example someone paste thing at `http://paste2.org/p/123456'
     or `http://paste2.org/get/123456',
     you can input `123456', `http://paste2.org/p/123456' or
     `http://paste2.org/get/123456' to
     get paste through `paste2-get-paste'.

* Tips:
     It's not necessary to use `paste2-buffer-create' with `erc' (or `rcirc') channel
     buffer, you can use it with any buffer, but it will prompt for irc
     channel name if option `paste2-blank-channel' is `nil'.

     Default, if current buffer is mark, then `paste2-buffer-create'
     will insert mark region to buffer `paste2-send-buffer'.

     You can switch any buffer after use `paste2-buffer-create', then
     use command `paste2-buffer-append' will append current buffer or
     mark region to buffer `paste2-send-buffer',
     If you use `C-u' before `paste2-buffer-append', will switch to
     buffer `paste2-send-buffer' after insert.

     You can move cursor to paste link, example `http://paste2/p/123456'
     or `http://paste2.org/get/123456', then use command `paste2-get-paste',
     `paste2-get-paste' will pick up paste id around point `123456',
     you just need type RET, everything is complete.

     You can type `C-u' before command `paste2-get-paste', the it will
     load `emacs-lisp-mode' syntax highlight for paste buffer.

     By default, all send paste function will check send string, if string
     is null, will ignore and stop send.


Installation:

Put paste2.el to your load-path.
The load-path is usually ~/elisp/.
It's set in your ~/.emacs like this:
(add-to-list 'load-path (expand-file-name "~/elisp"))

And the following to your ~/.emacs startup file.

(require 'paste2)

And this have below command for use:

     `paste2-send-paste'                     send paste current buffer content quickly.
     `paste2-send-paste-with-command-output' send paste with command output.
     `paste2-get-paste'                      get paste with special paste-id.
     `paste2-buffer-create'                  create a buffer and then send content of buffer.
     `paste2-buffer-append'                  append content to buffer `paste2-buffer'.

Enjoy!
