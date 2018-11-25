Even though I have to use MS Outlook at work I like writing my
e-mails in a normal *mail* buffer in Emacs. I usually copy and
paste the text from there into the mail. One day I got tired of
that and hacked together this little package.

It uses MAPI to send a message by fetching information from a
normal *mail* buffer to build some vbscript code that is executed
by wscript.exe.


Prerequisites:


* MAPI must be working on your machine

* It needs wscript.exe to be able to execute the vbscript doing the
  actual work.
