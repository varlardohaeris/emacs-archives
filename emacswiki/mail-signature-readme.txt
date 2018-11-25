This is a reworking of the function mail-signature in sendmail.el
(part of the Emacs distribution) to insert a context sensitive signature.
Using regular expressions, appropriate signatures can be inserted
for different audiences.
Repeated calls removes the current signature from the message and cycles
through all applicable signatures.
Use with something like this in .emacs:
(eval-after-load "sendmail"
  (progn
    (load "mail-signature")
    (setq mail-signature-alist
          (append '(("To" "friend" "-friendly")) mail-signature-alist))))
And create a file called ~/.signature-friendly that has a
signature appropriate for the user `friend' to receive.
If using message-mode (included with Emacs 19.34/GNUS 5.3 or later)
(setq message-signature 'mail-signature)
