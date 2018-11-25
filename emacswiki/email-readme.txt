 This package permits easy archiving of outgoing mails, sending cc's to
a specified set of poeple automatically and add a reference id to the
subject of the mail, all based on the mail headers only.  A set of three
associative-lists is used to determine the behaviour.  Its a complement of
vm-auto-folder-alist (used to archive incoming messages).

Installation:
Setup the following in your .emacs :
 (require 'email)
 (setq send-mail-function 'email-send)

Setup any other variable that you need like email-archive-file-name.  You
must not use mail-auto-archive-filename as it adds an FCC field automatically;; on invoking the mail-mode and this prevents me from adding an FCC field.  Set;; the value of that variable to email-auto-archive-filename instead.  Also, if
call some other function before calling sendmail-send-it, set the variable
email-send-mail-function.  The key "\C-c\C-e" in mail-mode-map is used to
provide the feature of expanding all the headers within the mail buffer
itself.

Configure your alists. For eg. :
(setq email-fcc-alist
   '(("Subject"
  ("Test mail"."/dev/null"))
 ("To"
  ("^navali"."/~/Mail/navali/outbox")
  ("staff"."~/Mail/staff/outbox")
  ("^bitftp"."~/Mail/ftp/requests")
  ("^ftpmail"."~/Mail/ftp/requests")
  ("smurty"."~/Mail/smurty/outbox")
  ("\\.ncst\\."."/~/Mail/sysadmin/ToNcst"))
 ("Resent-To"
  ("plg"."/dev/null"))
 ))

(setq email-cc-alist
   '(("To"
 ("psi.com\\b"."alan, srini")
 ("^phatak@"."srini"))))

(setq email-id-alist   ;; The second element of the alist
   '(("To"           ;; must be non-nil always. Setting this
 ("^acrouch".t)                  ;; to nil results in wrong behaviour.
 ("psi.com\\b".t)
 ("^srini".t)
 ("^geta".t)
 ("^phatak@".t))))

See the documentation of variables for further customisation.

BUGS(Features i.e. :-))

A default archive file (specfied via the mail-auto-archive-file-name) can
be specified using using another variable called email-archive-file-name.
This modification is needed because setting the former variable adds FCC
field automatically and this prevents me from adding the archive filename.
