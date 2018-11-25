Reworking of etags-select to work with exuberant-ctags and eldoc.
ectags-tag-directory will generate a tag file in the current directory
with the specified language using exuberant-ctags extended tag format
which contains useful information about the current tag such as
the signature, it's parent class, which class it is a member of. Eldoc
displays this information. At present it does not do a very good job
of finding the best candidate tag in an OO language where there may
be multiple tags with the same name. It tries, however. The tag file
needs to be in a specific format, hence the ectags-tag-directory-command.
ectags-visit-tags-table is used to load in a tag table.

Open a buffer with file/lines of exact-match tags shown.  Select one by
going to a line and pressing return.  pop-tag-mark still works with this
code.

An up to date version of this code lives on repo.or.cz
at git://repo.or.cz/ectags.git
