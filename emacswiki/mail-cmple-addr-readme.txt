Try completion of mail address interactively or expand alias on the header.
You type some string on the header and do this command,
you can try completion the string as a mail address.

It perform with mail aliases, original addresses of mail aliases,
specified domain names and specified addresses.
If `mail-expand-alias' is non-nil and the string is match for alias,
it expand original mail address.
If `mail-complete-alias` is non-nil, it try completion with mail aliases.
If `mail-complete-original-address' is non-nil, it try completion with
original mail addresses of mail aliases.

You can specify complete address with `mail-address-table.'
And you can specify complete domain with `mail-domain-table'.

If `mail-noh-self-insert' is non-nil,
you can self insert key sequence for this command not on the header."
