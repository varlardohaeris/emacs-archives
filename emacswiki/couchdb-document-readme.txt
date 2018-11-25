Usage:

(require 'couchdb-document)
(find-file "/couchdb:/test_db1/doc_XXXX")

(require 'couchdb-document-text)
(find-file "/couchdb:/test_db1/secret.org.gpg")

*NOTE*

After load tramp.el, it's necessary to call
`couchdb-document-register-file-name-handler' if couchdb-document.el is
already loaded, or else emacs will try to use tramp to load couchdb
document. A typical configure file will looks like below:

(require 'tramp)
(when (featurep 'couchdb-document)
 (couchdb-document-register-file-name-handler))


ChangeLog:

0.3.6: Fix `couchdb-document-name-extract', should handle port correctly.

0.3.5: add function to register file name handler. If emacs try to open couchdb
document with tramp, then re-call `couchdb-document-register-file-name-handler'.

0.3.4: add `couchdb-document-post', `couchdb-document-get' default nocache,
simplify `couchdb-document-put'.

0.3.3: `couchdb-document-get' will not remove metadata when not save to
cache; `couchdb-document-put' will take extra parameter.

0.3.2: fix debug related bug.

0.3.1: fix for ido-find-file.

0.3: encode and decode method now can be select by set 'encode and 'decode
with `couchdb-document-open-hook'. A simple text format is provided with
file couchdb-document-text.el.

0.2: implement document delete, directory create and delete

0.1: implement document open and save

TODO:
1. document name with '/', maybe replace with '%2F'.
2. revision(append '@' as revision in filename) support
3. using couchdb 'change' feature implement verify-visited-file-modtime
4. support dired.
