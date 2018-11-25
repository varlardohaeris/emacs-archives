Load the library and try opening a file which has an associated
application in `extview-application-associations' or in mailcap.

`extview-application-associations' can be used to override mailcap
handlers:

  (require 'extview)
  (push '("\\.pdf$" . "acroread %s") extview-application-associations)
  (push '("\\.py$" . nil) extview-application-associations)
  (push '("\\.html$" . ask) extview-application-associations)
