;;; Commentary:

;; This spot of code to lets M-x ffap find a file attached as a Gnus message
;; meta-language (MML) part, as per `C-c C-a' (mml-attach-part) when
;; composing a message.
;;
;;     <#part type="text/plain" filename="/foo/bar.txt" disposition=attachment>
;;     <#/part>
;;
;; Of course ffap can already follow it when point is right on the filename,
;; but ffap-mml.el makes it work when point is somewhere else in this form,
;; like as the start of the line.

