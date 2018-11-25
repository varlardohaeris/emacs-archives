With this package, you can view the RFC articles.  To use this
execute function `rfc-index' or
`rfc-goto-number'. The variable `rfc-article-alist' specifies the
directories or URLs which has rfc-index.txt and rfc????.txt's under them,
or a zip file which contains them.
rfc- is used as prefix for commands that is for this package.
rfc-index- is used for commands that is for `rfc-index-mode'.
rfc-article- is used for commands that is for `rfc-article-mode'.

Example

(setq rfc-url-save-directory "~/rfc")
(setq rfc-index-url "http://www.ietf.org/iesg/1rfc_index.txt")
(setq rfc-archive-alist (list (concat rfc-url-save-directory "/rfc.zip")
                              rfc-url-save-directory
                              "http://www.ietf.org/rfc/"))
(setq rfc-insert-content-url-hook '(rfc-url-save))
