;;; Commentary:

;; This is some more manuals and a couple more mode setups for
;; `info-lookup-symbol'.
;;
;; Functions adding manuals are `interactive' so you can for example
;; `M-x info-lookmore-c-gcc' to see it just for the current session.
;;
;; More manuals can be added with `info-lookmore-add-doc' calls.  Usually
;; you have to find the node name of the index, then check whether entries
;; can be used directly or need mangling.  The easiest is when there's a
;; separate "Functions" or "Functions and Variables" index.  See docstring
;; of `info-lookup-alist' for the details.

