Provides procedures to construct an HTML document easily.  For
example, you can construct an HTML table by the following code:

(html-table :border "1"
 (html-tr (html-th "Item No") (html-th "Quantity"))
 (html-tr (html-td "1") (html-td "120"))
 (html-tr (html-td "2") (html-td "30"))
 (html-tr (html-td "3") (html-td "215")))

There are mainly two functions to access the constructed
tree. `html-lite-write-tree' writes the tree indented to the
current ouput stream and `html-lite-browse-tree' use
`browse-url-browser-function' to browse the tree.

You can construct complete html tree by using:

(append
 (html-doctype)
 (html-html
  (html-head
   (html-title "Title"))
  (html-body "Content")))

But the simpler way would be jsut to use `with-html-lite-header' like

(with-html-lite-header "Title"
  (html-p "foo") (html-p "bar"))

The code is ported from html-lite.scm in the Gauche
distribution. Some things are missings but will be added asap.

I wrote this just for the fun of it and hope someone will find it
useful.
