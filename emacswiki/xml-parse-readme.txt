XML is yet another way of expressing recursive, attributed data
structures -- something which Lisp has had the capacity to do for
decades.

The approach taken by xml-parse.el is to read XML data into Lisp
structures, and allow those same Lisp structures to be written out
as XML.  It should facilitate the manipulation and use of XML by
Elisp programs.

NOTE: This is not a validating parser, and makes no attempt to read
DTDs.  See psgml.el if you need that kind of power.

Also, tags beginning with <? or <! are not parsed, but merely
included in the resulting data structure as separate string
entries.  These may be tested for using the function
`xml-tag-special-p'.  If present, they are treated just like normal
text, and will be inserted along with everything else.  If they
occur *before* the opening tag of an XML tree, they will not appear
in the parsed data, since such "pre-tags" are not the child of any
tag.

Here is the format of the Lisp data structure used:

  (TAG CHILD...)

Where TAG is either a string (naming the tag) or a list.  The list
form is used to identify attributes, and has the format:

  (TAG-NAME (ATTR-NAME . ATTR-VALUE)...)

After the TAG, there can be zero or more child structures, which
are either literal strings, or the same "TAG CHILD..." structure as
the parent.  See `insert-xml' for an EBNF grammar of this layout.

EXAMPLE: Given the following DocBook XML data:

  <book id="compiler">
    <bookinfo>
      <bookbiblio>
        <title>My own book!</title>
        <edition>First</edition>
        <authorgroup>
          <author>
            <firstname>John</firstname>
            <surname>Wiegley</surname>
          </author>
        </authorgroup>
      </bookbiblio>
    </bookinfo>
    <chapter>
      <title>A very small chapter</title>
      <para>Wonder where the content is...</para>
    </chapter>
  </book>

It would be parsed into this Lisp structure:

  '(("book" ("id" . "compiler"))
    ("bookinfo"
     ("bookbiblio"
      ("title" "My own book!")
      ("edition" "FIrst")
      ("authorgroup"
       ("author"
        ("firstname" "John")
        ("surname" "Wiegley")))))
    ("chapter"
     ("title" "A very small chapter")
     ("para" "Wonder where the content is...")))

Now it can easily be modified and interpreted using ordinary Lisp
code, without the ordeal of manipulating textual XML.  When you're
done modifying it, you can write it back out (complete with proper
indentation and newlines) using:

  (insert-xml <DATA> t)

See the documentation for `read-xml' and `insert-xml' for more
information.

There are also a set of helper functions for accessing parts of a
parsed tag:

  xml-tag-name       get the name of a tag
  xml-tag-attrlist   returns a tag's attribute alist
  xml-tag-attr       lookup a specific tag attribute
  xml-tag-children   returns a tag's child list
  xml-tag-child      lookup a specific child tag by name

Also, the attribute list and child lists can be searched using
`assoc', since they roughly have the same format as an alist.

###autoload
