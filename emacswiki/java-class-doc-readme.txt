 This is a simple package to quickly open the Java API
 documentation for a specific class.  After it is installed, just do
 M-x java-class-doc RET.

 It is quite ugly (but it works) and works by scanning the
 allclasses-noframe.html file for all Java classes and the path to
 their documentation page.  Then it build a cache from this, stored
 as an alist.  This cache can be saved to list to avoid the time
 consuming scan each time you start Emacs.

Installation

 - This is the most important step and what needs manual work.  Get
   the `allclasses-noframe.html' file, either from a local
   installation of the Java API documentation or from the internet
   (http://java.sun.com/j2se/1.4.2/docs/api/allclasses-noframe.html)

 - Evaluate this file (M-x eval-buffer RET)

 - Execute `java-class-doc-generate-cache' (M-x
   java-class-doc-generate-cache RET)

   This will generate a cache of class names / paths for the
   documentation.  Save this to a file using
   `java-class-doc-save-cache-to-file' (M-x
   java-class-doc-save-cache-to-file RET)

 - Put java-class-doc.el somewhere in your load-path.

 - Put the following in your .emacs:

   (autoload 'java-class-doc "java-class-doc" "Open documentation for class in Java API docs" t)

 - Try out M-x java-class-doc RET CLASSNAME RET (completion is available)

 - Enjoy!

Todo

 - Maybe add http-get support, or use wget/curl, to get the
 allclasses-noframe.html source directly from internet.

 - Improve speed and stability in
 `java-class-doc-generate-cache'.  It is not very robust if the
 format of the allclasses-noframe.html file changes.

 Any volunteers? :)

Bugs

 - No known bugs.
