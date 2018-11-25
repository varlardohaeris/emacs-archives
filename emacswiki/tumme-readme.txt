BACKGROUND
==========

 I needed a program to browse, organize and tag my pictures.  I got
tired of the old gallery program I used as it did not allow
multi-file operations easily.  Also, it put things out of my
control.  Image viewing programs I tested did not allow multi-file
operations or did not do what I wanted it to.

 So, I got the idea to use the wonderful functionality of Emacs and
`dired' to do it.  It would allow me to do almost anything I wanted,
which is basically just to browse all my pictures in an easy way,
letting me manipulate and tag them in various ways.  `dired' already
provide all the file handling and navigation facilities; I only
needed to add some functions to display the images.

 I briefly tried out thumbs.el, and although it seemed more
powerful than this package, it did not work the way I wanted to.  It
was too slow to created thumbnails of all files in a directory (I
currently keep all my 2000+ images in the same directory) and
browsing the thumbnail buffer was slow too.  tumme.el will not
create thumbnails until they are needed and the browsing is done
quickly and easily in dired.  I copied a great deal of ideas and
code from there though... :)

 About the name: tumme means thumb in Swedish and it is used for
working with thumbnails, so... :) If you want to know how to
pronounce it, go to the page on EmacsWiki and download the .ogg
file from there.

 `tumme' stores the thumbnail files in `tumme-dir' using the file
name format ORIGNAME.thumb.ORIGEXT.  For example
~/.tumme/myimage01.thumb.jpg.  The "database" is for now just a
plain text file with the following format:

file-name-non-directory;comment:comment-text;tag1;tag2;tag3;...;tagN


PREREQUISITES
=============

* The ImageMagick package.  Currently, `convert' and `mogrify' are
used.  Find it here: http://www.imagemagick.org.

* For non-lossy rotation of JPEG images, the JpegTRAN program is
needed.

* For `tumme-get-exif-data' and `tumme-write-exif-data' to work,
the command line tool `exiftool' is needed.  It can be found here:
http://www.sno.phy.queensu.ca/~phil/exiftool/.  These two functions
are, among other things, used for writing comments to image files
using `tumme-thumbnail-set-image-description' and to create
"unique" file names using `tumme-get-exif-file-name' (used by
`tumme-copy-with-exif-file-name').


USAGE
=====

Put tumme.el in your load-path and then:

  (require 'tumme)

If you plan to use tumme much, setting up key bindings for it in
dired is a good idea:

  (tumme-setup-dired-keybindings)

Next, do M-x tumme-dired RET.  This will ask you for a directory
where image files are stored, setup a useful window configuration
and enable the two special modes that tumme provides.  NOTE: If you
do not want tumme to split your windows, call it with a prefix
argument.

Start viewing thumbnails by doing C-S-n and C-S-p to go up and down
in the dired buffer while at the same time displaying a thumbnail
image.  The thumbnail images will be created on the fly, and
cached.  This means that the first time you browse your images, it
will be a bit slow because the thumbnails are created.  If you want
to avoid this, you can pre-create the thumbnail images by marking
all images in dired (% m \.jpg$ RET) and then do M-x
tumme-create-thumbs.

Next, try `tumme-display-thumbs' (C-t d).  If no file is marked, a
thumbnail for the file at point will show up in
`tumme-thumbnail-buffer'.  If one or more files are marked,
thumbnails for those files will be displayed.

Pressing TAB will switch to the window containing the
`tumme-thumbnail-buffer' buffer.  In there you can move between
thumbnail images and display a semi-sized version in an Emacs
buffer (RET), or the original image in an external viewer
(C-RET).  By pressing SPC or DEL you will navigate back and fort
while at the same time displaying each image in Emacs.  You can also
navigate using arrow keys.  Comment a file by pressing "c".  Press
TAB to get back to dired.

While in dired mode, you can tag and comment files, you can tell
`tumme' to mark files with a certain tag (using a regexp) etc.

The easiest way to see the available commands is to use the Tumme
menus added in tumme-thumbnail-mode and dired-mode.


LIMITATIONS
===========

* Only tested on GNU Emacs version 21.3 and 22 (CVS) under Mandrake
Linux.  I have got reports that `insert-image' does not work on
XEmacs.  I will probably never try to fix that, sorry.  This
project was mostly aimed at fixing my own itch, and I am not
interested in installing and using XEmacs.

* In order to work well, `tumme' require that all your images have
unique names.  The reason is the way thumbnail file names are
generated.  I will probably not fix this problem as my images all
have unique names.

* Supports all image formats that Emacs and convert supports, but
the thumbnails are hard-coded to JPEG format.

* WARNING: The "database" format used might be changed so keep a
backup of `tumme-db-file' when testing new versions.
