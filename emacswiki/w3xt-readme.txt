 w3XT lets generic lisp functions to be called when certain MIME types are
 encountered. Once `w3m-content-type-alist' is properly configured with a
 given file type, you will be able to execute any lisp function on a file of
 that type simply clicking on its link.

 In the example below we configure w3m to play any remote m3u playlist in emms:

 (w3m-content-type-alist
  (quote (("audio/x-mpegurl" "\\.m3u\\'"
 	  ("'emms-play-m3u-playlist" file)
 	  nil))))

 Feel free to submit EM3u as a patch to the w3m mainstream if you
 think it's worth your while.

Installation:

 Put this file on your Emacs-Lisp load path, then add one of the following
 to your ~/.emacs startup file.  You can load w3XT every time you start
 Emacs:

    (autoload 'w3m "w3xt" nil t)
