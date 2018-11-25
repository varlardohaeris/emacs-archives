This is `wikipedia-mode', a major mode for editing articles written
in the markup language used by Wikipedia, the free on-line
encyclopedia (http://www.wikipedia.org). It is intended to work
with GNU Emacs 21.x, and Xemacs 21.4.x. See below for details.

wikipedia mode can be found also at:
http://en.wikipedia.org/wiki/Wikipedia:Wikipedia-mode.el

{{{ INSTALLING WIKIPEDIA-MODE

Installing wikipedia-mode
=========================

Save wikipedia-mode.el in a convenient directory, preferably in
your `load-path'. Add the following to your `user-init-file':

  (autoload 'wikipedia-mode
    "wikipedia-mode.el"
    "Major mode for editing documents in Wikipedia markup." t)

If you did not save wikipedia-mode.el in your `load-path', you must
use the full pathname. On MS Windows, use forward slashes (/)
rather than back slashes (\) to indicate the directory, e.g.:

  (autoload 'wikipedia-mode
    "C:/Documents and Settings/USERNAME/.emacs.d/Wikipedia-mode.el"
    "Major mode for editing documents in Wikipedia markup." t)

If you want to associate filenames ending in ".wiki" with
wikipedia-mode, add the following to your init file:

  (setq auto-mode-alist
    (cons '("\\.wiki\\'" . wikipedia-mode) auto-mode-alist))

}}}

{{{ REQUIREMENTS

This  is not a real requirements but I highly recommend to use
outline-magic written by Carsten Dominik. If you don't want to use it
you have to comment out the relevant reference to outline magic.
It can be found at
http://www.astro.uva.nl/~dominik/Tools/outline-magic.el




}}}

{{{ RECOMMENDATIONS INSTALLING LONGLINES-MODE

Installing longlines-mode
=========================

Wikipedia articles don't use newline characters to break paragraphs
into lines, so each paragraph looks like a super-long line to
Emacs. To let Emacs handle "soft word wrapping", you need to
download a third-party package, longlines-mode.

Download longlines.el, saving into your `load-path':

  http://www.emacswiki.org/elisp/longlines.el

Add the following to your `user-init-file':

  (autoload 'longlines-mode "longlines.el"
    "Minor mode for editing long lines." t)


WARNING: if you insert text from one file in wikipedia-mode to
another file in wikipedia-mode I strongly recommend, to turn
longlines-mode off, before the copying!

}}}

{{{ RECOMMENDATIONS INSTALLING PABBREV-MODE

Installing longlines-mode
=========================

You may find pabbrev.el useful, which can be found at
http://www.russet.org.uk/download/emacs/pabbrev.el


}}}

{{{ Xemacs or (GNU) Emacs

Xemacs or (GNU) Emacs
=====================
Usually that is a question of taste. However almost all wikipedia
articles nowadays use UTF8 coding, so the question which of the
Macsen to use, boils down to which degree UTF8 support is
implemented (no mule Xemacs is ruled out). While Xemacs has the
better font support, the UTF8 support still is not complete and
hence at the time being it is sad for the maintainer (a long time
Xemacs user) to recommend NOT to use Xemacs, even not 21.5.x, which
has a much better implemented UTF8 coding engine. That might
however change in the foreseeable future....
WARNING: at least for me in Debian testing/unstable Emacs does not
ship all fonts necessary for a flawless editing of UTF8  files. For
example you can chose Greek input, write Greek text, but then when
you close and open the file again, the Greek symbol are not
displayed but you see empty blocks. The reason seems that emacs
chooses for the input fonts other fonts as for the display (don't
ask me). However for installing the (ugly) UTF8 compatible fonts
from ..... solved that problem.


}}}

{{{ INSTALLING EE-HELPER or MOZEX

Installing the helper programs.
=========================
Helper Programs: MozEx and EE-HELPER. There are two possibilities
in order to use Emacs as an external editor

    (1) EE-HELPER: This is perl script which will communicate with
        the  wikipedia server. However that sometimes be slow.

        PROS: if the editor supports UTF8, then ee-helper will
              pass the coding flawlessly.

        CONTRA: the problem with this script is that it directly
                communicates with the wikipedia site and does not
                warn you about simultaneous editing. Use it with
                care!!! Moreover section editing is not implemented.

    (2) MozEx: this is a Java-script which allows to communicate
        Mozilla (or Firefox) directly with Emacs.

        PROS: After finishing editing you use the wikipedia
              software to submit your changes and not the script,
              so you are warned about possible conflicting editing.

        CONTRA: the official version does not support UTF8,
                however there is now a new semi official version which
                does support UTF8.



Installing ee-helper
====================

Download   the perl script  from

  http://meta.wikimedia.org/wiki/Help:External_editors

and follow the instructions. configure the .ee-ini file.  chance in
your personal wikipedia-mode-map account setting the editing
functions: activate the `external editor' option.

Installing MozEx
================

If your web browser is Mozilla or Firefox, take a look at the MozEx
extension, which allows you to call Emacs for editing text boxes:

  http://mozex.mozdev.org/development.html

See also

  http://www.emacswiki.org/cgi-bin/wiki/FireFox

If you mostly use MozEx to edit Wikipedia articles, it might be
worthwhile to tell Emacs to enter wikipedia-mode whenever it is
called by MozEx. Just add this to your `user-init-file':

  (add-to-list 'auto-mode-alist '("mozex.\\.*" . wikipedia-mode))

    Recall: you have to click on edit (either edit article or edit
            section), then use mouse3 (or shift f10), then select
            mozex, then edit textarea: Edit-->mouse3-->mozex-->Edit
            Textarea. After editing, you have to _click_ on the
            text in the browser otherwise Mozilla will ignore your
            typing.

}}}

{{{ NEWS


NEWS
==================================
    (1) Font setting has changed.
    (2) Some makeup formats have been added: italics, bold, strong
        emphasise, links.
    (3) outline-cycle from Carsten Dominiks outline-magic has been
        added.
    (4) "Draft", "send" and "reply" (for discussion pages)
        abilities `based' on ideas of John Wigleys remember.el: see
        the functions wikipedia-draft-*
        RATIONALE: This comes handy in 2 situations
           1. You are editing articles which various authors (this I
              think is the usual case), you then want not to submit
              your edit immediately but want to copy it somewhere and
              to continue later. You can use the following functions
              for doing that:
              wikipedia-draft-buffer \C-c\C-b
              wikipedia-draft-region \C-c\C-r
              then the buffer/region will be appended to the
              wikipedia-draft-data-file (default is
              "~/Wiki/discussions/draft.wiki", which you can visit via
              wikipedia-draft-view-draft) and it will be
              surrounded by the ^L marks in order to set a page.
              moreover on top on that a section header == will be
              inserted, which consists of the Word Draft, a subject
              you are asked for and a date stamp.

              Another possibility consists in using the function
              wikipedia-draft, bound to \C-c \C-m then a new buffer
              will opened already in wikipedia mode. You edit and then
              either can send the content of the buffer to the
              wikipedia-draft-data-file in the same manner as
              described above using the function
              wikipedia-draft-buffer (bound to \C-c\C-k)

              BACK: In order to copy/send the content of temporary
              buffer or of a page in the wikipedia-draft-data-file
              back in to your wikipedia file, use the function
              wikipedia-send-draft-to-mozex bound to "\C-c\C-c". You
              will be asked to which buffer to copy your text!


           2. You want to reply  in a discussion page to a specific
              contribution, you can use either the function

              \\[wikipedia-reply-at-point-simple] bound to [(meta shift r)]
              which inserts a newline, a hline, and the signature of
              the author. Or can use
              \\[wikipedia-draft-reply] bound  [(meta r)]
              which does the same as wikipedia-reply-at-point-simple
              but in a temporary draft buffer.

              BACK: In order to copy/send the content of that buffer
              back in to your wikipedia file, use the function
              \\[wikipedia-send-draft-to-mozex] bound to "\C-c\C-c". You
              will be asked to which buffer to copy your text! If
              you want a copy to be send to your draft file, use
              the variable  wikipedia-draft-send-archive


}}}

{{{ NEW FUNCTIONS AND VARIABLES


VERSION 0.4
==================
NEW FUNCTIONS
------------------
wikipedia-insert-enumerate
wikipedia-insert-itemize
wikipedia-insert-strong-emphasis
wikipedia-insert-bold
wikipedia-insert-italics
wikipedia-insert-header
wikipedia-insert-link
wikipedia-turn-on-outline-minor-mode
wikipedia-insert-signature
wikipedia-insert-hline
wikipedia-unfill-paragraph-or-region
wikipedia-start-paragraph
wikipedia-hardlines
wikipedia-outline-magic-keys
wikipedia-enhance-indent
wikipedia-yank-prefix
wikipedia-simple-outline-promote
wikipedia-simple-outline-demote
wikipedia-next-long-line
wikipedia-unfill-paragraph
wikipedia-rename-buffer
wikipedia-draft
wikipedia-draft-buffer-desc
wikipedia-draft-append-to-file
wikipedia-draft-page
wikipedia-draft-region (&optional beg end)
wikipedia-draft-buffer
wikipedia-draft-clipboard
wikipedia-draft-mode
wikipedia-draft-view-draft
wikipedia-mark-section
wikipedia-activate-region
wikipedia-copy-page-to-register
wikipedia-insert-page-to-register
wikipedia-send-draft-to-mozex (target-buffer)
wikipedia-reply-at-point-simple
wikipedia-draft-reply
wikipedia-insert-quotation-with-signature
wikipedia-insert-quotation

NEW VARIABLES
---------------------
wikipedia-enumerate-with-terminate-paragraph
wikipedia-draft-buffer "*Wikipedia-Draft*"
wikipedia-draft-mode-map
wikipedia-draft-mode-hook
wikipedia-draft-register ?R
wikipedia-draft-filter-functions
wikipedia-draft-handler-functions '(wikipedia-draft-append-to-file)
wikipedia-draft-data-file "~/Wiki/discussions/draft.wiki"
wikipedia-draft-leader-text "== "
wikipedia-draft-page ?S
wikipedia-draft-send-archive
wikipedia-reply-with-quote


VERSION 0.5
====================================
NEW FUNCTIONS
------------------------------------
 wikipedia-insert-audio
 wikipedia-insert-bible-verse-template
 wikipedia-insert-bible-verse-template-old
 wikipedia-insert-image
 wikipedia-insert-link-www
 wikipedia-insert-user
 wikipedia-mark-signature
 wikipedia-outline-cycle
 wikipedia-reply-at-signature
 wikipedia-terminate-paragraph-and-indent
 wikipedia-yank-prefix

NEW VARIABLES (defvar, defcustom, defconst)
----------------------
wikipedia-reply-with-hline
wikipedia-user-simplify-signature
wikipedia-english-or-german
wikipedia-draft-reply-register ?M
wikipedia-mode-version

}}}

{{{ TODO

Todo
----


* Implement TeX highlighting in <math> environment
* Implement (La)TeX input syntax, following the ideas of CDlatex.el
* Make outline-cycle work correctly
* wikipedia-reply-at-point-simple should use regexp!

}}}
