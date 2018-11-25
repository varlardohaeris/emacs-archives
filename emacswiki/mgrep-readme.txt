Probably, you will search a word under same directory many
times. mgrep can search under the directory you specified
previously.

The latest version of this program can be downloaded from
http://www.bookshelf.jp/elc/mgrep.el

Usage

M-x mgrep

Run grep under your favorite directory

Variables

mgrep-list : Your favorite directory list. This valiable is used
as bellow.

(setq mgrep-list
      '(
         name   directory        mask   option
        ("dir" default-directory "*.el" dir)
        ("config" "~/mylisp/"  "*.el" nil)
        ("1.99" "d:/unix/Meadow2/1.99a6/" "*.*" sub)
        ))

name : Input your favorite name
directory : Directory you'd like to search
mask : file-mask for grep command.
option : usually option is nil. If option is t, mgrep uses find
command. If option is "dir", you can select directory like
find-file. If option is "sub", you can select sub directory to
search. "dirfind" , "subfind" are the options to use find command
of "dir" , "sub"
