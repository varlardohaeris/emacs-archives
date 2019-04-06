This package is some useful functions that base on `thingatpt.el'.
Those function can copy or paste special data object quickly
and don't need to move cursor.
Just binding your like keystroke to those functions.

thing-paste-sexp                paste regular expression around cursor.
thing-copy-sexp                 copy regular expression around cursor.
thing-replace-sexp              replace regular expression around cursor with content of kill-ring.

thing-paste-email               paste email string around cursor
thing-copy-email                copy email string around cursor.
thing-replace-email             replace email string around cursor with content of kill-ring.

thing-paste-filename            paste filename string around cursor.
thing-copy-filename             copy filename string around cursor.
thing-replace-filename          replace filename string around cursor with content of kill-ring.

thing-paste-url                 paste url string around cursor.
thing-copy-url                  copy url string around cursor.
thing-replace-url               replace url string around cursor with content of kill-ring.

thing-paste-word                paste word string around cursor.
thing-copy-word                 copy word string around cursor.
thing-replace-word              replace word string around cursor with content of kill-ring.

thing-paste-symbol              paste symbol string around cursor.
thing-copy-symbol               copy symbol string around cursor.
thing-replace-symbol            replace symbol string around cursor with content of kill-ring.

thing-paste-defun               paste function string around cursor.
thing-copy-defun                copy function string around cursor.
thing-replace-defun             replace function string around cursor with content of kill-ring.

thing-paste-list                paste list string around cursor.
thing-copy-list                 copy list string around cursor.
thing-replace-list              replace list string around cursor with content of kill-ring.

thing-paste-sentence            paste sentence string around cursor.
thing-copy-sentence             copy sentence string around cursor.
thing-replace-sentence          replace sentence string around cursor with content of kill-ring.

thing-paste-whitespace          paste whitespace string around cursor.
thing-copy-whitespace           copy whitespace string around cursor.
thing-replace-whitespace        replace whitespace string around cursor with content of kill-ring.

thing-paste-page                paste page string around cursor.
thing-copy-page                 copy page string around cursor.
thing-replace-page              replace page string around cursor with content of kill-ring.

thing-paste-line                paste current line.
thing-copy-line                 copy current line.
thing-replace-line              replace current line with content of kill-ring.

thing-paste-to-line-end         paste string to end of line.
thing-copy-to-line-end          copy string to end of line.
thing-replace-to-line-end       replace string to end of line with content of kill-ring.

thing-paste-to-line-beginning   paste string to beginning of line.
thing-copy-to-line-beginning    copy string to beginning of line.
thing-replace-to-line-beginning replace string to beginning of line with content of kill-ring.

thing-paste-comment             paste comment.
thing-copy-comment              copy comment.
thing-replace-comment           replace comment with content of kill-ring.

thing-paste-paragrap            paste paragraph around cursor.
thing-copy-paragrap             copy paragraph around cursor.
thing-replace-paragrap          replace paragraph around cursor with content of kill-ring.

thing-paste-parentheses         paste parentheses around cursor.
thing-copy-parentheses          copy parentheses around cursor.
thing-replace-parentheses       replace parentheses around cursor with content of kill-ring.


Installation:

Copy thing-edit.el to your load-path and add to your ~/.emacs

     (require 'thing-edit)

No more need
