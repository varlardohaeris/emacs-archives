;;; ink-mode.el --- Major mode for writing interactive fiction in Ink -*- lexical-binding: t -*-

;; Copyright (C) 2016-2020 Erik Sjöstrand, Damien Picard, and
;; ink-mode contributors (see the commit log for details).

;; Author: Erik Sjöstrand
;;         Damien Picard
;; Maintainer: Damien Picard
;; URL: http://github.com/Kungsgeten/ink-mode
;; Package-Version: 0.3.0
;; Package-Commit: f883fefb739d62e75a3f7247ea5c6ba8a0895c23
;; Version: 0.3.0
;; Keywords: languages, wp, hypermedia
;; Package-Requires: ((emacs "26.1"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `ink-mode' provides syntax highlighting and indentation for
;; the Ink scripting language, developed by Inkle Studios.

;; Other features are: divert autocompletion and links to headers and
;; labels; an `ink-play' command to playtest your story from Emacs
;; (bound to C-c C-c by default); an outline similar to org-mode's;
;; error reporting using flymake; a collection of YASnippet snippets.

;;; Code:

(require 'rx)
(require 'comint)
(require 'thingatpt)
(require 'outline)
(require 'subr-x)
(require 'easymenu)
(require 'flymake)
(require 'seq)

(defgroup ink nil
  "Major mode for writing interactive fiction in Ink."
  :group 'languages)

(defvar ink-mode-hook nil)

(defvar ink-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-c") 'ink-play)
    (define-key map (kbd "C-c C-p") 'ink-play-knot)
    (define-key map (kbd "C-c C-o") 'ink-follow-link-at-point)
    ;; Visibility cycling
    (define-key map (kbd "TAB") 'ink-cycle)
    (define-key map (kbd "<S-iso-lefttab>") 'ink-shifttab)
    (define-key map (kbd "<S-tab>")  'ink-shifttab)
    (define-key map (kbd "<backtab>") 'ink-shifttab)
    map)
  "Keymap for ink major mode.")

(defvar ink-mode-mouse-map
  (let ((map (make-sparse-keymap)))
    (define-key map [follow-link] 'mouse-face)
    (define-key map [mouse-2] #'ink-follow-link-at-point)
    map)
  "Keymap for following links with mouse.")

(easy-menu-define ink-mode-menu ink-mode-map
  "Menu for `ink-mode'."
  '("Ink"
    ["Run game from start" ink-play]
    ["Run game from knot or stitch" ink-play-knot]
    "---"
    ["Follow link at point" ink-follow-link-at-point]))

(defconst ink-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; // starts a comment
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    ;; End of line ends a comment
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?\" ".   " st)
    (modify-syntax-entry ?\( ".   " st)
    (modify-syntax-entry ?\) ".   " st)
    (modify-syntax-entry ?\[ ".   " st)
    (modify-syntax-entry ?\] ".   " st)
    (modify-syntax-entry ?0 ".   " st)
    (modify-syntax-entry ?1 ".   " st)
    (modify-syntax-entry ?2 ".   " st)
    (modify-syntax-entry ?3 ".   " st)
    (modify-syntax-entry ?4 ".   " st)
    (modify-syntax-entry ?5 ".   " st)
    (modify-syntax-entry ?6 ".   " st)
    (modify-syntax-entry ?7 ".   " st)
    (modify-syntax-entry ?8 ".   " st)
    (modify-syntax-entry ?9 ".   " st)
    st)
  "Syntax table used while in `ink-mode'.")


;;; Regular Expressions

(defconst ink-regex-header
  "^\\s-*\\(?1:=+\\)\\s-*\\(?2:\\(?:function\\)?\\)\\s-*\\(?3:[[:alnum:]_]+\\)\\s-*\\(?4:\\(?:([^)]*)\\)?\\)\\s-*\\(?5:=*\\)"
  "Regexp identifying Ink headers.
Group 1 matches the equal signs preceding the title.
Group 2 matches the function keyword.
Group 3 matches the header title.
Group 4 matches the function arguments.
Group 5 matches the optional equal signs following the header.")

(defconst ink-regex-label
  "^\\(?:\\s-*[*+\\-]\\)+\\s-*\\(?1:(\\(?2:[[:alnum:]_]+\\))\\)"
  "Regexp identifying Ink labels.
Group 1 matches a label including parentheses.
Group 2 matches a label excluding parentheses.")

(defconst ink-regex-divert
  "\\(?1:->\\|<-\\)\\(?3:\\s-*\\)\\(?2:[[:alnum:]_.]*\\)"
  "Regexp identifying Ink diverts.
Group 1 matches an left or right arrow.
Group 2 matches a link text.
Group 3 matches the spaces inbetween.")

(defconst ink-regex-include
  "^\\s-*\\(?1:INCLUDE\\)\\s-*\\(?2:.*?\\)\\s-*$"
  "Regexp identifying Ink includes.
Group 1 matches an INCLUDE keyword
Group 2 matches a link text")

(defconst ink-regex-comment
  ;; "^\\s-*\\(TODO\\|//\\|.*?/\\*\\|.*?\\*/\\)"
  "^\\s-*\\(TODO\\|//\\)"
  "Regexp identifying Ink comments.")


;;; Link following

(defun ink-follow-link-at-point ()
  "Open the current link.
Determine whether it leads to a header or to an included file by
matching regexps."
  (interactive "@")
  (let ((found-link nil))
    (cond ((thing-at-point-looking-at ink-regex-divert)
           (ink-follow-header-or-label-link)
           (setq found-link t))
          ((thing-at-point-looking-at ink-regex-include)
           (ink-follow-file-link)
           (setq found-link t))
          ((not found-link)
           (user-error "No links")))))

(defun ink-find-header (title)
  "Find a header (knot or stitch) matching TITLE in the buffer.
Return its position."
  (let (position)
    (save-excursion
      (goto-char (point-min))
      (while (and (not position)
                  (re-search-forward ink-regex-header (buffer-end 1) t))
        (when (string-equal (ink-get-knot-name) title)
          (setq position (point))))
      position)))

(defun ink-find-label (title-list &optional start end)
  "Find a label matching TITLE-LIST in the buffer.
Return its position.
TITLE-LIST consists of one to three elements, giving four possibilities:
\(label stitch knot\)
\(label stitch\)
\(label knot\)
\(label\)
START and END can specify the range in which
to search."
  ;; reverse title list to get label first
  (setq title-list (reverse title-list))
  (let (position
        (start (if start start (point-min)))
        (end   (if end end (point-max))))
    (save-excursion
      (goto-char start)
      (while (and (not position)
                  (re-search-forward ink-regex-label end t))
        ;; do different checks depending on title list length
        (cond ((and (not position)
                    (= 3 (length title-list))
                    ;; three elements: compare all three
                    (equal (ink-get-label-name) title-list))
               (setq position (point)))
              ((and (not position)
                    (= 2 (length title-list))
                    ;; two elements: compare first and (second or third) elements
                    (and
                     (equal (nth 0 (ink-get-label-name)) (nth 0 title-list))
                     (or
                      (equal (nth 1 (ink-get-label-name)) (nth 1 title-list))
                      (equal (nth 2 (ink-get-label-name)) (nth 1 title-list)))))
               (setq position (point)))
              ((and (not position)
                    (= 1 (length title-list))
                    ;; one element: compare only label
                    (equal (nth 0 (ink-get-label-name)) (nth 0 title-list)))
               (setq position (point))))))
    position))

(defun ink-follow-header-or-label-link ()
  "Go to the header or label matching the link at point."
  (let (position
        title title-list
        knot-name
        stitch-start stitch-end
        knot-start knot-end)
    (font-lock-ensure)
    (setq title (string-trim-right (match-string-no-properties 2) "\\."))
    (if (string-match-p "^\\(\\END\\|DONE\\)" title)
        (user-error "%s is not a real link" title)
      (progn
        (save-excursion
          ;; get knot and stitch names and start / end positions
          (when (ignore-errors (outline-back-to-heading t))
            (if (= (ink-outline-level) 2)
                ;; In stitch
                (progn
                  (setq stitch-start (point))
                  (save-excursion
                    (ink-end-of-subtree t)
                    (setq stitch-end (point)))
                  (ignore-errors (outline-up-heading 1))
                  (setq knot-name (ink-get-knot-name))
                  (setq knot-start (point))
                  (save-excursion
                    (ink-end-of-subtree t)
                    (setq knot-end (point))))
              ;; In knot
              (setq knot-name (ink-get-knot-name))
              (setq knot-start (point))
              (save-excursion
                (ink-end-of-subtree t)
                (setq knot-end (point))))))

        ;; Look for header
        (setq position (ink-find-header title))
        ;; Look for stitch with that name in current knot:
        (if (not position)
            (setq position (ink-find-header (concat knot-name "." title))))

        ;; Look for labels:
        (setq title-list (split-string title "\\."))
        (unless position
          (cond ((or (= 1 (length title-list))
                     (= 2 (length title-list)))
                 ;; Title has one or two element;
                 ;; look in order in stitch, knot and outside
                 (if (and (not position)
                          stitch-start)
                     (setq position
                           (ink-find-label title-list stitch-start stitch-end)))
                 (if (and (not position)
                          knot-start)
                     (setq position
                           (ink-find-label title-list knot-start knot-end)))
                 (if (not position)
                     (setq position
                           (ink-find-label title-list))))

                ;; Title has three elements;
                ;; look as knot.stitch.label in whole buffer
                ((and (not position)
                      (= 3 (length title-list)))
                 (setq position (ink-find-label title-list)))))

        (if position (progn
                       (message "Jumping to %s" title)
                       (goto-char position)
                       (ignore-errors (outline-show-subtree)))
          (user-error "Link `%s' not found. Is it in another file?" title))))))

(defun ink-follow-file-link ()
  "Find file matching the link at point."
  (let (file-name)
    (setq file-name (match-string-no-properties 2))
    (setq file-name (concat (file-name-directory
                             (buffer-file-name))
                            file-name))
    (find-file file-name)
    (message "Visiting %s" file-name)))


;;; Faces

(defface ink-shadow-face
  '((t (:inherit shadow)))
  "Face for Ink headers and glue."
  :group 'ink-faces)

(defface ink-knot-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for Ink knots: == * ==."
  :group 'ink-faces)

(defface ink-stitch-face
  '((t (:inherit 'ink-knot-face)))
  "Face for Ink stitches: = *."
  :group 'ink-faces)

(defface ink-link-face
  '((t (:inherit link)))
  "Face for Ink divert links."
  :group 'ink-faces)

(defface ink-arrow-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for Ink divert arrows."
  :group 'ink-faces)

(defface ink-tag-face
  '((t (:inherit font-lock-doc-face)))
  "Face for Ink tags: ()."
  :group 'ink-faces)

(defface ink-bracket-face
  '((t (:inherit italic)))
  "Face for Ink brackets: []."
  :group 'ink-faces)


;;; Highlighting

(defun ink-fontify-diverts (last)
  "Add text properties to next divert from point to LAST."
  (when (re-search-forward ink-regex-divert last t)
    (ink-fontify-links
     ;; Arrow part
     (list 'face 'ink-arrow-face
           'rear-nonsticky t
           'font-lock-multiline t))
    t))

(defun ink-fontify-includes (last)
  "Add text properties to next include from point to LAST."
  (when (re-search-forward ink-regex-include last t)
    (ink-fontify-links
     ;; INCLUDE part
     (list 'face 'font-lock-keyword-face
           'rear-nonsticky t
           'font-lock-multiline t))
    t))

(defun ink-fontify-links (pre-part)
  "Add text properties to link.
Use the PRE-PART list as properties to fontify the part preceding
the link, whether it be an arrow for diverts, or the INCLUDE
keyword."
  (let* ((link-start (match-beginning 2))
         (link-end (match-end 2))
         (title (string-trim-right (match-string-no-properties 2) "\\."))
         ;; Link part (without face)
         (lp (list 'keymap ink-mode-mouse-map
                   'mouse-face 'highlight
                   'font-lock-multiline t
                   'help-echo (if title title ""))))
    (when (match-end 1)
      (add-text-properties (match-beginning 1) (match-end 1) pre-part))
    (when link-start
      (add-text-properties link-start link-end lp)
      (add-face-text-property link-start link-end
                              'ink-link-face 'append))
    t))

(defvar ink-font-lock-keywords
  `(
    ;; TODO-style comments
    ("^\\s-*\\(TODO.*\\)" . font-lock-comment-face)

    ;; Knots
    (,ink-regex-header
     (1 'ink-shadow-face)
     (2 font-lock-keyword-face)
     (3 'ink-knot-face)
     (4 font-lock-variable-name-face)
     (5 'ink-shadow-face))

    ;; Diverts, threads and tunnels
    (ink-fontify-diverts)

    ;; Labels
    (,ink-regex-label 1 font-lock-variable-name-face)

    ;; Choices
    ("^\\s-*\\([*+]\\s-*\\)+" . font-lock-type-face)

    ;; Gathers
    ("^\\s-*\\(\\(?:\\s-*-\\)+\\)\\(?:[^>]\\|$\\)" 1 font-lock-type-face)

    ;; Keywords at beginning of line
    ("^\\s-*\\(VAR\\|CONST\\|LIST\\)" . font-lock-keyword-face)

    ;; Includes
    (ink-fontify-includes)

    ;; Vars, constants and lists
    ("^\\s-*\\(?:VAR\\|CONST\\|LIST\\)\\s-+\\([[:word:]_]+\\)" 1
     font-lock-variable-name-face)

    ;; Conditions
    ("{.*?\\(:\\).*?}" 1 font-lock-constant-face)

    ;; Alternatives
    ("\\(?:^\\|[^\\\\]\\)\\([{|}]+\\)" 1 font-lock-constant-face)

    ;; Code lines
    ("\\(^\\s-*~\\)" (0 font-lock-type-face)
     ("\\_<\\(?:return\\|temp\\|ref\\)\\_>" nil nil (0 font-lock-keyword-face))
     ("\\(\".*?\"\\)" nil nil (0 font-lock-string-face))
     ("\\([[:word:]_]+\\)(.*)" nil nil (1 font-lock-function-name-face))
     ("\\_<\\(?1:.*?\\)\\s-*\\(?2:=\\)\\_>" nil nil
      (1 font-lock-variable-name-face))
     ("\\_<\\(SEED_RANDOM\\|RANDOM\\|CHOICE_COUNT\\|TURNS\\|TURNS_SINCE\\|INT\\|FLOOR\\|FLOAT\\)\\_>" nil nil (0 font-lock-builtin-face)))

    ;; Tags
    ("\\(?:^\\|[^\\\\]\\)\\(#.*\\)$" 1 'ink-tag-face)

    ;; Glue
    ("\\(^\\s-*<>\\|<>\\s-*$\\)" . 'ink-shadow-face)

    ;; Brackets
    ("^\\(?:\\s-*[*+]\\).*\\(\\[.*\\]\\)" 1 'ink-bracket-face)))


;;; Indentation

(defcustom ink-indent-choices-with-spaces nil
  "If non-nil, force using spaces between choices and gathers.
You'd get something like:

-   I looked at Monsieur Fogg
    *   ... and I could contain myself no longer.
        'What is the purpose of our journey, Monsieur?'
        'A wager,' he replied.
        * *   'A wager!'[] I returned.
              He nodded.
            * * *   'But surely that is foolishness!'

Otherwise, use the setting of `indent-tabs-mode', which may give:

-   I looked at Monsieur Fogg
    *   ... and I could contain myself no longer.
        'What is the purpose of our journey, Monsieur?'
        'A wager,' he replied.
        *   *   'A wager!'[] I returned.
                He nodded.
            *   *   *   'But surely that is foolishness!'"
  :group 'ink
  :type 'boolean)

(defun ink-indent-line ()
  "Indent current line of Ink code."
  (save-excursion
    (indent-line-to (max 0 (ink-calculate-indentation)))
    (ink-indent-choices))
  (let* ((actual-indentation
          (save-excursion
            (goto-char (line-beginning-position))
            (search-forward-regexp "[^[:space:]]")
            (- (point) 1)))
         (follow-indentation-p
          ;; Check if point is within indentation.
          (>= actual-indentation
              (point))))
    (when follow-indentation-p (back-to-indentation))))

(defun ink-count-choices ()
  "Return number of choices or gathers in line."
  (interactive)
  (let ((choices 0))
    (save-excursion
      (beginning-of-line)
      (re-search-forward
       "\\(?1:\\(?:[*+-]\\s-*\\)+?\\)\\s-*\\(?:->\\)?\\s-*\\([^*+-]\\|$\\)"
       (line-end-position) t)
      (if (match-beginning 0)
          (setq choices (count-matches "\\([*+-]\\)" (line-beginning-position) (match-end 1)))))
    choices))

(defun ink-get-tab-string ()
  "Get the string to insert as tabs depending on `indent-tabs-mode'."
  (if indent-tabs-mode
      "\t"
    (make-string (max 0 (- tab-width 1)) ? )))

(defun ink-indent-choices ()
  "Indent choices and gathers: add indentations between symbols."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (and (looking-at "^\\s-*[*+\\-]")
               ;; (not (looking-at "^\\s-*-.*:")) ;; Conditions
               (not (looking-at ".*\\*/"))) ;; Comments
      (let (found-not-choice found-divert replacement-string)
        (while (and (not found-not-choice)
                    (re-search-forward
                     "\\(?1:[*+\\-]\\)\\(?2:\\s-*\\)"
                     (line-end-position) t))
          (save-match-data
            (cond ((looking-at ">")
                   (setq found-divert t)
                   (setq found-not-choice t))
                  ((looking-at "[^*+\\-]")
                   (setq found-not-choice t))))
          (unless found-divert
            (setq replacement-string
                  (cond (ink-indent-choices-with-spaces
                         (if found-not-choice
                             (ink-get-tab-string)
                           " "))
                        (indent-tabs-mode
                         "\t")
                        (t
                         (ink-get-tab-string))))
            ;; Compare string to be replaced with replacement, and do
            ;; it only if different. This avoid making changes which
            ;; disable auto-complete.
            (when (not (equal replacement-string (match-string-no-properties 2)))
              (replace-match replacement-string nil nil nil 2))))))))

(defun ink-calculate-bracket-difference ()
  "Count the difference between opening and closing brackets."
  (-
   (count-matches
    "\\({\\)"
    (line-beginning-position)
    (line-end-position))
   (count-matches
    "\\(}\\)"
    (line-beginning-position)
    (line-end-position))))

(defun ink-calculate-indentation ()
  "Find indent level at point."
  (let (indented
        (bracket-difference 0)
        (indentation-list (list))
        (indentation 0)
        (start-pos) (on-last-line)
        comment-start comment-end)

    (save-excursion
      ;; Indent comments as the first non-comment line below
      (beginning-of-line)

      ;; Don't indent empty lines
      (if (looking-at "^\\s-*$")
          (setq indented t))

      (while (and (not indented)
                  (or
                   (looking-at "^\\s-*$")
                   (looking-at "^\\s-*//")
                   (looking-at "^\\s-*TODO")))
        (forward-line 1)
        (re-search-forward "^\\s-*[^[:space:]]+.*$")
        (beginning-of-line))

      (setq start-pos (point))

      ;; Multiline comments
      (save-excursion
        (cond ((looking-at ".*\\(?1:/\\*\\)")
               ;; Comment starting at line
               (setq comment-start (match-beginning 1))
               (setq start-pos (match-end 1)))
              ((re-search-backward "/\\*" nil t)
               ;; Comment before
               (setq comment-start (point))))
        (when comment-start
          (when (re-search-forward "\\*/" nil t)
            ;; Find end of comment
            (setq comment-end (point))
            (when (and (> comment-end start-pos)
                       (= comment-start
                          (progn (re-search-backward "/\\*" nil t)
                                 (point))))
              ;; Inside comment: the end we found is not that of a
              ;; later comment. Advance to next non-empty, non-comment
              ;; line.
              (goto-char comment-end)
              (forward-line 1)
              (while (or (looking-at "^\\s-*$")
                         (looking-at "^\\s-*//")
                         (looking-at "^\\s-*TODO"))
                (forward-line 1)
                (re-search-forward "^\\s-*[^[:space:]]+.*$")
                (beginning-of-line))
              ;; (forward-line 1)
              (setq start-pos (point))))))

      ;; Go back to header or buffer start
      (or
       (ignore-errors (outline-back-to-heading t))
       (goto-char (point-min)))

      (while (and (not indented)
                  (not on-last-line))
        ;; Go forward one line until exit condition: on starting line
        (when (= (point)
                 start-pos)
          (setq on-last-line t))

        ;; Calculate the difference betweeen the numbers of opening
        ;; and closing brackets
        (when (looking-at ".*[{}]")
          (setq bracket-difference (ink-calculate-bracket-difference)))

        (cond
         ;; At header; only useful for multiline comments,
         ;; which look down
         ((looking-at ink-regex-header)
          (setq indentation-list '()))

         ;; Conditions inside brackets - ...:
         ((and (looking-at "^\\s-*-.*:\\s-*")
               (or
                (seq-contains indentation-list 'bracket)
                (seq-contains indentation-list 'bracket-cond)))
          ;; Pop until previous bracket
          (while (not (or (eq 'bracket (nth 0 indentation-list))
                          (eq 'bracket-cond (nth 0 indentation-list))))
            (pop indentation-list))
          (cond
           ;; If inside bracket with condition, keep same indentation
           ((eq 'bracket-cond (nth 0 indentation-list))
            (pop indentation-list)
            (unless on-last-line
              (push 'bracket-cond indentation-list)))
           ;; If inside simple bracket, add one indentation
           ((eq 'bracket (nth 0 indentation-list))
            (unless on-last-line
              (push 'cond indentation-list)))))

         ;; Choices * +
         ((looking-at "^\\s-*[*+]")
          ;; (not (looking-at ".*?\\*/"))) ;; comments
          (while (or (eq 'choice (nth 0 indentation-list))
                     (eq 'gather (nth 0 indentation-list)))
            (pop indentation-list))
          (setq indentation-list (nconc (make-list (ink-count-choices) 'choice)
                                        indentation-list)))

         ;; Gathers -
         ((and (looking-at "^\\s-*\\(-[^>]\\|-$\\)")
               (not (looking-at ".*?\\*/"))) ;; comments
          (while (or (eq 'choice (nth 0 indentation-list))
                     (eq 'gather (nth 0 indentation-list)))
            (pop indentation-list))
          (setq indentation-list (nconc (make-list (ink-count-choices) 'gather)
                                        indentation-list)))

         ;; Increase indent on opening bracket with condition
         ((and
           (> bracket-difference 0)
           (looking-at "^\\s-*{\\s-*.*?:"))
          (unless on-last-line
            (push 'bracket-cond indentation-list)))

         ;; Decrease indent on closing bracket
         ((and (looking-at ".*[{}]")
               (< bracket-difference 0))
          (while (and (not (or (eq 'bracket (nth 0 indentation-list))
                               (eq 'bracket-cond (nth 0 indentation-list))))
                      (> (length indentation-list) 0))
            (pop indentation-list))
          (when (> (length indentation-list) 0)
            (pop indentation-list))))

        ;; Increase indent on opening bracket
        (when (and
               (looking-at ".*{")
               (> bracket-difference 0)
               (not (looking-at "^\\s-*{.*?:")) ;; already addressed
               (not on-last-line))
          (push 'bracket indentation-list))

        (forward-line 1))

      ;; Add up indentations from list
      (goto-char start-pos)
      ;; Reverse list, because each level may depend on the
      ;; previous ones
      (setq indentation-list (reverse indentation-list))
      (let (element value)
        ;; pop each level in turn, then accumulate the indentation
        ;; depending on element type
        (while (> (length indentation-list) 0)
          (setq element (pop indentation-list))
          (cond
           ((eq element 'choice)
            (if (looking-at "^\\s-*[*+]") ;; on choice line
                (setq value tab-width)
              (if ink-indent-choices-with-spaces
                  (setq value (ink-calculate-choice-indentation
                               element indentation-list indentation))
                (setq value (* 2 tab-width)))))

           ((eq element 'gather)
            (if (looking-at "^\\s-*-") ;; on gather line
                (setq value tab-width)
              (if ink-indent-choices-with-spaces
                  (setq value (ink-calculate-choice-indentation
                               element indentation-list indentation))
                (setq value (* 2 tab-width))))
            ;; Cancel last gather, to unindent once
            (when (not (eq element
                           (nth 0 indentation-list)))
              (setq value (- value tab-width))))

           ((eq element 'bracket)
            (setq value tab-width))
           ((eq element 'bracket-cond)
            (setq value tab-width))
           ((eq element 'cond)
            (setq value tab-width)))
          (when value
            (setq indentation (+ indentation value))))))
    indentation))

(defun ink-calculate-choice-indentation (element indentation-list indentation)
  "Get the number of columns to indent choices and gathers.
This depends on previous indentation, and settings. ELEMENT is
the current element in the INDENTATION-LIST for the lign to
indent. INDENTATION is the current sum."
  (let (value)
    (if ink-indent-choices-with-spaces
        (if (eq element (nth 0 indentation-list))
            ;; all but last elements
            (setq value (+ 2 tab-width))
          ;; last element
          (if indent-tabs-mode
              ;; find the closest tab, depending on current
              ;; indentation
              (setq value
                    (- (* tab-width
                          (ceiling (/ (+ 2.0 tab-width
                                         indentation)
                                      tab-width)))
                       indentation))
            (setq value (* 2 tab-width))))
      (setq value (* 2 tab-width)))
    value))


;;; Ink-play

(defcustom ink-inklecate-path (executable-find "inklecate")
  "The path to the Inklecate executable."
  :group 'ink
  :type '(file))

(defvar-local ink-comint-do-filter nil)

(defun ink-play-knot ()
  "Play the current ink buffer from the knot or stitch at point."
  (interactive)
  (ink-play t))

(defun ink-play (&optional go-to-knot)
  "Play the current ink buffer.
If the GO-TO-KNOT optional argument is non-nil, start at the knot
or stitch at point. In that case we issue \"-> knot.stitch\" to
the process, and suppress the beginning output using the comint
output filter."
  (interactive "P")
  (let* ((file-name (buffer-file-name))
         (ink-buffer
          (if (comint-check-proc "*Ink*")
              (progn
                (comint-exec "*Ink*" "Ink" ink-inklecate-path
                             nil `("-p" ,file-name))
                "*Ink*")
            (make-comint "Ink" ink-inklecate-path nil
                         "-p" (buffer-file-name))))
         (knot-name (ink-get-knot-name)))
    (switch-to-buffer-other-window ink-buffer)
    (comint-clear-buffer)
    (if (and go-to-knot knot-name)
        (progn
          (setq ink-comint-do-filter t)
          (message (concat "Running " knot-name "..."))
          (comint-send-string (get-process "Ink")
                              (concat "-> " knot-name "\n"))
          (comint-delete-output)
          (comint-clear-buffer))
      (setq ink-comint-do-filter nil))
    (message "Running Ink...")))

(defun ink-filter-output-line (line)
  "Filter single line of text from Inklecate's output.
The filter is active only on starting play. It outputs all
errors, warnings and infos appearing in LINE, and discards the
rest."
  (let ((result ""))
    (if ink-comint-do-filter
        (cond ((string-match-p "^\\(ERROR:\\|WARNING:\\|TODO\\)" line)
               (setq result (concat line "\n")))
              ((string-match-p "\\?>" line)
               (setq result (concat (substring line 3) "\n"))
               (setq ink-comint-do-filter nil))
              ((not result)
               (setq result "")))
      (setq result (concat line "\n")))
    result))

(defun ink-comint-filter-output (output)
  "Comint output filter for `ink-play'.
This whole filter is just so that the first output of comint
doesn't print before the first important line when starting
directly at a knot... OUTPUT is the output to be filtered."
  (if ink-comint-do-filter
      (setq output (mapconcat #'ink-filter-output-line (split-string output "\n") "")))
  output)

(add-hook 'comint-preoutput-filter-functions #'ink-comint-filter-output)


;;; Error checking with flymake

(defvar-local ink--flymake-proc nil)

(defun ink-flymake (report-fn &rest _args)
  "Ink backend for Flymake.
Creates temporary files and passes their names as arguments to
`ink-inklecate-path' (which see). The output of this command is
analyzed for error and warning messages."
  (unless (executable-find ink-inklecate-path)
    (error "Cannot find a suitable checker"))
  ;; If a live process launched in an earlier check was found, that
  ;; process is killed.  When that process's sentinel eventually runs,
  ;; it will notice its obsoletion, since it have since reset
  ;; `ink-flymake-proc' to a different value
  ;;
  (when (process-live-p ink--flymake-proc)
    (kill-process ink--flymake-proc))
  ;; From ‘flymake-proc-init-create-temp-buffer-copy’.
  ;;
  (let* ((source (current-buffer))
         (temp-file   (flymake-proc-init-create-temp-buffer-copy
                       'flymake-proc-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name)))
         (json-file   (concat local-file ".json")))
    ;; Save the current buffer, the narrowing restriction, remove any
    ;; narrowing restriction.
    ;;
    (save-restriction
      (widen)
      ;; Reset the `ink--flymake-proc' process to a new process
      ;; calling the ink tool.
      ;;
      (setq ink--flymake-proc
            (make-process
             :name "ink-flymake" :noquery t :connection-type 'pipe
             ;; Make output go to a temporary buffer.
             ;;
             :buffer (generate-new-buffer " *ink-flymake*")
             :command (list ink-inklecate-path "-o" json-file local-file)
             :sentinel
             (lambda (proc _event)
               ;; Check that the process has indeed exited, as it might
               ;; be simply suspended.
               ;;
               (when (eq 'exit (process-status proc))
                 (unwind-protect
                     ;; Only proceed if `proc' is the same as
                     ;; `ink--flymake-proc', which indicates that
                     ;; `proc' is not an obsolete process.
                     ;;
                     (if (with-current-buffer source (eq proc ink--flymake-proc))
                         (with-current-buffer (process-buffer proc)
                           (goto-char (point-min))
                           ;; Parse the output buffer for diagnostic's
                           ;; messages and locations, collect them in a list
                           ;; of objects, and call `report-fn'.
                           ;;
                           (cl-loop
                            while (search-forward-regexp
                                   "^\\(.*?\\): '.*?' line \\([0-9]+\\): \\(.*\\)$"
                                   nil t)
                            for msg = (match-string 3)
                            for (beg . end) = (flymake-diag-region
                                               source
                                               (string-to-number (match-string 2)))
                            for type = (cond
                                        ((string-match "ERROR" (match-string 1)) :error)
                                        ((string-match "WARNING" (match-string 1)) :warning)
                                        ((string-match "TODO" (match-string 1)) :note)
                                        (t :warning))
                            collect (flymake-make-diagnostic source
                                                             beg
                                                             end
                                                             type
                                                             msg)
                            into diags
                            finally (funcall report-fn diags)))
                       ;; Cleanup the temporary buffer used to hold the
                       ;; check's output.
                       ;;
                       (kill-buffer (process-buffer proc)))
                   ;; Delete temporary files
                   (flymake-proc--safe-delete-file local-file)
                   (flymake-proc--safe-delete-file json-file))))))
      ;; Send the buffer contents to the process's stdin, followed by
      ;; an EOF.
      ;;
      (process-send-region ink--flymake-proc (point-min) (point-max))
      (process-send-eof ink--flymake-proc))))


;;; Outline

;; Outline functions were derived from markdown-mode.el, in turn
;; originally derived from from org.el.

(defvar-local ink-cycle-global-status 1)
(defvar-local ink-cycle-subtree-status nil)

(defalias 'ink-end-of-heading 'outline-end-of-heading)

(defun ink-end-of-subtree (&optional invisible-OK)
  "Move to the end of the current subtree.
Only visible heading lines are considered, unless INVISIBLE-OK is
non-nil.
Derived from `markdown-end-of-subtree', derived from `org-end-of-subtree'."
  (outline-back-to-heading invisible-OK)
  (let ((first t)
        (level (ink-outline-level)))
    (while (and (not (eobp))
                (or first (> (ink-outline-level) level)))
      (setq first nil)
      (outline-next-heading))
    (if (memq (preceding-char) '(?\n ?\^M))
        (progn
          ;; Go to end of line before heading
          (forward-char -1)
          (if (memq (preceding-char) '(?\n ?\^M))
              ;; leave blank line before heading
              (forward-char -1)))))
  (point))

(defun ink-get-knot-name ()
  "Return the name of the knot at point, or knot.stitch if in stitch."
  (save-excursion
    (let ((knot-name ""))
      (when (ignore-errors (outline-back-to-heading t))
        (re-search-forward ink-regex-header)
        (setq knot-name (match-string-no-properties 3))
        (if (= (ink-outline-level) 2)
            ;; Currently in stitch, go up to look at knot
            (progn
              (ignore-errors (outline-up-heading 1))
              (re-search-forward ink-regex-header)
              (setq knot-name
                    (concat (match-string-no-properties 3) "."
                            knot-name))))
        knot-name))))

(defun ink-get-label-name ()
  "Return the name of the label at point.
Can also be knot.label if in knot, or knot.stitch.label if in
stitch."
  (save-excursion
    (beginning-of-line)
    (let (
          knot-name
          (title-list (list)))
      (re-search-forward ink-regex-label (line-end-position) t)
      (setq title-list (list (match-string-no-properties 2)))
      (setq knot-name (ink-get-knot-name))
      (if (and knot-name title-list)
          (setq title-list (append title-list (reverse (split-string knot-name "\\.")))))
      title-list)))

(defun ink-shifttab ()
  "S-TAB keybinding: cycle global heading visibility by calling `ink-cycle' with argument t."
  (interactive)
  (ink-cycle t))

(defun ink-cycle (&optional arg)
  "Visibility cycling for Ink mode.
If ARG is t, perform global visibility cycling. If the point is
at a header, cycle visibility of the corresponding subtree.
Otherwise, indent the current line or insert a tab, as
appropriate, by calling `indent-for-tab-command'."
  (interactive "P")
  (cond
   ;; Global cycling
   ((eq arg t)
    (cond
     ;; Move from overview to contents
     ((and (eq last-command this-command)
           (eq ink-cycle-global-status 2))
      (outline-hide-sublevels 1)
      (message "CONTENTS")
      (setq ink-cycle-global-status 3))
     ;; Move from contents to all
     ((and (eq last-command this-command)
           (eq ink-cycle-global-status 3))
      (outline-show-all)
      (message "SHOW ALL")
      (setq ink-cycle-global-status 1))
     ;; Defaults to overview
     (t
      (outline-hide-body)
      (message "OVERVIEW")
      (setq ink-cycle-global-status 2))))

   ;; At a heading: rotate between three different views
   ((thing-at-point-looking-at ink-regex-header)
    (outline-back-to-heading)
    (let (eoh eol eos)
      ;; Determine boundaries
      (save-excursion
        (outline-back-to-heading)
        (save-excursion
          (beginning-of-line 2)
          (while (and (not (eobp)) ;; this is like `next-line'
                      (get-char-property (1- (point)) 'invisible))
            (beginning-of-line 2)) (setq eol (point)))
        (ink-end-of-heading)   (setq eoh (point))
        (ink-end-of-subtree t)
        (skip-chars-forward " \t\n")
        (beginning-of-line 1) ; in case this is an item
        (setq eos (1- (point))))
      ;; Find out what to do next and set `this-command'
      (cond
       ;; Nothing is hidden behind this heading
       ((= eos eoh)
        (message "EMPTY ENTRY")
        (setq ink-cycle-subtree-status nil))
       ;; Entire subtree is hidden in one line: open it
       ((>= eol eos)
        ;; (ink-show-entry)
        (outline-show-entry)
        (outline-show-children)
        (message "CHILDREN")
        (setq ink-cycle-subtree-status 'children))
       ;; We just showed the children, now show everything.
       ((and (eq last-command this-command)
             (eq ink-cycle-subtree-status 'children))
        (outline-show-subtree)
        (message "SUBTREE")
        (setq ink-cycle-subtree-status 'subtree))
       ;; Default action: hide the subtree.
       (t
        (outline-hide-subtree)
        (message "FOLDED")
        (setq ink-cycle-subtree-status 'folded)))))

   ;; Otherwise, indent as appropriate
   (t
    (indent-for-tab-command))))

(defun ink-outline-level ()
  "Return the depth to which a statement is nested in the outline."
  (if (> (length (match-string-no-properties 1)) 1)
      1
    2))


;;; Autocomplete

(defun ink-get-headers-and-labels ()
  "Return a list of all header and label hierarchies."
  (let ((headers-labels (list "END" "DONE")) match)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward ink-regex-header nil t)
        (when (setq match (match-string-no-properties 3))
          (push match headers-labels)
          (push (ink-get-knot-name) headers-labels)))
      (goto-char (point-min))
      (while (re-search-forward ink-regex-label nil t)
        (when (setq match (match-string-no-properties 2))
          (push match headers-labels)
          (push (concat (ink-get-knot-name) "." match) headers-labels))))
    (sort (delete-dups headers-labels) #'string<)))

(defun ink-completion-at-point ()
  "Return completion table for entity at point.
Completion is only provided for diverts."
  (when (and (thing-at-point-looking-at ink-regex-divert)
             (<= (match-beginning 3)
                 (point)))
    (when (> (match-beginning 2)
             (point))
      (goto-char (match-end 2)))
    (list (match-beginning 2) (match-end 2)
          (completion-table-dynamic
           (lambda (_) (ink-get-headers-and-labels))))))


;; Snippets -- taken from pony-mode.el

(defcustom ink-snippet-dir (expand-file-name
                            (concat (file-name-directory (or load-file-name default-directory))
                                    "./snippets"))
  "Directory in which to locate Yasnippet snippets for Ink Mode."
  :group 'ink
  :type 'string)

;;;###autoload
(defun ink-load-snippets()
  "Load snippets if yasnippet installed and ink-snippet-dir is set."
  (interactive)
  (when ink-snippet-dir
    (cond
     ((fboundp 'yas-load-directory)
      (yas-load-directory ink-snippet-dir))
     ((fboundp 'yas/load-directory)
      (yas/load-directory ink-snippet-dir)))))


;;; Mode Definition

;;;###autoload
(define-derived-mode ink-mode prog-mode "Ink"
  "Major mode for editing interactive fiction using the Ink
  scripting language."
  :syntax-table ink-mode-syntax-table

  ;; Syntax
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "//+\\s-*")
  (setq-local comment-use-syntax t)
  (setq-local comment-end "")
  (setq-local comment-auto-fill-only-comments t)
  (setq font-lock-defaults '(ink-font-lock-keywords))

  ;; Indent
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local indent-line-function #'ink-indent-line)

  ;; Complete
  (setq-local completion-cycle-threshold t)
  (add-hook 'completion-at-point-functions
            #'ink-completion-at-point nil 'local)

  ;; Outline
  (setq-local outline-regexp ink-regex-header)
  (setq-local outline-level #'ink-outline-level)
  ;; Cause use of ellipses for invisible text.
  (add-to-invisibility-spec '(outline . t))

  ;; Flymake
  (add-hook 'flymake-diagnostic-functions 'ink-flymake nil t)

  ;; Snippets
  (ink-load-snippets))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ink\\'" . ink-mode))

(provide 'ink-mode)
;;; ink-mode.el ends here
