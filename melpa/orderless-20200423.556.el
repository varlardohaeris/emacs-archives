;;; orderless.el --- Completion style for matching regexps in any order  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Omar Antolín Camarena

;; Author: Omar Antolín Camarena <omar@matem.unam.mx>
;; Keywords: extensions
;; Package-Version: 20200423.556
;; Version: 0.3
;; Homepage: https://github.com/oantolin/orderless
;; Package-Requires: ((emacs "24.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides an `orderless' completion style that divides
;; the pattern into components (space-separated by default), and
;; matches candidates that match all of the components in any order.

;; Completion styles are used as entries in the variables
;; `completion-styles' and `completion-category-overrides', see their
;; documentation.

;; To use this completion style you can use the following minimal
;; configuration:

;; (setq completion-styles '(orderless))

;; You can customize the `orderless-regexp-separator' to decide how
;; the input pattern is split into component regexps.  The default
;; splits on spaces.  You might want to add hyphens and slashes, for
;; example, to ease completion of symbols and file paths,
;; respectively.

;; Each component can match in any one of several matching styles:
;; literally, as a regexp, as an initialism, in the flex style, or as
;; word prefixes.  It is easy to add new styles: they are functions
;; from strings to strings that map a component to a regexp to match
;; against.  The variable `orderless-component-matching-styles' lists
;; the matching styles to be used for components, by default it allows
;; regexp and initialism matching.

;;; Code:

(require 'cl-lib)

(defgroup orderless nil
  "Completion method that matches space-separated regexps in any order."
  :group 'completion)

(defface orderless-match-face-0
  '((default :weight bold)
    (((class color) (min-colors 88) (background dark)) :foreground "#72a4ff")
    (((class color) (min-colors 88) (background light)) :foreground "#223fbf")
    (t :foreground "blue"))
  "Face for matches of components numbered 0 mod 4."
  :group 'orderless)

(defface orderless-match-face-1
  '((default :weight bold)
    (((class color) (min-colors 88) (background dark)) :foreground "#ed92f8")
    (((class color) (min-colors 88) (background light)) :foreground "#8f0075")
    (t :foreground "magenta"))
  "Face for matches of components numbered 1 mod 4."
  :group 'orderless)

(defface orderless-match-face-2
  '((default :weight bold)
    (((class color) (min-colors 88) (background dark)) :foreground "#90d800")
    (((class color) (min-colors 88) (background light)) :foreground "#145a00")
    (t :foreground "green"))
  "Face for matches of components numbered 2 mod 4."
  :group 'orderless)

(defface orderless-match-face-3
  '((default :weight bold)
    (((class color) (min-colors 88) (background dark)) :foreground "#f0ce43")
    (((class color) (min-colors 88) (background light)) :foreground "#804000")
    (t :foreground "yellow"))
  "Face for matches of components numbered 3 mod 4."
  :group 'orderless)

(defcustom orderless-regexp-separator " +"
  "Regexp to match component separators for orderless completion.
This is passed to `split-string' to divide the pattern into
component regexps."
  :type '(choice (const :tag "Spaces" " +")
                 (const :tag "Spaces, hyphen or slash" " +\\|[-/]")
                 (regexp :tag "Custom regexp"))
  :group 'orderless)

(defcustom orderless-match-faces
  [orderless-match-face-0
   orderless-match-face-1
   orderless-match-face-2
   orderless-match-face-3]
  "Vector of faces used (cyclically) for component matches."
  :type '(vector 'face)
  :group 'orderless)

(defcustom orderless-component-matching-styles
  '(orderless-regexp orderless-initialism)
  "List of allowed component matching styles.
If this variable is nil, regexp matching is assumed.

A matching style is simply a function from strings to strings
that takes a component to a regexp to match against.  If the
resulting regexp has no capturing groups, the entire match is
highlighted, otherwise just the captured groups are."
  :type '(set
          (const :tag "Regexp" orderless-regexp)
          (const :tag "Literal" orderless-literal)
          (const :tag "Initialism" orderless-initialism)
          (const :tag "Flex" orderless-flex)
          (const :tag "Prefixes" orderless-prefixes)
          (function :tag "Custom matching style"))
  :group 'orderless)

(defalias 'orderless-regexp #'identity
  "Match a component as a regexp.
This is simply the identity function.")

(defalias 'orderless-literal #'regexp-quote
  "Match a component as a literal string.
This is simply `regexp-quote'.")

(defun orderless--anything-between (rxs)
  "Return a regexp to match the rx-regexps RXS with .* in between."
  (rx-to-string
   `(seq ,@(cl-loop for (sexp . more) on rxs
                    collect `(group ,sexp)
                    when more collect `(zero-or-more nonl)))))

(defun orderless-flex (component)
  "Match a component in flex style.
This means the characters in COMPONENT must occur in the
candidate in that order, but not necessarily consecutively."
  (orderless--anything-between
   (cl-loop for char across component collect char)))

(defun orderless-initialism (component)
  "Match a component as an initialism.
This means the characters in COMPONENT must occur in the
candidate, in that order, at the beginning of words."
  (orderless--anything-between
   (cl-loop for char across component collect `(seq word-start ,char))))

(defun orderless-prefixes (component)
  "Match a component as multiple word prefixes.
The COMPONENT is split at word endings, and each piece must match
at a word boundary in the candidate.  This is similar to the
`partial-completion' completion style."
  (orderless--anything-between
   (cl-loop for prefix in (split-string component "\\>" t)
            collect `(seq word-boundary ,prefix))))

(defun orderless--highlight-matches (regexps string)
    "Highlight a match of each of the REGEXPS in STRING.
Warning: only call this if you know all REGEXPs match STRING!"
    (setq string (copy-sequence string))
    (cl-loop with n = (length orderless-match-faces)
             for regexp in regexps and i from 0 do
             (string-match regexp string)
             (cl-loop
              for (x y) on (or (cddr (match-data)) (match-data)) by #'cddr
              when x do
              (font-lock-prepend-text-property
               x y
               'face (aref orderless-match-faces (mod i n))
               string)))
    string)

(defun orderless--component-regexp (component)
  "Build regexp to match COMPONENT.
Consults `orderless-component-matching-styles' to decide what to
match."
  (rx-to-string
   `(or ,@(cl-loop for style in orderless-component-matching-styles
                   collect `(regexp ,(funcall style component))))))

(defun orderless-all-completions (string table pred _point)
  "Split STRING into components and find entries TABLE matching all.
The predicate PRED is used to constrain the entries in TABLE.
This function is part of the `orderless' completion style."
  (condition-case nil
      (save-match-data
        (let* ((limit (car (completion-boundaries string table pred "")))
               (prefix (substring string 0 limit))
               (components (split-string (substring string limit)
                                         orderless-regexp-separator
                                         t))
               (completion-regexp-list ; used by all-completions!!!
                (if orderless-component-matching-styles
                    (mapcar #'orderless--component-regexp components)
                  components))
               (completions (all-completions prefix table pred)))
          (when completions
            (nconc
             (cl-loop for candidate in completions
                      collect (orderless--highlight-matches
                               completion-regexp-list
                               candidate))
             limit))))
    (invalid-regexp nil)))

(defun orderless-try-completion (string table pred point &optional _metadata)
  "Complete STRING to unique matching entry in TABLE.
This uses `orderless-all-completions' to find matches for STRING
in TABLE among entries satisfying PRED.  If there is only one
match, it completes to that match.  If there are no matches, it
returns nil.  In any other case it \"completes\" STRING to
itself, without moving POINT.
This function is part of the `orderless' completion style."
  (let* ((limit (car (completion-boundaries string table pred "")))
         (prefix (substring string 0 limit))
         (all (orderless-all-completions string table pred point)))
    (cond
     ((null all) nil)
     ((atom (cdr all))
      (let ((full (concat prefix (car all))))
        (cons full (length full))))
     (t (cons string point)))))

(cl-pushnew '(orderless
              orderless-try-completion orderless-all-completions
              "Completion of multiple regexps, in any order.")
            completion-styles-alist
            :test #'equal)

(defvar orderless-old-regexp-separator nil
  "Stores the old value of `orderless-regexp-separator'.")

(defun orderless--restore-regexp-separator ()
  "Restore old value of `orderless-regexp-separator'."
  (when orderless-old-regexp-separator
    (setq orderless-regexp-separator orderless-old-regexp-separator
          orderless-old-regexp-separator nil))
  (remove-hook 'minibuffer-exit-hook #'orderless--restore-regexp-separator))

(defun orderless-temporarily-change-separator (separator)
  "Use SEPARATOR to split the input for the current completion session."
  (interactive
   (list (let ((enable-recursive-minibuffers t))
           (read-string "Orderless regexp separator: "))))
  (unless orderless-old-regexp-separator
    (setq orderless-old-regexp-separator orderless-regexp-separator))
  (setq orderless-regexp-separator separator)
  (add-to-list 'minibuffer-exit-hook #'orderless--restore-regexp-separator))

(provide 'orderless)
;;; orderless.el ends here
