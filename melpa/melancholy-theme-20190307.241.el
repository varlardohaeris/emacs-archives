;;; melancholy-theme.el --- A dark theme for dark minds -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Sod Oscarfono

;; Author: Sod Oscarfono <sod@oscarfono.com>
;; URL: http://github.com/techquila/melancholy-theme
;; Package-Version: 20190307.241
;; Version: 2.0
;; Package-requires: emacs, powerline

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
;; ========================================
;; A dark theme for dark minds.  > Emacs 24

;;; Code:
;; ========================================

(deftheme melancholy  "A dark theme for dark minds")

;;;; Theme Faces

(custom-theme-set-faces
  'melancholy

 ;;;; window and frame settings
 ;; ========================================
 '(scroll-bar ((t (:background "#999999" :foreground "#333333"))))
 '(fringe ((t (:inherit default))))
 '(vertical-border ((t (:foreground "#666666"))))
 '(hl-line ((t (:background "#666666"))))
 '(header-line ((t (:foreground "#DEDEDE" :background "#333333"))))

 ;; line numbers
 ;; ========================================
 '(linum ((t (:foreground "#333333" :height 88 :strikethrough nil))))
  
 ;; base settings
 ;; ========================================

 '(button ((t (:underline (:color foreground-color :style line) :foreground "#F92672"))))
 '(cursor ((t (:background "#DBDBDB"))))
 
 '(bold ((t (:weight bold))))
 '(italic ((t (:slant italic))))
 '(bold-italic ((t (:weight bold :slant italic))))

 '(default ((t (:inherit nil :stipple nil :background "#161A1F" :foreground "#DBDBDB" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 99 :width normal :foundry "unknown" :family "Monospace Regular"))))
 '(fixed-pitch ((t (:family "Monospace Regular"))))
 '(variable-pitch ((t (normal :family "ubuntu" :weight normal :height 99))))

 '(link ((t (:foreground "#F92672" :underline t :weight bold))))
 '(link-visited ((t ( :foreground "#FF00FF"))))

 '(shadow ((t (:background "#666666"))))

 '(match ((t (:background "#EEE8AA"))))
 '(highlight ((t (:background "#F92672"))))
 '(lazy-highlight ((t (:foreground "#666666" :background "#96BFF0"))))
 '(secondary-selection ((t (:background "#FFB728"))))
 
 '(font-lock-builtin-face ((t (:foreground "#96BF33"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#8C8C8C"))))
 '(font-lock-comment-face ((t (:foreground "#8C8C8C"))))
 '(font-lock-constant-face ((t (:foreground "#DFAF8F"))))
 '(font-lock-doc-face ((t (:foreground "#FFB728"))))
 '(font-lock-function-name-face ((t (:foreground "#00BFFF"))))
 '(font-lock-keyword-face ((t (:foreground "#F92672" :height 121 :weight bold))))
 '(font-lock-negation-char-face ((t (:foreground "#F37DEE"))))
 '(font-lock-preprocessor-face ((t (:foreground "#F92672"))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "#A63A62"))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "#A63A62"))))
 '(font-lock-string-face ((t (:foreground "#F37DEE" :slant italic :weight extra-light))))
 '(font-lock-type-face ((t (:foreground "#96BFF0"))))
 '(font-lock-variable-name-face ((t (:foreground "#96BF33"))))
 '(font-lock-warning-face ((t (:foreground "#FF6969"))))

 '(tooltip ((t (:foreground "#161A1F" :background "#EEE8AA")) (t (:inherit (variable-pitch)))))
 '(trailing-whitespace ((t (:background "#FF6969"))))

 ;; parens / smart-parens
 ;; ========================================
 '(show-paren-match ((t (:background "#444444" :weight bold))))
 '(show-paren-mismatch ((t (:background "#FF6969" :weight bold))))
 '(sp-show-pair-match-face ((t (:background "#444444" :weight bold))))
 '(sp-show-pair-mismatch-face ((t (:background "#FF6969" :weight bold))))
  

 ;; info/errors
 ;; ========================================
 '(success ((t (:foreground "#96BF33"))))
 '(warning ((t (:foreground "#FFB728"))))
 '(error ((t (:foreground "#FF6347" :weight bold))))
 '(next-error ((t (:inherit (region)))))

 ;; info-header-node
 ;; info-header-xref
 ;; info-index-match
 ;; info-menu-header
 ;; info-menu-star
 ;; info-node
 ;; info-title-1
 ;; info-title-2
 ;; info-title-3
 ;; info-title-4
 ;; info-xref
 ;; info-xref-visited
 

 ;; calendar
 ;; ========================================
 ;; calendar-month-header
 '(calendar-today ((t (:foreground "#96BF33" :weight bold))))
 '(calendar-weekday-header ((t (:foreground "#FFB728"))))
 '(calendar-weekend-header ((t (:foreground "#666666"))))
 '(calendar-holiday-marker ((t (:foreground "#666666"))))
 

 ;; company
 ;; ========================================
 ;; company-echo
 ;; company-echo-common
 ;; company-preview
 ;; company-preview-common
 ;; company-preview-search
 ;; company-scrollbar-bg
 ;; company-scrollbar-fg
 ;; company-template-field
 ;; company-tooltip
 ;; company-tooltip-annotation
 ;; company-tooltip-annotation-selection
 ;; company-tooltip-common
 ;; company-tooltip-common-selection
 ;; company-tooltip-mouse
 ;; company-tooltip-search
 ;; company-tooltip-search-selection
 ;; company-tooltip-selection

 
 ;; diff
 ;; ========================================
 ;; diff-added
 ;; diff-changed
 ;; diff-context
 ;; diff-file-header
 ;; diff-function
 ;; diff-header
 ;; diff-hunk-header
 ;; diff-index
 ;; diff-indicator-added
 ;; diff-indicator-changed
 ;; diff-indicator-removed
 ;; diff-nonexistent
 ;; diff-refine-added
 ;; diff-refine-changed
 ;; diff-refine-removed
 ;; diff-removed

 ;; dired
 ;; ========================================
 ;; dired-directory
 ;; dired-flagged
'(dired-header ((t (:foreground "#00B7FF" :background "#999999"))))
 ;; dired-ignored
 ;; dired-mark
 ;; dired-marked
 ;; dired-perm-write
 ;; dired-symlink
 ;; dired-warning

 ;; eldoc-highlight-function-argument

;; epa
 ;; ========================================
 ;; epa-field-body
 ;; epa-field-name
 ;; epa-mark
 ;; epa-string
 ;; epa-validity-disabled
 ;; epa-validity-high
 ;; epa-validity-low
 ;; epa-validity-medium

 ;; gnus
 ;; ========================================
 ;; gnus-group-mail-1
 ;; gnus-group-mail-1-empty
 ;; gnus-group-mail-2
 ;; gnus-group-mail-2-empty
 ;; gnus-group-mail-3
 ;; gnus-group-mail-3-empty
 ;; gnus-group-mail-low
 ;; gnus-group-mail-low-empty
 ;; gnus-group-news-1
 ;; gnus-group-news-1-empty
 ;; gnus-group-news-2
 ;; gnus-group-news-2-empty
 ;; gnus-group-news-3
 ;; gnus-group-news-3-empty
 ;; gnus-group-news-4
 ;; gnus-group-news-4-empty
 ;; gnus-group-news-5
 ;; gnus-group-news-5-empty
 ;; gnus-group-news-6
 ;; gnus-group-news-6-empty
 ;; gnus-group-news-low
 ;; gnus-group-news-low-empty
 ;; gnus-splash
 ;; gnus-summary-cancelled
 ;; gnus-summary-high-ancient
 ;; gnus-summary-high-read
 ;; gnus-summary-high-ticked
 ;; gnus-summary-high-undownloaded
 ;; gnus-summary-high-unread
 ;; gnus-summary-low-ancient
 ;; gnus-summary-low-read
 ;; gnus-summary-low-ticked
 ;; gnus-summary-low-undownloaded
 ;; gnus-summary-low-unread
 ;; gnus-summary-normal-ancient
 ;; gnus-summary-normal-read
 ;; gnus-summary-normal-ticked
 ;; gnus-summary-normal-undownloaded
 ;; gnus-summary-normal-unread
 ;; gnus-summary-selected


 ;; helm
 ;; ========================================
 ;; helm-M-x-key
 ;; helm-action
 ;; helm-bookmark-addressbook
 ;; helm-bookmark-directory
 ;; helm-bookmark-file
 ;; helm-bookmark-gnus
 ;; helm-bookmark-info
 ;; helm-bookmark-man
 ;; helm-bookmark-w3m
 '(helm-buffer-directory ((t (:foreground "#333333" :background "#DEDEDE"))))
 ;; helm-buffer-file
 ;; helm-buffer-not-saved
 ;; helm-buffer-process
 ;; helm-buffer-saved-out
 ;; helm-buffer-size
 ;; helm-candidate-number
 ;; helm-etags-file
 ;; helm-ff-directory
 ;; helm-ff-dirs
 ;; helm-ff-dotted-directory
 ;; helm-ff-dotted-symlink-directory
 ;; helm-ff-executable
 ;; helm-ff-file
 ;; helm-ff-invalid-symlink
 ;; helm-ff-prefix
 ;; helm-ff-symlink
 ;; helm-grep-cmd-line
 ;; helm-grep-file
 ;; helm-grep-finish
 ;; helm-grep-lineno
 ;; helm-grep-match
 '(helm-header ((t (:background "#F92672" :foreground "#FFFFFF"))))
 '(helm-source-header ((t :background "#888888" :foreground "#161A1F" :family "Open-sans" :height 110 :weight bold)))
 ;; helm-header-line-left-margin
 ;; helm-helper
 ;; helm-history-deleted
 ;; helm-history-remote
 ;; helm-lisp-completion-info
 ;; helm-lisp-show-completion
 ;; helm-locate-finish
 ;; helm-match
 ;; helm-match-item
 ;; helm-moccur-buffer
 ;; helm-prefarg
 ;; helm-resume-need-update
 '(helm-selection ((t (:background "#00B7FF" :foreground "#161A1F"))))
 ;;'(helm-selection-line ((t (:background "#F92672" :foreground: "#FFFFFF"))))
 ;;'(helm-separator)
 ;; helm-visible-mark

 ;; help-argument-name
 

 '(isearch ((t (:background "#96BF33" :foreground "#444444"))))
 '(isearch-fail ((t (:background "#00B7FF"))))
 
 ;; minibuffer
 ;; ========================================
 '(minibuffer-prompt ((t (:foreground "#00B7FF" :weight bold))))

 ;; modeline
 ;; ========================================
 '(mode-line ((t (:background "#333333" :foreground "#00B7FF" ))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:weight extra-bold))))
 '(mode-line-highlight ((t (:box (:line-width 2 :color "#666666" :style released-button))) (t (:inherit (highlight)))))
 '(mode-line-inactive ((t (:background "#222222" :foreground "#666666" ))))
 

 ;; org-mode
 ;; ========================================
 ;; org-agenda-calendar-event
 ;; org-agenda-calendar-sexp
 ;; org-agenda-clocking
 ;; org-agenda-column-dateline
 ;; org-agenda-current-time
 ;; org-agenda-date
 '(org-agenda-date-today ((t (:foreground "#96BF33" :weight bold))))
'(org-agenda-date-weekend ((t (:foreground "#666666"))))
 ;; org-agenda-diary
 ;; org-agenda-dimmed-todo-face
'(org-agenda-done ((t (:foreground "#666666" :strike-through t))))
 ;; org-agenda-filter-category
 ;; org-agenda-filter-regexp
 ;; org-agenda-filter-tags
 ;; org-agenda-restriction-lock
 ;; org-agenda-structure
 ;; org-archived
 '(org-block ((t (:foreground "#00BFFF" :box nil))))
 '(org-block-begin-line ((t (:background "#444444" :foreground "#00BFFF"))))
 '(org-block-end-line ((t (:background "#444444" :foreground "#00BFFF"))))
 ;;'org-checkbox
 ;; org-checkbox-statistics-done
 ;; org-checkbox-statistics-todo
 ;; org-clock-overlay
 ;;'(org-code ((t (:family "Monospace Regular" ))))
 ;; org-column
 ;; org-column-title
 ;; org-date
 ;; org-date-selected
 ;; org-default
 '(org-document-info ((t (:foreground "#00BFFF" :height 1.25 ))))
 ;; org-document-info-keyword
 '(org-document-title ((t (:foreground "#00BFFF" :height 1.75 :weight extra-bold ))))
 '(org-done ((t (:foreground "#96BF33" :strike-through t))))
 ;; org-drawer
 ;; org-ellipsis
 ;; org-footnote
 ;; org-formula
 '(org-headline-done ((t (:foreground "#666666" :strike-through t))))
 ;; org-hide
 ;; org-latex-and-related
 '(org-level-1 ((t  :height 1.802 :weight bold)))
 '(org-level-2 ((t  :foreground "#888888" :height 1.602 )))
 '(org-level-3 ((t  :foreground "#888888" :height 1.424)))
 '(org-level-4 ((t  :foreground "#888888" :height 1.266)))
 '(org-level-5 ((t  :foreground "#888888" :height 1.125)))
 '(org-level-6 ((t  )))
 '(org-level-7 ((t  )))
 '(org-level-8 ((t  )))
 '(org-link ((t (:foreground "#F92672" :underline  ))))
 ;; org-list-dt
 ;; org-macro
 ;; org-meta-line
 ;; org-mode-line-clock
 ;; org-mode-line-clock-overrun
 ;; org-priority
 ;; org-property-value
 ;; org-quote
 ;; org-scheduled
 ;; org-scheduled-previously
 ;; org-scheduled-today
 ;; org-sexp-date
 ;; org-special-keyword
 '(org-table ((t :family "Monospace Regular")))
 ;; org-tag
 ;; org-tag-group
 ;; org-target
 ;; org-time-grid
 ;; org-todo
 ;; org-upcoming-deadline
 ;; org-verbatim
 ;; org-verse
 ;; org-warning

 ;; outline
 ;; ========================================
 ;; outline-1
 ;; outline-2
 ;; outline-3
 ;; outline-4
 ;; outline-5
 ;; outline-6
 ;; outline-7
 ;; outline-8

 ;; package
 ;; ========================================
 ;; package-description
 ;; package-help-section-name
 ;; package-name
 ;; package-status-avail-obso
 ;; package-status-available
 ;; package-status-built-in
 ;; package-status-dependency
 ;; package-status-disabled
 ;; package-status-external
 ;; package-status-held
 ;; package-status-incompat
 ;; package-status-installed
 ;; package-status-new
 ;; package-status-unsigned

 '(query-replace ((t (:inherit isearch))))
 '(region ((t (:background "#555555"))))
 ;; success
 ;; tool-bar
 
;; tty-menu-disabled-face
 ;; tty-menu-enabled-face
 ;; tty-menu-selected-face
 
 ;; undo-tree-visualizer-active-branch-face
 ;; undo-tree-visualizer-current-face
 ;; undo-tree-visualizer-default-face
 ;; undo-tree-visualizer-register-face
 ;; undo-tree-visualizer-unmodified-face

 )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'melancholy)
;;; melancholy-theme.el ends here
