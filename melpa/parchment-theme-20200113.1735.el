;;; parchment-theme.el --- Light theme inspired by Acme and Leuven -*- lexical-binding: t -*-

;; Author: Alex Griffin <a@ajgrf.com>
;; URL: https://github.com/ajgrf/parchment
;; Package-Version: 20200113.1735
;; Version: 0.3.0
;; Package-Requires: ((emacs "24"))

;; Copyright © 2019 Alex Griffin <a@ajgrf.com>
;;
;;
;; Permission to use, copy, modify, and/or distribute this software for
;; any purpose with or without fee is hereby granted, provided that the
;; above copyright notice and this permission notice appear in all
;; copies.
;;
;; THE SOFTWARE IS PROVIDED "AS IS" AND ISC DISCLAIMS ALL WARRANTIES WITH
;; REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL ISC BE LIABLE FOR ANY
;; SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT
;; OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

;;; Commentary:

;; Parchment is a light theme inspired by the Leuven theme and the
;; Acme text editor. It's meant to be more subdued and easier on the
;; eyes than black-on-white, and less "busy" than most colorschemes.

;;; Code:

(deftheme parchment
  "A light colorscheme inspired by Acme and Leuven.")

(defvar parchment-want-modify-tty-colors nil
  "Whether to redefine the terminal colors that Emacs knows about.
Set to non-nil if you're using a matching parchment terminal theme.")

(defvar parchment-add-mode-hooks nil
  "Whether to add mode hooks to modify faces per major mode.
Allows better fine-tuning of styles, but may be too intrusive if you
switch themes often.")

(defmacro parchment-style-theme (&rest styles)
  "Apply a list of face STYLES associated with theme THEME.
Wraps `custom-theme-set-faces' with a compact syntax.

Each STYLE should be one of the following forms:

  (FACE FOREGROUND BACKGROUND [ATTRIBUTES])
  (FACE SPEC)

SPEC is passed directly to `custom-theme-set-faces'.  If FOREGROUND or
BACKGROUND are nil then they will be skipped."
  (declare (indent 1))
  (let (forms)
    (while styles
      (pcase (pop styles)
        (`(,face ,fg ,bg . ,attrs)
         (push `(,'\` (,face ((t ,@(when fg `(:foreground (,'\, ,fg)))
                                 ,@(when bg `(:background (,'\, ,bg)))
                                 ,@attrs))))
               forms))
        (`(,face ((,class . ,attrs) . ,rest))
         (push `(,'\` (,face ((,class ,@attrs) ,@rest)))
               forms))))
    `(custom-theme-set-faces
      'parchment
      ,@(nreverse forms))))

(declare-function parchment-modify-tty-colors "parchment-theme.el")

(let ((black   "#000000") (pale-gray    "#eaeaea")
      (red     "#880000") (pale-red     "#ffeaea")
      (green   "#005500") (pale-green   "#eaffea")
      (brown   "#663311") (yellow       "#eeee9e")
      (blue    "#004488") (pale-blue    "#cceeff")
      (magenta "#770077") (pale-magenta "#ffeaff")
      (cyan    "#007777") (pale-cyan    "#eaffff")
      (beige   "#eeeecc") (pale-yellow  "#ffffea"))

  (defun parchment-modify-tty-colors ()
    (tty-color-define "black"          0 (tty-color-standard-values black))
    (tty-color-define "red"            1 (tty-color-standard-values red))
    (tty-color-define "green"          2 (tty-color-standard-values green))
    (tty-color-define "yellow"         3 (tty-color-standard-values brown))
    (tty-color-define "blue"           4 (tty-color-standard-values blue))
    (tty-color-define "magenta"        5 (tty-color-standard-values magenta))
    (tty-color-define "cyan"           6 (tty-color-standard-values cyan))
    (tty-color-define "white"          7 (tty-color-standard-values beige))
    (tty-color-define "brightblack"    8 (tty-color-standard-values pale-gray))
    (tty-color-define "brightred"      9 (tty-color-standard-values pale-red))
    (tty-color-define "brightgreen"   10 (tty-color-standard-values pale-green))
    (tty-color-define "brightyellow"  11 (tty-color-standard-values yellow))
    (tty-color-define "brightblue"    12 (tty-color-standard-values pale-blue))
    (tty-color-define "brightmagenta" 13 (tty-color-standard-values pale-magenta))
    (tty-color-define "brightcyan"    14 (tty-color-standard-values pale-cyan))
    (tty-color-define "brightwhite"   15 (tty-color-standard-values pale-yellow)))

  ;; Style HTML tags
  (defun parchment-modify-sgml-tags ()
    (when (member 'parchment custom-enabled-themes)
      (face-remap-add-relative 'font-lock-function-name-face
                               `(:foreground ,magenta))))

  (when parchment-want-modify-tty-colors
    (add-hook 'tty-setup-hook #'parchment-modify-tty-colors))

  (when parchment-add-mode-hooks
    (add-hook 'sgml-mode-hook 'parchment-modify-sgml-tags))

  (parchment-style-theme
    ;; FACE                     FOREGROUND   BACKGROUND   ATTRIBUTES
    (default                    black        pale-yellow)
    (bold                       nil          nil          :weight bold)
    (italic                     nil          nil          :slant italic)
    (bold-italic                nil          nil          :weight bold :slant italic)
    (underline                  nil          nil          :underline t)
    (shadow ((((type tty)) :foreground ,cyan)
              (t :foreground "#808075")))
    (link                       blue         nil          :underline t)
    (link-visited               magenta      nil          :underline t)
    (error                      red          nil)
    (warning                    red          nil)
    (success                    green        nil)

    ;; standard interface elements
    (cursor                     nil          black)
    (compilation-error          ((((type tty)) :foreground ,red)
                                 (t :foreground ,pale-red :distant-foreground ,red)))
    (compilation-info           ((((type tty)) :foreground ,green)
                                 (t :foreground ,pale-green :distant-foreground ,green)))
    (compilation-mode-line-exit nil          nil          :inherit compilation-info :weight bold)
    (compilation-mode-line-fail nil          nil          :inherit compilation-error :weight bold)
    (compilation-mode-line-run  nil          nil          :inherit compilation-error)
    (compilation-warning        nil          nil          :inherit compilation-error)
    (fringe                     black        pale-gray)
    (flyspell-incorrect ((((type tty)) :foreground ,red :underline t)
                          (t :underline (:color "red" :style wave))))
    (flyspell-duplicate ((((type tty)) :foreground ,green :underline t)
                          (t :underline (:color ,green :style wave))))
    (header-line                nil          pale-cyan    :box t)
    (highlight                  nil          pale-blue)
    (hl-line                    nil          beige)
    (isearch                    nil          pale-blue    :weight bold)
    (isearch-fail               red          pale-red)
    (lazy-highlight             nil          nil          :weight bold)
    (line-number                black        pale-gray)
    (linum                      nil          nil          :inherit line-number)
    (match                      nil          yellow)
    (menu                       nil          pale-cyan    :weight bold)
    (tty-menu-disabled-face     cyan         pale-cyan)
    (tty-menu-enabled-face      nil          pale-cyan)
    (tty-menu-selected-face     pale-yellow  cyan         :weight bold)
    (minibuffer-prompt          nil          pale-blue    :weight bold)
    (mode-line                  pale-blue    blue         :box ,black)
    (mode-line-inactive         nil          pale-gray    :box ,black)
    (mode-line-highlight        black        yellow)
    (mode-line-buffer-id        nil          nil          :inherit mode-line-emphasis)
    (mode-line-emphasis ((((type tty)) :weight bold)
                         (t :foreground ,pale-yellow :distant-foreground ,black :weight bold)))
    (sml/charging ((((type tty)))
                   (t :foreground ,pale-green :distant-foreground ,green)))
    (sml/discharging ((((type tty)) :foreground ,red)
                      (t :foreground ,pale-red :distant-foreground ,red)))
    (sml/filename               nil          nil          :inherit mode-line-buffer-id)
    (sml/modes                  nil          nil          :inherit mode-line-emphasis :weight normal)
    (sml/modified               nil          nil          :inherit sml/discharging :weight bold)
    (sml/outside-modified       pale-yellow  red)
    (region                     nil          yellow)
    (secondary-selection        nil          yellow)
    (show-paren-match           nil          pale-blue)
    (show-paren-mismatch        pale-yellow  red)
    (trailing-whitespace        red          pale-red)
    (whitespace-big-indent      pale-red     red)
    (whitespace-empty           nil          nil          :inherit whitespace-indentation)
    (whitespace-hspace          nil          beige        :inherit whitespace-space)
    (whitespace-indentation     red          yellow)
    (whitespace-line            nil          nil          :inherit trailing-whitespace)
    (whitespace-newline         nil          nil          :inherit shadow)
    (whitespace-space-after-tab nil          nil          :inherit whitespace-indentation)
    (whitespace-space-before-tab pale-red    red)
    (whitespace-space           nil          nil          :inherit shadow)
    (whitespace-tab             nil          nil          :inherit shadow)
    (whitespace-trailing        nil          nil          :inherit trailing-whitespace)

    ;; generic syntax highlighting
    (font-lock-warning-face     red          nil)
    (font-lock-function-name-face nil        nil)
    (font-lock-variable-name-face nil        nil)
    (font-lock-keyword-face     nil          nil)
    (font-lock-comment-face     brown        nil          :slant italic)
    (font-lock-type-face        blue         nil)
    (font-lock-constant-face    nil          nil)
    (font-lock-builtin-face     blue         nil)
    (font-lock-preprocessor-face magenta     nil)
    (font-lock-string-face      green        nil)
    (font-lock-doc-face         green        nil)

    ;; filetype syntax highlighting
    (css-selector               blue         nil)
    (cider-repl-prompt-face     blue         nil          :weight bold)
    (clojure-keyword-face       blue         nil)
    (diff-added                 green        pale-green)
    (diff-changed               blue         pale-blue)
    (diff-context               nil          nil)
    (diff-file-header           nil          nil          :weight bold :inherit diff-header)
    (diff-header                black        pale-gray)
    (diff-hunk-header           magenta      pale-magenta)
    (diff-refine-added ((((type tty)) :inherit diff-added :underline t)
                        (t :inherit diff-added :box t)))
    (diff-refine-changed        nil          pale-blue)
    (diff-refine-removed ((((type tty)) :inherit diff-removed :underline t)
                          (t :inherit diff-removed :box t)))
    (diff-removed               red          pale-red)
    (js2-function-call          nil          nil)
    (ledger-font-pending-face   magenta      nil          :slant italic)
    (ledger-font-posting-date-face blue      nil)
    (ledger-occur-xact-face     nil          beige)
    (makefile-space             nil          pale-red)
    (markdown-header-face-1     black        pale-gray    :weight bold :height 1.3 :overline t)
    (markdown-header-face-2     blue         pale-cyan    :weight bold :overline t)
    (markdown-header-face-3     green        pale-green   :weight bold :overline t)
    (markdown-header-face-4     brown        nil          :weight bold)
    (markdown-header-face-5     magenta      nil          :weight bold)
    (markdown-header-face-6     cyan         nil          :weight bold :slant italic)
    (markdown-blockquote-face   green        nil)
    (markdown-inline-code-face  blue         nil          :inherit fixed-pitch)
    (markdown-language-keyword-face cyan     nil)
    (markdown-list-face         nil          nil)
    (markdown-pre-face          blue         nil          :inherit fixed-pitch)
    (markdown-reference-face    nil          nil)
    (markdown-table-face        green        nil          :inherit markdown-code-face)
    (markdown-url-face          blue         nil)
    (message-header-name        cyan         nil)
    (message-header-other       nil          nil)
    (message-header-to          nil          nil)
    (message-header-subject     black        pale-gray    :weight bold :overline t)
    (message-separator          nil          nil          :inherit shadow)
    (message-header-cc          nil          nil)
    (org-agenda-calendar-event  blue         pale-cyan    :weight bold)
    (org-agenda-calendar-sexp   nil          nil          :inherit org-agenda-calendar-event)
    (org-agenda-current-time    blue         nil          :underline t)
    (org-agenda-date            blue         nil          :inherit org-agenda-structure)
    (org-agenda-date-today      black        beige        :inherit org-agenda-date)
    (org-agenda-date-weekend    black        nil          :inherit org-agenda-date)
    (org-agenda-dimmed-todo-face nil         nil          :inherit shadow)
    (org-agenda-done            nil          nil)
    (org-agenda-filter-category nil          nil          :inherit org-agenda-filter-tags)
    (org-agenda-filter-tags     magenta      nil)
    (org-agenda-structure       cyan         nil          :weight bold :height 1.6)
    (org-checkbox               pale-yellow  cyan         :weight bold :box (:line-width 1 :style pressed-button))
    (org-document-title         black        nil          :weight bold :height 1.8)
    (org-document-info          nil          nil)
    (org-document-info-keyword  cyan         pale-cyan)
    (org-drawer                 cyan         pale-cyan)
    (org-meta-line              cyan         pale-cyan)
    (org-block-begin-line ((((type tty)) :foreground ,black :background ,pale-gray :underline t)
                            (t :foreground "#55554e" :background ,pale-gray :underline t :box "#ccccbb")))
    (org-block-end-line         nil          nil          :inherit org-block-begin-line)
    (org-block ((((type tty)) :background ,beige)
                (t :background "#f7f7db")))
    (org-level-1                black        pale-gray    :weight bold :height 1.3 :overline t)
    (org-level-2                blue         pale-cyan    :weight bold :overline t)
    (org-level-3                green        pale-green   :weight bold :overline t)
    (org-level-4                brown        nil          :weight bold)
    (org-level-5                magenta      nil          :weight bold)
    (org-level-6                cyan         nil          :weight bold :slant italic)
    (org-level-7                green        nil          :weight bold :slant italic)
    (org-level-8                brown        nil          :weight bold :slant italic)
    (org-table                  green        pale-green)
    (org-code                   green        nil)
    (org-verbatim               blue         nil)
    (org-date                   blue         nil          :underline t)
    (org-date-selected          pale-yellow  cyan         :weight bold)
    (org-sexp-date              blue         nil)
    (org-scheduled              nil          nil)
    (org-scheduled-today        nil          beige        :weight bold)
    (org-scheduled-previously   red          nil)
    (org-special-keyword        cyan         pale-cyan    :weight bold)
    (org-tag                    nil          nil          :slant italic :inherit shadow)
    (org-mode-line-clock        nil          nil)
    (org-mode-line-clock-overrun pale-yellow red)
    (org-time-grid              nil          nil          :inherit shadow)
    (org-todo                   red          pale-red     :weight bold :box t)
    (org-done                   nil          pale-gray    :inherit shadow :weight bold :box t)
    (org-upcoming-deadline      red          nil)
    (org-habit-alert-face       yellow       brown        :box ,brown)
    (org-habit-alert-future-face black       yellow       :box t :strike-through t)
    (org-habit-clear-face       pale-blue    blue         :box ,blue)
    (org-habit-clear-future-face blue        pale-cyan    :box t)
    (org-habit-overdue-face     pale-red     red          :box ,red)
    (org-habit-overdue-future-face red       pale-red     :box t)
    (org-habit-ready-face       pale-green   green        :box ,green)
    (org-habit-ready-future-face green       pale-green   :box t)
    (org-drill-hidden-cloze-face pale-yellow cyan)
    (org-drill-visible-cloze-face cyan       nil)
    (org-drill-visible-cloze-hint-face magenta nil)
    (outline-1                  black        pale-gray    :weight bold :height 1.3 :overline t)
    (outline-2                  blue         pale-cyan    :weight bold :overline t)
    (outline-3                  green        pale-green   :weight bold :overline t)
    (outline-4                  brown        nil          :weight bold)
    (outline-5                  magenta      nil          :weight bold)
    (outline-6                  cyan         nil          :weight bold :slant italic)
    (outline-7                  green        nil          :weight bold :slant italic)
    (outline-8                  brown        nil          :weight bold :slant italic)
    (rainbow-delimiters-depth-1-face nil     nil)
    (rainbow-delimiters-depth-2-face black   pale-gray)
    (rainbow-delimiters-depth-3-face blue    pale-cyan)
    (rainbow-delimiters-depth-4-face green   pale-green)
    (rainbow-delimiters-depth-5-face brown   beige)
    (rainbow-delimiters-depth-6-face magenta pale-magenta)
    (rainbow-delimiters-depth-7-face cyan    pale-cyan)
    (rainbow-delimiters-depth-8-face green   nil)
    (rainbow-delimiters-depth-9-face brown   nil)
    (rainbow-delimiters-mismatched-face red  pale-red     :weight bold)
    (rainbow-delimiters-unmatched-face red   pale-red     :weight bold)
    (rst-adornment              cyan         nil)
    (rst-directive              magenta      nil)
    (rst-literal                green        nil)
    (rst-reference              blue         nil)
    (rst-level-1                black        pale-gray    :weight bold :height 1.3 :overline t)
    (rst-level-2                blue         pale-cyan    :weight bold :overline t)
    (rst-level-3                green        pale-green   :weight bold :overline t)
    (rst-level-4                brown        nil          :weight bold)
    (rst-level-5                magenta      nil          :weight bold)
    (rst-level-6                cyan         nil          :weight bold :slant italic)
    (sh-heredoc                 green        nil          :inherit font-lock-string-face)
    (sh-quoted-exec             nil          nil)
    (vimrc-number               nil          nil)
    (web-mode-html-tag-face     cyan         nil)
    (web-mode-html-attr-name-face nil        nil)

    ;; package interface elements
    (calendar-month-header      nil          nil          :weight bold)
    (calendar-today             nil          beige        :weight bold :box ,brown)
    (calendar-weekday-header    blue         nil          :weight bold)
    (calendar-weekend-header    nil          nil          :weight bold)
    (diary                      magenta      nil)
    (holiday                    nil          pale-red     :box ,red)
    (bui-hint-key ((((type tty)) :foreground ,magenta)
                   (t :foreground ,pale-magenta :distant-foreground ,magenta)))
    (bui-info-param-title       nil          nil          :weight bold)
    (company-scrollbar-bg       nil          nil          :inherit company-tooltip)
    (company-scrollbar-fg       nil          cyan)
    (company-tooltip            nil          pale-cyan)
    (company-tooltip-common     nil          nil          :weight bold)
    (company-tooltip-annotation cyan         nil)
    (company-tooltip-annotation-selection pale-yellow nil)
    (company-tooltip-selection  pale-yellow  cyan)
    (custom-button-pressed-unraised magenta  nil          :underline t)
    (custom-changed             nil          nil          :inverse-video t :inherit custom-set)
    (custom-comment             black        pale-gray)
    (custom-comment-tag         blue         nil)
    (custom-group-tag           blue         nil          :weight bold :height 1.2 :inherit variable-pitch)
    (custom-group-tag-1         red          nil          :inherit custom-group-tag)
    (custom-invalid             red          pale-red     :weight bold)
    (custom-modified            nil          nil          :inherit custom-changed)
    (custom-rogue               pale-red     black)
    (custom-set                 blue         nil)
    (custom-state               green        nil)
    (custom-themed              nil          nil          :inherit custom-changed)
    (custom-variable-tag        blue         nil          :weight bold)
    (comint-highlight-input     blue         nil          :weight bold)
    (comint-highlight-prompt    blue         nil          :weight bold)
    (dired-directory            blue         nil          :weight bold)
    (dired-header               blue         nil          :weight bold)
    (dired-ignored              brown        nil)
    (dired-symlink              cyan         nil          :weight bold)
    (eldoc-highlight-function-argument blue  nil          :weight bold :underline t)
    (elfeed-search-date-face    blue         nil)
    (elfeed-search-feed-face    brown        nil)
    (elfeed-search-tag-face     green        nil)
    (emms-browser-album-face    green        pale-green   :weight bold :overline t)
    (emms-browser-artist-face   blue         pale-cyan    :weight bold :overline t)
    (emms-browser-composer-face nil          nil          :inherit emms-browser-artist-face)
    (emms-browser-performer-face nil         nil          :inherit emms-browser-artist-face)
    (emms-browser-track-face    nil          nil)
    (emms-browser-year/genre-face black      pale-gray    :weight bold :height 1.2 :overline t)
    (emms-metaplaylist-mode-current-face pale-red red)
    (emms-metaplaylist-mode-face red         nil)
    (emms-playlist-selected-face pale-yellow blue         :weight bold)
    (emms-playlist-track-face   nil          nil)
    (emms-stream-name-face      nil          nil          :weight bold)
    (emms-stream-url-face       blue         nil          :underline t)
    (eshell-ls-archive          nil          nil)
    (eshell-ls-backup           brown        nil)
    (eshell-ls-clutter          brown        nil)
    (eshell-ls-directory        blue         nil          :weight bold)
    (eshell-ls-executable       green        nil          :weight bold)
    (eshell-ls-missing          red          nil          :strike-through t :slant italic)
    (eshell-ls-product          nil          nil)
    (eshell-ls-readonly         nil          nil)
    (eshell-ls-special          magenta      nil)
    (eshell-ls-symlink          cyan         nil          :weight bold)
    (eshell-ls-unreadable       red          nil)
    (eshell-prompt              blue         nil          :weight bold)
    (evil-ex-info               red          nil          :slant italic)
    (evil-ex-substitute-replacement red      nil          :underline t)
    (evil-mc-cursor-default-face pale-yellow blue)
    (evil-mc-cursor-bar-face    nil          blue         :height 1)
    (evil-mc-cursor-hbar-face   nil          nil          :underline (:color ,blue :style line))
    (eww-invalid-certificate    red          nil          :inherit eww-valid-certificate)
    (eww-valid-certificate      nil          nil          :weight bold)
    (flycheck-fringe-error      red          pale-red)
    (flycheck-fringe-info       green        pale-green)
    (flycheck-fringe-warning    red          pale-red)
    (geiser-font-lock-autodoc-current-arg blue nil        :weight bold :underline t)
    (geiser-font-lock-autodoc-identifier nil nil)
    (geiser-font-lock-doc-link  nil          nil          :inherit link)
    (geiser-font-lock-error-link nil         nil          :inherit link)
    (geiser-font-lock-repl-input nil         nil          :inherit geiser-font-lock-repl-prompt)
    (geiser-font-lock-repl-output nil        nil)
    (geiser-font-lock-repl-prompt blue       nil          :weight bold)
    (geiser-font-lock-xref-link nil          nil          :inherit link)
    (guix-build-log-phase-end   green        nil)
    (guix-build-log-phase-start green        nil          :weight bold)
    (guix-derivation-drv-file-name cyan      nil          :inherit guix-derivation-file-name)
    (guix-operation-option-key  nil          nil          :inherit bui-hint-key)
    (guix-package-info-description nil       nil)
    (guix-package-info-future   magenta      nil          :weight bold)
    (guix-package-info-license  nil          nil)
    (guix-package-info-source   nil          nil          :inherit link)
    (guix-true                  green        nil          :weight bold)
    (helm-M-x-key               magenta      nil)
    (helm-buffer-archive        blue         nil)
    (helm-buffer-directory      blue         nil          :weight bold)
    (helm-buffer-file           nil          nil)
    (helm-buffer-modified       nil          nil          :inherit font-lock-comment-face)
    (helm-buffer-not-saved      red          nil)
    (helm-buffer-process        blue         nil)
    (helm-buffer-saved-out      red          pale-red     :weight bold :slant italic)
    (helm-buffer-size           nil          nil          :inherit shadow)
    (helm-candidate-number      black        yellow)
    (helm-etags-file            brown        nil)
    (helm-delete-async-message  yellow       nil          :distant-foreground ,brown)
    (helm-ff-denied             red          nil)
    (helm-ff-directory          blue         nil          :weight bold)
    (helm-ff-dotted-directory   nil          nil          :inherit helm-ff-directory)
    (helm-ff-dotted-symlink-directory nil    nil          :inherit helm-ff-symlink)
    (helm-ff-executable         green        nil)
    (helm-ff-file               nil          nil)
    (helm-ff-invalid-symlink    red          nil          :strike-through t :slant italic)
    (helm-ff-pipe               magenta      nil)
    (helm-ff-prefix             nil          yellow)
    (helm-ff-socket             magenta      nil)
    (helm-ff-suid               green        pale-green   :weight bold)
    (helm-ff-symlink            cyan         nil          :weight bold)
    (helm-ff-truename           nil          nil)
    (helm-grep-file             nil          nil          :inherit compilation-info)
    (helm-grep-finish           green        nil)
    (helm-grep-lineno           nil          nil          :inherit compilation-line-number)
    (helm-grep-match            nil          pale-magenta :weight bold)
    (helm-history-remote        magenta      nil)
    (helm-header-line-left-margin nil        yellow)
    (helm-match                 nil          pale-magenta)
    (helm-prefarg               red          nil)
    (helm-moccur-buffer         cyan         nil          :underline t)
    (helm-selection             nil          pale-blue)
    (helm-source-header         black        pale-gray    :weight bold :height 1.3 :inherit variable-pitch)
    (helm-visible-mark          pale-yellow  red)
    (hi-black-b                 black        nil          :weight bold)
    (hi-black-hb                black        nil          :weight bold :height 1.3 :inherit variable-pitch)
    (hi-blue                    nil          pale-blue    :box ,blue)
    (hi-blue-b                  blue         nil          :weight bold)
    (hi-green                   nil          pale-green   :box ,green)
    (hi-green-b                 green        nil          :weight bold)
    (hi-pink                    nil          pale-red     :box ,red)
    (hi-red-b                   red          nil          :weight bold)
    (hi-yellow                  nil          beige        :box ,brown)
    (ido-indicator              yellow       red          :weight bold)
    (ido-only-match             green        nil)
    (ido-subdir                 blue         nil          :weight bold)
    (ido-virtual                nil          nil          :inherit shadow)
    (info-title-1               black        pale-gray    :weight bold :height 1.3 :overline t)
    (info-title-2               blue         pale-cyan    :weight bold :overline t)
    (info-title-3               green        pale-green   :weight bold :overline t)
    (info-title-4               brown        nil          :weight bold)
    (info-header-node           magenta      nil          :underline t)
    (info-menu-header           blue         pale-cyan    :weight bold :overline t)
    (info-menu-star             nil          nil)
    (info-node                  blue         nil          :underline t)
    (ivy-current-match          pale-yellow  blue         :weight bold)
    (ivy-minibuffer-match-face-1 nil         pale-gray)
    (ivy-minibuffer-match-face-2 nil         pale-magenta :weight bold)
    (ivy-minibuffer-match-face-3 nil         pale-blue    :weight bold)
    (ivy-minibuffer-match-face-4 nil         yellow       :weight bold)
    (ivy-modified-buffer        nil          nil          :inherit font-lock-comment-face)
    (ivy-modified-outside-buffer red         pale-red     :weight bold :slant italic)
    (ivy-org                    green        nil)
    (ivy-remote                 magenta      nil)
    (ivy-virtual                nil          nil          :inherit shadow)
    (ivy-confirm-face           green        nil)
    (ivy-match-required-face    red          nil)
    (ivy-cursor                 pale-yellow  black        :weight bold)
    (counsel-application-name   green        nil)
    (counsel-key-binding        magenta      nil)
    (git-commit-comment-file    nil          nil)
    (magit-bisect-bad           red          nil)
    (magit-bisect-good          green        nil)
    (magit-bisect-skip          cyan         nil)
    (magit-blame-date           blue         nil)
    (magit-blame-highlight      black        pale-gray)
    (magit-blame-name           magenta      nil)
    (magit-branch-local         blue         nil)
    (magit-branch-remote        green        nil)
    (magit-cherry-equivalent    magenta      nil)
    (magit-cherry-unmatched     cyan         nil)
    (magit-diff-added           green        pale-green)
    (magit-diff-added-highlight nil          nil          :inherit magit-diff-added)
    (magit-diff-base            brown        beige)
    (magit-diff-base-highlight  nil          nil          :inherit magit-diff-base)
    (magit-diff-context         black        pale-gray)
    (magit-diff-context-highlight nil        nil          :inherit magit-diff-context)
    (magit-diff-file-heading-selection nil   nil          :inherit magit-diff-file-heading-highlight)
    (magit-diff-hunk-heading    cyan         pale-cyan)
    (magit-diff-hunk-heading-highlight magenta pale-magenta)
    (magit-diff-hunk-heading-selection nil   nil          :inherit magit-diff-hunk-heading-highlight)
    (magit-diff-lines-heading   pale-magenta magenta      :weight bold)
    (magit-diff-removed         red          pale-red)
    (magit-diff-removed-highlight nil        nil          :inherit magit-diff-removed)
    (magit-diffstat-added       green        nil)
    (magit-diffstat-removed     red          nil)
    (magit-dimmed               nil          nil          :inherit shadow)
    (magit-hash                 cyan         nil)
    (magit-header-line          nil          nil          :weight bold)
    (magit-keyword              blue         nil)
    (magit-log-author           magenta      nil)
    (magit-log-date             blue         nil)
    (magit-log-graph            cyan         nil)
    (magit-mode-line-process-error pale-red  nil          :distant-foreground ,red :weight bold)
    (magit-popup-argument       blue         nil)
    (magit-popup-key            magenta      nil)
    (magit-process-ng           red          nil          :weight bold)
    (magit-process-ok           green        nil          :weight bold)
    (magit-reflog-amend         magenta      nil)
    (magit-reflog-checkout      blue         nil)
    (magit-reflog-cherry-pick   nil          nil          :inherit magit-reflog-commit)
    (magit-reflog-commit        green        nil)
    (magit-reflog-merge         nil          nil          :inherit magit-reflog-commit)
    (magit-reflog-other         cyan         nil)
    (magit-reflog-rebase        magenta      nil)
    (magit-reflog-remote        cyan         nil)
    (magit-reflog-reset         red          nil)
    (magit-refname              cyan         nil)
    (magit-section-heading      brown        nil          :weight bold)
    (magit-section-heading-selection brown   nil)
    (magit-section-highlight    nil          pale-gray)
    (magit-sequence-drop        red          nil)
    (magit-sequence-head        blue         nil)
    (magit-sequence-part        brown        nil)
    (magit-sequence-stop        green        nil)
    (magit-signature-bad        red          pale-red     :weight bold)
    (magit-signature-error      red          nil)
    (magit-signature-expired    brown        nil)
    (magit-signature-expired-key nil         nil          :inherit magit-signature-expired)
    (magit-signature-good       green        nil)
    (magit-signature-revoked    magenta      nil)
    (magit-signature-untrusted  cyan         nil)
    (magit-tag                  brown        nil)
    (mu4e-cited-1-face          blue         nil          :slant italic)
    (mu4e-cited-2-face          green        nil          :slant italic)
    (mu4e-cited-3-face          brown        nil          :slant italic)
    (mu4e-cited-4-face          blue         nil          :slant italic)
    (mu4e-cited-5-face          green        nil          :slant italic)
    (mu4e-cited-6-face          brown        nil          :slant italic)
    (mu4e-cited-7-face          magenta      nil          :slant italic)
    (mu4e-conversation-header   brown        beige        :box t)
    (mu4e-conversation-sender-1 nil          pale-gray)
    (mu4e-conversation-sender-2 nil          pale-cyan)
    (mu4e-conversation-sender-3 nil          pale-green)
    (mu4e-conversation-sender-4 nil          beige)
    (mu4e-conversation-sender-5 nil          pale-red)
    (mu4e-conversation-sender-6 blue         nil)
    (mu4e-conversation-sender-7 green        nil)
    (mu4e-conversation-sender-8 magenta      nil)
    (mu4e-conversation-sender-me nil         nil)
    (mu4e-header-value-face     nil          nil)
    (mu4e-special-header-value-face nil      nil)
    (mu4e-modeline-face         pale-green   nil          :weight bold :distant-foreground ,green)
    (pass-mode-directory-face   blue         nil          :weight bold)
    (solaire-default-face       nil          "#f7f7db")
    (solaire-fringe-face        black        pale-gray)
    (solaire-hl-line-face       nil          beige)
    (term-color-black           black        pale-gray)
    (term-color-red             red          pale-red)
    (term-color-green           green        pale-green)
    (term-color-yellow          brown        yellow)
    (term-color-blue            blue         pale-blue)
    (term-color-magenta         magenta      pale-magenta)
    (term-color-cyan            cyan         pale-cyan)
    (term-color-white           beige        pale-yellow)
    (transient-argument         blue         nil)
    (transient-disabled-suffix  nil          pale-red     :box ,red)
    (transient-enabled-suffix   nil          pale-green   :box ,green)
    (transient-key              magenta      nil)
    (transient-separator        nil          nil          :inherit shadow :inverse-video t)
    (undo-tree-visualizer-active-face nil    nil          :weight bold)
    (undo-tree-visualizer-current-face red   nil)
    (undo-tree-visualizer-default-face nil   nil          :inherit shadow)
    (undo-tree-visualizer-register-face magenta nil)
    (undo-tree-visualizer-unmodified-face cyan nil)
    (which-key-docstring-face   nil          nil          :inherit font-lock-doc-face)
    (which-key-group-description-face blue   nil)
    (which-key-key-face         magenta      nil)
    (which-key-note-face        nil          nil          :inherit font-lock-comment-face)
    (which-key-separator-face   nil          nil))

  (custom-theme-set-variables 'parchment
   ;; shell-mode colors
   `(ansi-color-names-vector
     [,black ,red ,green ,brown ,blue ,magenta ,cyan ,beige])
   `(org-drill-done-count-color ,brown)
   `(org-drill-failed-count-color ,red)
   `(org-drill-mature-count-color ,green)
   `(org-drill-new-count-color ,blue)))

;;;###autoload
(when (and (boundp 'custom-theme-load-path)
           load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'parchment)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; parchment-theme.el ends here
