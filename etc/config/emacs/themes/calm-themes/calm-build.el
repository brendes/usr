;;; calm-build.el --- Calm theme builder -*- no-byte-compile: t; -*-

;; Copyright (C) 2025 Brendan Desmond

;; Author: Brendan Desmond
;; Version: 0.1

;; This program is free software; you can ,redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option) any
;; later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A minimal theme with clear and simple UI.

;;; Code:

(require 'cl-lib)

;;;; faces
(defface calm-border nil
  "Face for border lines around UI elements."
  :group 'faces)

(defface calm-code nil
  "Face for inline code snippets."
  :group 'faces)

(defface calm-strong nil
  "Face for item selection such as in popup menu items."
  :group 'faces)

;;;; ui
(defun calm--set-ui ()
  (dolist (sym '(left-arrow right-arrow left-curly-arrow right-curly-arrow))
    (set-fringe-bitmap-face sym 'shadow))
  (setq window-divider-default-places t)
  (setq window-divider-default-right-width 1)
  (setq window-divider-default-bottom-width 1)
  (window-divider-mode -1)
  (setq highlight-indent-guides-auto-enabled nil)
  (setq x-underline-at-descent-line nil)
  (diff-hl-mode 1)
  (diff-hl-margin-mode -1))

;;;; helper functions
(defun calm--default-weight-fg (medium-color normal-color)
  "Return different strings depending on the `:weight` attribute of a the
default face. For example, return a hex string for a lighter color when the
face's weight is medium, and a hex string for a darker color when it's not
(e.g., normal weight)."
  (if (string-equal (face-attribute 'default :weight) 'medium)
      medium-color
    normal-color))

(defun calm--set-theme-appearance (mode)
  "Set theme appearance for MODE ('light or 'dark) across platforms."
  (when (eq system-type 'darwin)
    (setq default-frame-alist
          (append default-frame-alist
                  `((ns-transparent-titlebar . t)
                    (ns-appearance . ,mode))))
    ;; Force update existing frames' titlebars
    (modify-all-frames-parameters
     `((ns-transparent-titlebar . t)
       (ns-appearance . ,mode)))))

;;;; constants and variables
(defvar calm-ui-font-height 0.9)

(defconst calm-light-common-colors
  '((black      "#000000")
    (white      "#ffffff")
    (red-1      "#bb5458")
    (red-2      "#e88888")
    (red-3      "#f0d0d0")
    (orange-1   "#d08770")
    (orange-2   "#efaf8f")
    (yellow-1   "#b09070")
    (yellow-2   "#efd8af")
    (green-1    "#809874")
    (green-2    "#afbf87")
    (green-3    "#e0e8c8")
    (cyan-1     "#78a8a4")
    (cyan-2     "#bfdfd8")
    (blue-1     "#6078a0")
    (blue-2     "#aabbee")
    (blue-3     "#e0e8f0")
    (magenta-1  "#af7898")
    (magenta-2  "#d8a8d0")
    (ok-fg      cyan-1)
    (ok-bg      cyan-2)
    (warn-fg    "#bf6880")
    (warn-bg    "#efc8d8")
    (link       "#5577bb"))
  "All the semantic color mappings shared by every light variant.")

(defconst calm-dark-common-colors
  '((black      "#000000")
    (white      "#ffffff")
    (red-1      "#ee888b")
    (red-2      red-1)
    (red-3      red-1)
    (orange-1   "#dd8866")
    (orange-2   "#dd8866")
    (yellow-1   "#cc9966")
    (yellow-2   "#cc9966")
    (green-1    "#a9b665")
    (green-2    green-1)
    (green-3    green-2)
    (blue-1     "#7da3b8")
    (blue-2     blue-1)
    (blue-3     blue-1)
    (magenta-1  "#d3889b")
    (magenta-2  magenta-1)
    (cyan-1     "#88b8a8")
    (cyan-2     cyan-1)
    (ok-fg      cyan-1)
    (ok-bg      cyan-1)
    (warn-bg    red-1)
    (warn-fg    red-1)
    (link       cyan-1))
  "All the semantic color mappings shared by every dark variant.")

(defmacro calm-define-variant (name doc mode overrides)
  "Define a Calm theme VARIANT by merging OVERRIDES into the shared palette.

NAME      — symbol, e.g. `calm-dark`
DOC       — docstring for the theme
MODE      — either the symbol `dark` or `light`
OVERRIDES — literal alist of (KEY VALUE) pairs (can be quoted)"
  (calm--set-ui)
  ;; pick shared palette at compile time
  (let* ((common (eval
                  (if (eq mode 'dark)
                      'calm-dark-common-colors
                    'calm-light-common-colors)))
         ;; unwrap a leading quote if present
         (ovr (if (and (consp overrides) (eq (car overrides) 'quote))
                  (cadr overrides)
                overrides))
         (default-border (if (eq mode 'dark) 'fg-faint 'fg-fade))
         (default-black  (if (eq mode 'dark) 'bg-main  'fg-main))
         (default-white  (if (eq mode 'dark) 'fg-main  'bg-main))
         ;; inject any missing defaults into the override list
         (ovr2
          (let ((base ovr))
            (dolist (pair
                     `((region   bg-hl)
                       (border   ,default-border)
                       (black    ,default-black)
                       (white    ,default-white)
                       (bg-bold  fg-faint)))
              (unless (assoc (car pair) base)
                (setq base (append base (list pair)))))
            base))
         ;; final binding list: shared palette first, then overrides
         (all-bindings (append common ovr2)))
    `(progn
       (deftheme ,name ,doc
         :background-mode ',mode
         :kind 'color-scheme
         :family 'calm)
       (calm--set-theme-appearance ',mode)
       (let* ,all-bindings
         (calm-build-theme ',name))
       (provide-theme ',name))))

;;;; core theme definition
(defun calm-build-theme (theme-name)
  "Build a custom theme named THEME-NAME.  Colors are specified by wrapping the call to this function in a let which defines them."
  (custom-theme-set-faces
   theme-name
   ;; main faces from which others are derived
   `(default ((t (:foreground ,fg-main :background ,bg-main))))
   `(bold ((t (:foreground ,fg-bold :weight bold))))
   `(cursor ((t (:foreground ,bg-main :background ,fg-bold))))
   `(hl-line ((t (:foreground unspecified :background ,bg-fade))))
   `(highlight ((t (:foreground unspecified :background ,bg-hl))))
   `(link ((t (:foreground ,link :underline t))))
   `(lazy-highlight ((t (:foreground ,fg-bold :background ,bg-bold))))
   `(match ((t (:foreground ,bg-main :background ,fg-subtle))))
   `(region ((t (:distant-foreground ,fg-dim :background ,region :extend nil))))
   `(shadow ((t (:foreground ,fg-dim))))
   `(success ((t (:foreground ,ok-fg :weight bold))))
   `(warning ((t (:foreground ,red-1 :weight bold))))
   `(calm-border ((t (:foreground ,border :background ,border))))
   `(calm-code ((t (:foreground ,red-1 :background ,bg-subtle))))
   `(calm-strong ((t (:foreground ,fg-bold :background ,bg-bold))))

   ;; editor and ui
   `(completions-common-part ((t (:inherit bold))))
   `(error ((t (:inherit warning :underline (:color ,warn-fg :style wave)))))
   `(fixed-pitch ((t (:inherit default))))
   `(fringe ((t (:inherit default))))
   `(fill-column-indicator ((t (:foreground ,bg-fade))))
   `(isearch ((t (:inherit match))))
   `(isearch-fail ((t (:foreground ,black :background ,warn-bg))))
   `(line-number ((t (:foreground ,fg-faint))))
   `(line-number-current-line ((t (:foreground ,fg-subtle))))
   `(link-visited ((t (:inherit link :foreground ,fg-subtle))))
   `(minibuffer-prompt ((t (:foreground ,fg-main :weight bold))))
   `(secondary-selection ((t (:inherit shadow :inverse-video t :extend nil))))
   `(show-paren-match ((t (:inherit calm-strong :underline t))))
   `(vertical-border ((t (:inherit calm-border))))
   `(window-divider ((t (:inherit calm-border))))
   `(window-divider-first-pixel ((t (:inherit window-divider))))
   `(window-divider-last-pixel  ((t (:inherit window-divider))))

   ;; syntax
   `(escape-glyph ((t (:foreground ,warn-fg))))
   `(font-lock-comment-face ((t (:foreground ,fg-dim))))
   `(font-lock-escape-face ((t (:foreground ,fg-subtle))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,fg-subtle))))
   `(font-lock-string-face ((t (:foreground ,fg-subtle))))
   `(font-lock-builtin-face ((t (:inherit font-lock-keyword-face))))
   `(font-lock-constant-face ((t nil)))
   `(font-lock-doc-face ((t (:inherit font-lock-string-face))))
   `(font-lock-function-call-face ((t (:inherit font-lock-keyword-face))))
   `(font-lock-function-name-face ((t nil)))
   `(font-lock-keyword-face ((t nil)))
   `(font-lock-negation-char-face ((t (:inherit font-lock-keyword-face))))
   `(font-lock-number-face ((t (:inherit font-lock-constant-face))))
   `(font-lock-operator-face ((t (:inherit font-lock-keyword-face))))
   `(font-lock-preprocessor-face ((t nil)))
   `(font-lock-punctuation-face ((t nil)))
   `(font-lock-type-face ((t nil)))
   `(font-lock-variable-name-face ((t nil)))
   `(font-lock-variable-use-face ((t nil)))
   `(font-lock-warning-face ((t (:inherit warning))))
   `(sh-heredoc ((t nil)))
   `(sh-quoted-exec ((t nil)))
   `(trailing-whitespace ((t (:foreground ,warn-fg :background ,warn-bg))))

   ;; third-party syntax
   `(clojure-keyword-face ((t (:inherit font-lock-keyword-face))))
   `(tuareg-font-double-semicolon-face ((t (:inherit font-lock-keyword-face))))
   `(tuareg-font-lock-governing-face ((t (:inherit font-lock-keyword-face))))
   `(tuareg-font-lock-operator-face ((t (:inherit font-lock-operator-face))))
   `(tuareg-font-lock-doc-verbatim-face ((t (:inherit font-lock-doc-face))))

   ;; mode-line and header-line
   `(mode-line
     ((t (:inherit mode-line
          :height ,calm-ui-font-height
          :foreground ,fg-main
          :background ,bg-fade
          :overline ,border
          :underline nil
          :box (:line-width 3 :style flat-button)))))
   `(mode-line-buffer-id ((t (:inherit variable-pitch :weight bold))))
   `(mode-line-inactive
     ((t (:inherit mode-line :foreground ,fg-fade :overline ,border))))
   `(mode-line-highlight
     ((t (:inherit mode-line :foreground ,fg-bold :background ,bg-hl))))
   `(header-line ((t (:inherit mode-line :overline ,border))))

   ;; tab-bar
   `(tab-bar
     ((t (:inherit variable-pitch
                   :foreground ,fg-main
                   :height ,calm-ui-font-height))))
   `(tab-bar-tab ((t (:inherit tab-bar :background ,bg-main :weight bold))))
   `(tab-bar-tab-inactive ((t (:inherit tab-bar :foreground ,fg-dim))))

   ;; tab-line
   `(tab-line ((t (:inherit mode-line))))
   `(tab-line-tab ((t (:inherit tab-line :foreground ,fg-dim))))
   `(tab-line-tab-current ((t (:inherit (bold tab-line-tab)))))
   `(tab-line-tab-inactive ((t (:foreground ,fg-dim))))
   `(tab-line-tab-modified ((t (:slant italic))))
   `(tab-line-highlight ((t (:inherit (mode-line-highlight tab-line-tab)))))
   `(tab-line-close-highlight ((t (:inherit tab-line-highlight))))

   ;; outline-mode and org-mode
   `(outline-1 ((t (:inherit bold))))
   `(outline-2 ((t (:inherit outline-1))))
   `(outline-3 ((t (:inherit outline-1))))
   `(outline-4 ((t (:inherit outline-1))))
   `(outline-5 ((t (:inherit outline-1))))
   `(outline-6 ((t (:inherit outline-1))))
   `(outline-7 ((t (:inherit outline-1))))
   `(outline-8 ((t (:inherit outline-1))))
   ;;
   `(org-agenda-structure ((t (:inherit bold))))
   `(org-block ((t (:background ,bg-subtle))))
   `(org-block-begin-line ((t (:inherit shadow :slant italic))))
   `(org-block-end-line ((t (:inherit org-block-begin-line))))
   `(org-checkbox-statistics-todo ((t (:inherit (outline-1 warning)))))
   `(org-checkbox-statistics-done ((t (:inherit (outline-1 success)))))
   `(org-code ((t (:inherit calm-code))))
   `(org-date ((t (:inherit link))))
   `(org-document-info ((t (:inherit default))))
   `(org-document-title ((t (:inherit default))))
   `(org-drawer ((t (:inherit shadow))))
   `(org-formula ((t (:inherit (org-formula org-code)))))
   `(org-headline-done ((t (:inherit outline-1))))
   `(org-hide ((t (:foreground ,bg-main))))
   `(org-list-bullet ((t (:inherit bold))))
   `(org-list-dt ((t (:inherit bold))))
   `(org-meta-line ((t (:inherit font-lock-comment-face))))
   `(org-quote ((t (:inherit org-block :italic t))))
   `(org-table ((t (:foreground ,fg-subtle :family fixed-pitch))))
   `(org-table-header ((t (:inherit bold :background ,bg-fade :family fixed-pitch))))
   `(org-tag ((t (:inherit shadow :weight normal))))
   `(org-todo ((t (:foreground ,black :background ,warn-bg :weight bold))))
   `(org-done ((t (:foreground ,black :background ,ok-bg :weight bold))))
   `(org-verbatim ((t (:inherit org-code))))

   ;; etc
   `(comint-highlight-prompt ((t (:foreground ,fg-dim))))
   `(custom-group-tag
     ((t (:inherit custom-group-tag :foreground ,fg-main :weight bold))))
   `(custom-variable-tag
     ((t (:inherit custom-variable-tag :foreground ,fg-main :weight bold))))
   `(dictionary-word-definition-face ((t (:inherit variable-pitch))))
   `(dired-directory ((t (:inherit default))))
   `(dired-header ((t (:inherit bold))))
   `(dired-broken-symlink ((t (:inherit warning))))
   `(eshell-ls-directory ((t (:inherit default))))
   `(eshell-ls-symlink ((t (:inherit default))))
   `(eshell-prompt ((t (:inherit shadow))))
   `(eww-form-submit
     ((t (:inherit eww-form-submit :foreground ,fg-main :background ,bg-hl :box (:line-width 1 :style released-button)))))
   `(eww-form-text
     ((t (:background ,bg-fade :box (:line-width 1 :color ,fg-fade)))))
   `(eww-form-textarea
     ((t (:inherit eww-form-text :box (:line-width 1 :color ,bg-main)))))
   `(eww-valid-certificate
     ((t (:inherit eww-valid-certificate :foreground ,fg-bold))))
   `(eww-invalid-certificate ((t (:inherit (eww-invalid-certificate warning)))))
   `(gnus-header-name ((t (:inherit shadow))))
   `(gnus-header-content ((t (:inherit default))))
   `(gnus-signature ((t (:inherit default))))
   `(help-key-binding ((t (:inherit org-code))))
   `(Info-quoted      ((t (:inherit (fixed-pitch-serif org-code)))))
   `(info-header-node ((t (:inherit bold :italic t))))
   `(info-menu-star   ((t (:inherit shadow))))
   `(info-node        ((t (:inherit bold :italic t))))
   `(nobreak-space ((t (:inherit shadow))))
   `(nobreak-hyphen ((t (:inherit shadow))))
   `(shr-code ((t (:inherit calm-code))))
   `(shr-h1 ((t (:inherit shr-text :height 1.3 :weight bold))))
   `(shr-h2 ((t (:inherit shr-text :height 1.2 :weight bold))))
   `(shr-h3 ((t (:inherit shr-text :height 1.1 :weight bold))))
   `(shr-h4 ((t (:inherit shr-text :weight bold))))
   ;;
   `(term-color-black ((t (:foreground ,black))))
   `(term-color-blue ((t (:foreground ,blue-1))))
   `(term-color-cyan ((t (:foreground ,cyan-1))))
   `(term-color-green ((t (:foreground ,green-1))))
   `(term-color-magenta ((t (:foreground ,magenta-1))))
   `(term-color-red ((t (:foreground ,red-1))))
   `(term-color-white ((t (:foreground ,bg-hl))))
   `(term-color-yellow ((t (:foreground ,yellow-1))))
   `(term-color-bright-black ((t (:foreground ,fg-fade))))
   `(term-color-bright-blue ((t (:foreground ,blue-2))))
   `(term-color-bright-cyan ((t (:foreground ,cyan-2))))
   `(term-color-bright-green ((t (:foreground ,green-2))))
   `(term-color-bright-magenta ((t (:foreground ,magenta-2))))
   `(term-color-bright-red ((t (:foreground ,red-2))))
   `(term-color-bright-white ((t (:foreground ,white))))
   `(term-color-bright-yellow ((t (:foreground ,yellow-2))))

   ;; third-party
   `(corfu-border  ((t (:background ,fg-dim))))
   `(corfu-current ((t (:inherit calm-strong))))
   `(corfu-default ((t (:background ,bg-subtle :foreground ,fg-main))))
   ;;
   `(diff-hl-change ((t (:foreground ,blue-3 :background ,blue-3))))
   `(diff-hl-insert ((t (:foreground ,green-3 :background ,green-3))))
   `(diff-hl-delete ((t (:foreground ,red-3 :background ,red-3))))
   `(diff-hl-margin-change ((t (:foreground ,blue-1))))
   `(diff-hl-margin-insert ((t (:foreground ,green-1))))
   `(diff-hl-margin-delete ((t (:foreground ,red-1))))
   ;;
   `(elfeed-search-date-face ((t (:inherit shadow))))
   `(elfeed-search-unread-title-face ((t (:inherit bold))))
   `(elfeed-search-feed-face ((t (:inherit shadow))))
   `(elfeed-search-tag-face ((t (:inherit shadow))))
   ;;
   `(eshell-syntax-highlighting-alias-face ((t (:inherit default))))
   `(eshell-syntax-highlighting-invalid-face ((t (:inherit error))))
   `(eshell-syntax-highlighting-shell-builtin-face ((t (:inherit default))))
   `(eshell-syntax-highlighting-shell-command-face ((t (:inherit default))))
   ;;
   `(ess-assignment-face ((t (:inherit bold))))
   `(ess-constant-face ((t (:inherit font-lock-constant-face))))
   `(ess-modifiers-face ((t (:inherit bold))))
   `(ess-%op%-face ((t (:inherit bold))))
   ;;
   `(evil-ex-substitute-matches ((t (:inherit lazy-highlight))))
   `(evil-ex-substitute-replacement ((t (:foreground ,black :background ,ok-bg :weight bold))))
   ;;
   `(font-latex-bold-face ((t (:inherit bold))))
   `(font-latex-italic-face ((t (:foreground ,fg-dim :italic t))))
   `(font-latex-math-face ((t (:foreground ,fg-subtle))))
   `(font-latex-sedate-face ((t (:foreground ,fg-subtle))))
   `(font-latex-superscript-face ((t (:inherit font-latex-superscript-face :foreground ,fg-dim :height 0.85))))
   `(font-latex-verbatim-face ((t (:inherit font-lock-string-face))))
   `(font-latex-warning-face ((t (:inherit warning))))
   ;;
   `(highlight-indent-guides-odd-face ((t (:background ,bg-hl))))
   `(highlight-indent-guides-even-face ((t (:background ,bg-hl))))
   `(highlight-indent-guides-character-face ((t (:foreground ,bg-hl))))
   `(highlight-indentation-face ((t (:background ,bg-fade))))
   ;;
   `(magit-branch-local ((t (:inherit bold))))
   `(magit-branch-remote ((t (:inherit bold))))
   `(magit-hash ((t (:foreground ,fg-subtle))))
   `(magit-diff-hunk-heading
     ((t (:inherit magit-diff-hunk-heading :background ,bg-hl))))
   `(magit-diff-hunk-heading-highlight
     ((t (:inherit magit-diff-hunk-heading-highlight :background ,fg-dim))))
   `(magit-diff-context-highlight
     ((t (:inherit magit-diff-context-highlight :background ,bg-fade))))
   `(magit-section-heading ((t (:inherit variable-pitch :weight bold))))
   `(magit-section-highlight ((t (:background ,bg-fade))))
   `(magit-tag ((t (:foreground ,yellow-1))))
   ;;
   `(marginalia-documentation ((t (:foreground ,fg-subtle :italic t))))
   ;;
   `(markdown-code-face ((t (:inherit org-block))))
   `(markdown-header-face-1
     ((t (:inherit markdown-header-face :height 1.5 :weight bold))))
   `(markdown-header-face-2
     ((t (:inherit markdown-header-face :height 1.3 :weight bold))))
   `(markdown-header-face-3
     ((t (:inherit markdown-header-face :height 1.1 :weight bold))))
   `(markdown-header-delimiter-face ((t (:inherit markdown-header-face))))
   `(markdown-inline-code-face ((t (:inherit org-code))))
   `(markdown-line-break-face ((t (:inherit shadow :underline t))))
   `(markdown-link-face ((t (:inherit shadow :underline t))))
   `(markdown-list-face ((t (:inherit default))))
   `(markdown-markup-face ((t (:inherit shadow))))
   `(markdown-pre-face ((t (:inherit markdown-code-face))))
   ;;
   `(markup-comment-face ((t (:inherit font-lock-comment-face))))
   `(markup-meta-face ((t (:inherit shadow))))
   `(markup-gen-face ((t (:inherit outline-1))))
   `(markup-list-face ((t (:inherit default))))
   `(markup-meta-hide-face ((t (:inherit shadow))))
   `(markup-title-0-face ((t (:inherit markup-gen-face))))
   `(markup-title-1-face ((t (:inherit markup-gen-face))))
   `(markup-title-2-face ((t (:inherit markup-gen-face))))
   `(markup-title-3-face ((t (:inherit markup-gen-face))))
   `(markup-title-4-face ((t (:inherit markup-gen-face))))
   `(markup-title-5-face ((t (:inherit markup-gen-face))))
   `(markup-verbatim-face ((t (:inherit shadow))))
   ;;
   `(mini-modeline-mode-line
     ((t (:inherit (mini-modeline-mode-line variable-pitch) :background ,bg-fade :height 0.15))))
   ;;
   `(orderless-match-face-0 ((t (:inherit bold))))
   `(orderless-match-face-1 ((t (:foreground ,cyan-1 :weight bold))))
   `(orderless-match-face-2 ((t (:foreground ,magenta-1 :weight bold))))
   `(orderless-match-face-3 ((t (:foreground ,blue-1 :weight bold))))
   ;;
   `(pdf-view-rectangle ((t (:inherit calm-border))))
   ;;
   `(popup-face ((t (:foreground ,fg-main :background ,bg-fade))))
   `(popup-tip-face ((t (:inherit popup-face))))
   `(popup-menu-selection-face ((t (:inherit calm-strong))))
   `(popup-menu-mouse-face ((t (:inherit popup-menu-selection-face))))
   ;;
   `(tab-bar-echo-area-tab ((t (:inherit tab-bar-tab))))
   ;;
   `(transient-key-exit ((t (:inherit warning))))
   `(transient-key-return ((t (:inherit success))))
   `(transient-key-stay ((t (:inherit bold))))
   ;;
   `(which-key-key-face ((t (:foreground ,red-1))))
   ;;
   `(vertico-current ((t (:background ,bg-fade :extend t))))
   `(vertico-posframe ((t (:inherit default))))
   `(vertico-posframe-border ((t (:background ,fg-dim))))
   )

  ;; variables
  (custom-theme-set-variables
   theme-name
   `(hl-todo-keyword-faces
     '(("HOLD"  . (:foreground ,black :background ,yellow-2 :weight bold))
       ("TODO"  . (:inherit org-todo))
       ("FIXME" . (:inherit org-todo))
       ("BUG"   . (:inherit org-todo))
       ("FAIL"  . (:inherit org-todo))
       ("DONE"  . (:inherit org-done))
       ("NOTE"  . (:foreground ,black :background ,ok-bg :weight bold))))
   `(org-todo-keyword-faces
     '(("IN PROGRESS" . (:foreground ,black :background ,blue-3))
       ("WAITING"     . (:foreground ,black :background ,yellow-2 :weight bold))
       ("ON HOLD"     . (:foreground ,fg-bold :background ,fg-faint :weight bold))))
   )
)

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'calm-build)

;;; calm-build.el ends here
