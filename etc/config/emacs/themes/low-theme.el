;;; low-theme.el --- Low theme

;; Copyright (C) 2022 Brendan Desmond

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

;; A minimal calm theme with clear and simple UI.  Syntax highlighting is used
;; only for comments, docstrings, strings, and escape sequences.  Markup
;; languages receive highlighting for things such as code snippets, tables, etc.

;;; Code:

(deftheme low
  "Calm and minimal theme with clear UI (light version).")

(defgroup low '()
  "Faces and colors for the low theme.")

;;;; custom faces
(defface low-faded nil
  "Face for faded foreground text."
  :group 'low)

(defface low-subtle nil
  "Face for subtle foreground color."
  :group 'low)

(defface low-border-ui nil
  "Face for border lines around UI elements."
  :group 'low)

(defface low-critical nil
  "Face for critical information such as errors."
  :group 'low)

(defface low-menu-bg nil
  "Face for popup menu background."
  :group 'low)

(defface low-menu-sel nil
  "Face for popup menu selection."
  :group 'low)

(defface low-salient nil
  "Face for important information."
  :group 'low)

(defface org-list-bullet nil
  "Face for bullet of plain list."
  :group 'org)

;; (font-lock-add-keywords 'org-mode
;;  '(("^\\([0-9]+[.)]\\|\\+\\|\\-\\) " 1 'org-list-bullet)) 'append)
(font-lock-add-keywords 'org-mode
                        '(("^\\([0-9]+[.)]\\|\\+\\|\\-\\) " . 'org-list-bullet)))

;;;; ui
(add-to-list 'default-frame-alist '(ns-appearance . light))
(add-to-list 'default-frame-alist '(internal-border-width . 20))
(dolist (sym '(left-curly-arrow right-curly-arrow))
  (set-fringe-bitmap-face sym 'low-faded))

(setq window-divider-default-right-width 3)
(setq window-divider-default-bottom-width 3)
(setq window-divider-default-places 'right-only)
(window-divider-mode 1)
;; (setq x-underline-at-descent-line nil)

(let*
    ((base-0  "#ffffff")
     (base-1  "#f8f8f8")
     (base-2  "#f0f0f0")
     (base-3  "#e8e8e8")
     (base-4  "#d8d8d8")
     (base-5  "#b8b8b8")
     (base-6  "#a8a8a8")
     (base-7  "#000000")

     (gray-1     base-6)
     (gray-2     base-3)
     (black      "#000000")
     (white      "#ffffff")
     (red-1      "#cc6066")
     (red-2      "#ee9098")
     (green-1    "#739e5c")
     (green-2    "#b9cdb8")
     (yellow-1   "#bba277")
     (yellow-2   "#f4dbae")
     (blue-1     "#5577cc")
     (blue-2     "#aaccee")
     (magenta-1  "#aa66bb")
     (magenta-2  "#e8b8ea")
     (cyan-1     "#55aaaa")
     (cyan-2     "#bbd8d8")
     (cyan-3     "#ddeeee")

     (slate      "#73809f")
     (brown      "#957f5f")
     (steel      "#444a58")
     (maroon     "#8c5d7b")
     (aqua       "#5f8c7d")
     (leaf       "#81895d")
     (umber      "#bf9077")
     (match-1    yellow-2)
     (match-2    cyan-2)
     (button     blue-1)
     (salient    cyan-1))

  (custom-theme-set-faces
   'low

   ;;;; main faces from which the rest are derived
   `(default         ((t (:foreground ,base-7 :background ,base-0))))
   `(highlight       ((t (:background ,base-2))))
   `(match           ((t (:foreground ,black :background ,match-2))))
   `(link            ((t (:foreground ,button :underline t))))
   `(lazy-highlight  ((t (:foreground ,black :background ,match-1))))
   `(org-code        ((t (:foreground ,salient :background ,base-1))))
   `(region          ((t (:background ,base-3))))
   `(shadow          ((t (:foreground ,base-6))))
   `(success         ((t (:foreground ,salient :weight bold))))
   `(warning         ((t (:foreground ,red-1 :weight bold))))
   `(low-border-ui   ((t (:foreground ,base-5 :background nil))))
   `(low-critical    ((t (:foreground ,base-0 :background ,red-2))))
   `(low-faded       ((t (:foreground ,base-4))))
   `(low-menu-bg     ((t (:foreground ,black :background ,base-2))))
   `(low-menu-sel    ((t (:foreground ,base-7 :background ,base-4))))
   `(low-salient     ((t (:foreground ,salient))))
   `(low-subtle      ((t (:background ,base-1))))
   `(window-divider  ((t (:foreground ,base-2))))

   ;;;; syntax on
   `(font-lock-builtin-face        ((t (:inherit font-lock-keyword-face))))
   `(font-lock-comment-face        ((t (:foreground ,base-6))))
   `(font-lock-constant-face       ((t (:foreground ,aqua))))
   `(font-lock-function-name-face  ((t (:foreground ,maroon))))
   `(font-lock-keyword-face        ((t (:foreground ,slate))))
   `(font-lock-string-face         ((t (:foreground ,leaf))))
   `(font-lock-type-face           ((t (:foreground ,brown))))
   `(font-lock-variable-name-face  ((t (:foreground ,steel))))
   `(sh-quoted-exec                ((t (:inherit font-lock-string-face))))
   `(trailing-whitespace           ((t (:inherit low-critical))))

   ;;;; syntax off
   ;; `(font-lock-builtin-face      ((t nil)))
   `(font-lock-preprocessor-face ((t nil)))

   ;;;; editor and ui
   `(error                      ((t (:inherit warning))))
   `(fixed-pitch                ((t (:inherit default))))
   `(fringe                     ((t (:inherit default))))
   `(hl-line                    ((t (:inherit highlight))))
   `(highlight-indentation-face ((t (:inherit highlight))))
   `(isearch                    ((t (:inherit match))))
   `(linum                      ((t (:inherit low-faded))))
   `(line-number                ((t (:inherit low-faded))))
   `(line-number-current-line   ((t (:inherit highlight))))
   `(minibuffer-prompt          ((t (:inherit low-salient :weight bold))))
   `(secondary-selection        ((t (:inherit shadow :inverse-video t))))
   `(show-paren-match           ((t (:inherit region :underline t))))
   `(tab-bar                    ((t (:inherit (tab-bar variable-pitch)
                                     :foreground ,base-7
                                     :background ,base-2
                                     :height 130
                                     :overline ,base-5
                                     :underline nil
                                     :box (:line-width 5 :color ,base-2)))))
   `(tab-bar-tab                ((t (:inherit tab-bar
                                     :foreground ,base-7
                                     :background ,base-0
                                     :overline ,base-5
                                     :underline nil
                                     :box (:line-width 5 :color ,base-0)))))
   `(tab-bar-tab-inactive       ((t (:inherit tab-bar :foreground ,base-7
                                     :underline nil))))
   `(tab-line                   ((t (:inherit tab-bar))))
   `(tab-line-highlight         ((t (:foreground ,base-7 :background ,base-3
                                     :box (:line-width 5 :color ,base-3)))))
   `(tab-line-close-highlight   ((t (:inherit tab-line-highlight :background ,base-4))))
   `(tab-line-tab               ((t (:inherit tab-bar-tab))))
   `(tab-line-tab-current       ((t (:inherit tab-bar-tab))))
   `(tab-line-tab-modified      ((t (:inherit low-salient))))
   `(tab-line-tab-inactive      ((t (:inherit tab-bar-tab-inactive))))
   `(vertical-border            ((t (:inherit low-border-ui))))
   `(window-divider-first-pixel ((t (:foreground ,base-2))))
   `(window-divider-last-pixel  ((t (:inherit window-divider-first-pixel))))

   ;;;; header and mode-line: requires `window-divider-mode`
   `(mode-line           ((t (:inherit (mode-line variable-pitch)
                              :height 130
                              :foreground ,base-7
                              :background ,base-2
                              :overline nil
                              :box (:line-width 5 :color ,base-2)))))
   `(mode-line-inactive  ((t (:inherit mode-line
                              :height 130
                              :foreground ,base-6
                              :background ,base-2))))
   `(mode-line-highlight ((t (:foreground ,base-7 :background ,base-4
                              :box (:line-width 5 :color ,base-4)))))
   `(header-line         ((t (:inherit mode-line))))

   ;;;; outline-mode and org-mode
   `(outline-1             ((t (:inherit bold))))
   `(outline-2             ((t (:inherit outline-1))))
   `(outline-3             ((t (:inherit outline-1))))
   `(outline-4             ((t (:inherit outline-1))))
   `(outline-5             ((t (:inherit outline-1))))
   `(outline-6             ((t (:inherit outline-1))))
   `(outline-7             ((t (:inherit outline-1))))
   `(outline-8             ((t (:inherit outline-1))))
   `(org-agenda-structure  ((t (:inherit bold))))
   `(org-block             ((t (:inherit low-subtle))))
   `(org-block-begin-line  ((t (:inherit shadow))))
   `(org-block-end-line    ((t (:inherit org-block-begin-line))))
   `(org-date              ((t (:inherit link))))
   `(org-document-info     ((t (:inherit default))))
   `(org-document-title    ((t (:inherit default))))
   `(org-drawer            ((t (:inherit shadow))))
   `(org-formula           ((t (:inherit (org-formula org-code)))))
   `(org-headline-done     ((t (:inherit outline-1))))
   `(org-hide              ((t (:inherit low-faded))))
   `(org-list-bullet       ((t (:inherit bold))))
   `(org-list-dt           ((t (:inherit bold))))
   `(org-meta-line         ((t (:inherit font-lock-comment-face))))
   `(org-quote             ((t (:inherit org-block :italic t))))
   `(org-table             ((t (:inherit org-block))))
   `(org-table-header      ((t (:inherit highlight :underline t))))
   `(org-todo              ((t (:inherit warning))))
   `(org-done              ((t (:inherit success))))
   `(org-verbatim          ((t (:inherit org-code))))

   ;;;; etc
   `(dired-directory        ((t (:inherit bold))))
   `(dired-broken-symlink   ((t (:inherit warning :inverse-video t))))
   `(eshell-ls-directory    ((t (:inherit default))))
   `(eshell-ls-symlink      ((t (:inherit default))))
   `(eshell-prompt          ((t (:inherit shadow))))
   `(eww-form-submit        ((t (:inherit link))))
   `(eww-form-text          ((t (:inherit low-subtle :box (:line-width 1 :color ,base-6)))))
   `(eww-form-textarea      ((t (:inherit eww-form-text))))
   `(eww-valid-certificate  ((t (:inherit success))))
   `(gnus-header-name       ((t (:inherit shadow))))
   `(gnus-header-content    ((t (:inherit default))))
   `(gnus-signature         ((t (:inherit default))))
   `(help-key-binding       ((t (:inherit org-code))))
   `(Info-quoted            ((t (:inherit (fixed-pitch-serif org-code)))))
   `(term-color-black       ((t (:foreground ,black     :background ,gray-1))))
   `(term-color-blue        ((t (:foreground ,blue-1    :background ,blue-2))))
   `(term-color-cyan        ((t (:foreground ,cyan-1    :background ,cyan-2))))
   `(term-color-green       ((t (:foreground ,green-1   :background ,green-2))))
   `(term-color-magenta     ((t (:foreground ,magenta-1 :background ,magenta-2))))
   `(term-color-red         ((t (:foreground ,red-1     :background ,red-2))))
   `(term-color-white       ((t (:foreground ,base-3    :background ,white))))
   `(term-color-yellow      ((t (:foreground ,yellow-1  :background ,yellow-2))))

   ;;;; third-party
   `(clojure-keyword-face              ((t (:inherit default))))
   `(corfu-border                      ((t (:background ,base-6))))
   `(corfu-current                     ((t (:inherit low-menu-sel))))
   `(corfu-default                     ((t (:inherit low-menu-bg :foreground ,base-7))))
   `(elfeed-search-date-face           ((t (:inherit shadow))))
   `(elfeed-search-unread-title-face   ((t (:inherit bold))))
   `(elfeed-search-feed-face           ((t (:inherit shadow))))
   `(elfeed-search-tag-face            ((t (:inherit shadow))))
   `(ess-assignment-face               ((t (:inherit bold))))
   `(ess-constant-face                 ((t (:inherit font-lock-constant-face))))
   `(ess-modifiers-face                ((t (:inherit bold))))
   `(ess-%op%-face                     ((t (:inherit bold))))
   `(eyebrowse-mode-line-active        ((t (:inherit bold :underline t))))
   `(evil-ex-substitute-matches        ((t (:inherit match))))
   `(evil-ex-substitute-replacement    ((t (:inherit lazy-highlight :italic t))))
   `(font-latex-bold-face              ((t (:inherit bold))))
   `(font-latex-sedate-face            ((t (:inherit shadow))))
   `(magit-hash                        ((t (:inherit shadow))))
   `(magit-section-heading             ((t (:inherit bold))))
   `(magit-section-highlight           ((t (:inherit highlight))))
   `(markdown-code-face                ((t (:inherit org-block))))
   `(markdown-header-delimiter-face    ((t (:inherit markdown-header-face))))
   `(markdown-inline-code-face         ((t (:inherit org-code))))
   `(markdown-line-break-face          ((t (:inherit shadow :underline t))))
   `(markdown-link-face                ((t (:inherit shadow :underline t))))
   `(markdown-list-face                ((t (:inherit default))))
   `(markdown-markup-face              ((t (:inherit shadow))))
   `(markdown-pre-face                 ((t (:inherit markdown-code-face))))
   `(markup-comment-face               ((t (:inherit shadow))))
   `(markup-meta-face                  ((t (:inherit shadow))))
   `(markup-gen-face                   ((t (:inherit outline-1))))
   `(markup-list-face                  ((t (:inherit default))))
   `(markup-meta-hide-face             ((t (:inherit shadow))))
   `(markup-title-0-face               ((t (:inherit markup-gen-face))))
   `(markup-title-1-face               ((t (:inherit markup-gen-face))))
   `(markup-title-2-face               ((t (:inherit markup-gen-face))))
   `(markup-title-3-face               ((t (:inherit markup-gen-face))))
   `(markup-title-4-face               ((t (:inherit markup-gen-face))))
   `(markup-title-5-face               ((t (:inherit markup-gen-face))))
   `(markup-verbatim-face              ((t (:inherit shadow))))
   `(mini-modeline-mode-line           ((t (:inherit (mini-modeline-mode-line variable-pitch)
                                                     :background ,base-2 :height 0.15))))
   `(orderless-match-face-0            ((t (:foreground ,salient :weight bold))))
   `(popup-face                        ((t (:inherit menu-text))))
   `(popup-tip-face                    ((t (:inherit popup-face))))
   `(popup-menu-selection-face         ((t (:inherit low-menu-sel))))
   `(popup-menu-mouse-face             ((t (:inherit popup-menu-selection-face))))
   `(tab-bar-echo-area-tab             ((t (:inherit variable-pitch :foreground ,base-7 :weight bold))))
   `(tuareg-font-lock-governing-face   ((t (:inherit font-lock-keyword-face))))
   `(tuareg-font-lock-operator-face    ((t (:inherit font-lock-keyword-face))))
   `(vertico-current                   ((t (:inherit hl-line))))
   )

  ;;;; variables
  (custom-theme-set-variables
   'low
   `(ansi-color-names-vector
     [(,black . ,gray-1)
      (,red-1 . ,red-2)
      (,green-1 . ,green-2)
      (,yellow-1 . ,yellow-2)
      (,blue-1 . ,blue-2)
      (,magenta-1 . ,magenta-2)
      (,cyan-1 . ,cyan-2)
      (,gray-2 . ,white)])
   `(hl-todo-keyword-faces '(("HOLD" . ,base-6)
                             ("TODO" . ,red-1)
                             ("FAIL" . ,red-1)
                             ("BUG" . ,red-1)
                             ("DONE" . ,salient)
                             ("NOTE" . ,salient)))
   `(org-todo-keyword-faces '(("IN PROGRESS" . ,blue-1)
                              ("WAITING" . ,yellow-1)
                              ("ON HOLD" . ,base-6)))
   `(zoom-window-mode-line-color ,red-2))
  )

(provide 'low-theme)
(provide-theme 'low)

;;; low-theme.el ends here
