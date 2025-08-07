;;; monotropa-theme.el --- Monotropa Theme

;; Copyright 2025-present, all rights reserved.
;;
;; Code licensed under MIT licence.

;; Author: Brendan Desmond
;; Version: 0.1
;; Package-Version: 0
;; Package-Requires: ((emacs "29"))

;;; Commentary:

;; (Almost) monochromatic theme for Emacs. Originally based on maio/eink-emacs.
;; Fork of the Monotropic theme by caffo (https://github.com/caffo/monotropic-theme).
;; This theme looks best with a font that has good italics support. I like `Triplicate A Code`.

;;; Code:

(deftheme monotropa
  "Based on https://github.com/caffo/monotropic-theme")

(window-divider-mode -1)
(global-diff-hl-mode 1)
(diff-hl-margin-mode -1)
(when (and (featurep 'fontaine)
           (boundp 'fontaine-mode))
  (fontaine-set-preset 'paper))

(let ((theme-name 'monotropa)
      (fg "black")
      (bg "#fffffa")
      (bg-light "#ddddda")
      (bg-light-2 "#eaeae5")
      (bg-light-3 "#f0f0eb")
      (bg-light-4 "#f8f8f3")
      (fg-light "#8a8a85")
      (fg-light-2 "#b8b8b3")
      (red "indian red")
      (green "DarkOliveGreen4")
      (blue "SkyBlue4")
      (ui-font-height 0.95))

  (custom-theme-set-faces
   theme-name

   ;; main
   `(default ((t (:background ,bg :foreground ,fg))))
   `(button ((t (:foreground ,fg :underline t))))
   `(completions-common-part ((t (:inherit default :weight bold))))
   `(cursor ((t (:background ,fg :foreground ,bg))))
   `(custom-variable-tag ((t (:foreground ,fg :weight bold))))
   `(default-italic ((t (:italic t))))
   `(error ((t (:underline ,fg-light))))
   `(fringe ((t (:background ,bg :foreground ,bg))))
   `(header-line ((t (:inherit (variable-pitch mode-line) :overline ,fg-light-2))))
   `(highlight ((t (:background ,bg-light-3))))
   `(hl-line ((t (:background ,bg-light-3))))
   `(idle-highlight ((t (:background ,bg-light-2))))
   `(ido-first-match ((t (:foreground ,fg))))
   `(ido-only-match ((t (:foreground ,fg))))
   `(ido-subdir ((t (:foreground ,fg))))
   `(isearch ((t (:background ,fg-light :foreground ,bg))))
   `(lazy-highlight ((t (:background ,bg-light :foreground ,fg))))
   `(line-number ((t (:foreground ,bg-light))))
   `(link ((t (:foreground ,fg :underline t))))
   `(minibuffer-prompt ((t (:foreground ,fg :weight bold))))
   `(mode-line
     ((t (:foreground ,fg
          :background ,bg-light-3
          :overline ,fg-light
          :box (:line-width 1 :style flat-button) :height ,ui-font-height))))
   `(mode-line-buffer-id ((t (:inherit variable-pitch :weight bold))))
   `(mode-line-highlight ((t (:background ,bg-light))))
   `(mode-line-inactive
     ((t (:inherit mode-line
          :foreground ,fg-light
          :background ,bg-light-3
          :overline ,fg-light-2))))
   `(region ((t (:background ,bg-light-2 :foreground ,fg))))
   `(success ((t (:inherit success :foreground ,green :weight bold))))
   `(secondary-selection ((t (:foreground ,bg :background ,fg-light-2))))
   `(shadow ((t (:foreground ,fg-light))))
   `(show-paren-match
     ((t (:foreground ,fg :background ,bg-light :weight bold :underline t))))
   `(show-paren-mismatch
     ((t (:background ,red :foreground ,bg :weight bold :underline t))))
   `(tab-bar ((t (:inherit variable-pitch :foreground ,fg :background ,bg))))
   `(tab-bar-tab ((t (:inherit tab-bar :slant italic))))
   `(tab-bar-tab-inactive ((t (:inherit tab-bar :foreground ,fg-light))))
   `(trailing-whitespace ((t (:background ,red))))
   `(vertical-border ((t (:foreground ,fg-light))))
   `(warning ((t (:underline ,fg-light))))

   ;; built-in syntax
   `(font-latex-bold-face ((t (:foreground ,fg))))
   `(font-latex-italic-face ((t (:foreground ,fg :slant italic))))
   `(font-latex-match-reference-keywords ((t (:foreground ,fg))))
   `(font-latex-match-variable-keywords ((t (:foreground ,fg))))
   `(font-latex-string-face ((t (:foreground ,fg-light))))
   `(font-lock-builtin-face ((t (:foreground ,fg))))
   `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
   `(font-lock-comment-face ((t (:foreground ,fg-light :slant italic))))
   `(font-lock-constant-face ((t (:foreground ,fg))))
   `(font-lock-doc-face ((t (:foreground ,fg))))
   `(font-lock-function-name-face ((t (:foreground ,fg))))
   `(font-lock-keyword-face ((t (:foreground ,fg))))
   `(font-lock-preprocessor-face ((t (:foreground ,fg))))
   `(font-lock-reference-face ((t (:foreground ,fg))))
   `(font-lock-string-face ((t (:foreground ,fg))))
   `(font-lock-type-face ((t (:foreground ,fg))))
   `(font-lock-variable-name-face ((t (:foreground ,fg :underline nil))))
   `(font-lock-warning-face ((t (:foreground ,fg :weight bold))))
   `(sh-heredoc ((t (:foreground ,fg))))
   `(sh-quoted-exec ((t (:foreground ,fg))))
   ;; external syntax
   `(clojure-keyword-face ((t (:inherit font-lock-keyword-face))))
   `(cperl-hash-face ((t (:foreground ,fg))))
   `(cperl-array-face ((t (:foreground ,fg))))
   `(cperl-nonoverridable-face ((t (:foreground ,fg))))
   `(elixir-operator-face ((t (:foreground ,fg :weight bold))))
   `(elixir-negation-face ((t (:foreground ,fg :weight bold))))
   `(elixir-attribute-face ((t (:foreground ,fg :background ,bg :slant italic ))))
   `(elixir-atom-face ((t (:foreground ,fg :background ,bg :weight bold ))))
   `(elixir-ignored-var-face ((t (:foreground ,fg :background ,bg ))))
   `(ess-assignment-face ((t (:inherit bold))))
   `(ess-constant-face ((t (:inherit font-lock-constant-face))))
   `(ess-modifiers-face ((t (:inherit bold))))
   `(ess-%op%-face ((t (:inherit bold))))
   `(js2-function-param ((t (:foreground ,fg))))
   `(js2-error ((t (:inherit error))))
   `(js2-external-variable ((t (:foreground ,fg))))
   `(tuareg-font-double-semicolon-face ((t (:inherit font-lock-keyword-face))))
   `(tuareg-font-lock-governing-face ((t (:inherit font-lock-keyword-face))))
   `(tuareg-font-lock-operator-face ((t (:inherit font-lock-operator-face))))
   `(tuareg-font-lock-doc-verbatim-face ((t (:inherit font-lock-doc-face))))

   ;; org
   `(org-agenda-date ((t (:foreground ,fg :height 1.2))))
   `(org-agenda-date-today ((t (:foreground ,fg :weight bold :height 1.4))))
   `(org-agenda-date-weekend ((t (:foreground ,fg :weight normal))))
   `(org-agenda-structure ((t (:foreground ,fg :weight bold))))
   `(org-block ((t (:foreground ,fg))))
   `(org-block-begin-line ((t (:foreground ,fg-light))))
   `(org-block-end-line ((t (:foreground ,fg-light))))
   `(org-date ((t (:foreground ,fg :underline t))))
   `(org-document-info ((t (:foreground unspecified))))
   `(org-document-title ((t (:inherit bold))))
   `(org-done ((t (:foreground ,fg-light :weight bold))))
   `(org-drawer ((t (:foreground ,fg-light))))
   `(org-headline-done ((t (:foreground ,fg :slant italic))))
   `(org-hide ((t (:foreground ,bg))))
   `(org-level-1 ((t (:foreground ,fg :weight semi-bold :height 1.3))))
   `(org-level-2 ((t (:foreground ,fg :weight semi-bold :height 1.1))))
   `(org-level-3 ((t (:foreground ,fg :weight semi-bold :height 1.1))))
   `(org-level-4 ((t (:foreground ,fg :weight semi-bold :height 1.1))))
   `(org-level-5 ((t (:foreground ,fg :weight semi-bold :height 1.1))))
   `(org-level-6 ((t (:foreground ,fg :weight semi-bold :height 1.1))))
   `(org-link ((t (:foreground ,fg :underline t))))
   `(org-quote ((t (:foreground ,fg :slant italic :inherit org-block))))
   `(org-scheduled ((t (:foreground ,fg))))
   `(org-sexp-date ((t (:foreground ,fg))))
   `(org-special-keyword ((t (:foreground ,fg))))
   `(org-todo ((t (:foreground ,fg :weight bold :slant italic :underline t))))
   `(org-verse ((t (:inherit org-block :slant italic))))
   `(org-table ((t (:foreground ,fg))))
   `(org-table-header ((t (:inherit org-table :foreground ,fg :slant italic))))

   ;; term
   `(term-color-red ((t (:foreground "indianred3"))))
   `(term-color-yellow ((t (:foreground "dark goldenrod"))))
   `(term-color-green ((t (:foreground "DarkSeaGreen4"))))
   `(term-color-blue ((t (:foreground "SkyBlue4"))))
   `(term-color-magenta ((t (:foreground "HotPink3"))))
   `(term-color-cyan ((t (:foreground "cadet blue"))))
   `(term-color-white ((t (:foreground ,bg-light))))
   `(term-color-bright-black ((t (:foreground ,fg-light))))
   `(term-color-bright-red ((t (:foreground "IndianRed2"))))
   `(term-color-bright-yellow ((t (:foreground "burlywood2"))))
   `(term-color-bright-green ((t (:foreground "dark sea green"))))
   `(term-color-bright-blue ((t (:foreground "SkyBlue2"))))
   `(term-color-bright-magenta ((t (:foreground "plum"))))
   `(term-color-bright-cyan ((t (:foreground "PaleTurquoise3"))))
   `(term-color-bright-white ((t (:foreground ,bg))))

   ;; eshell
   `(eshell-ls-directory-face ((t (:foreground ,fg :weight bold))))
   `(eshell-ls-archive-face ((t (:foreground ,fg))))
   `(eshell-ls-backup-face ((t (:foreground ,fg))))
   `(eshell-ls-clutter-face ((t (:foreground ,fg))))
   `(eshell-ls-directory-face ((t (:foreground ,fg))))
   `(eshell-ls-executable-face ((t (:foreground ,fg))))
   `(eshell-ls-missing-face ((t (:foreground ,fg))))
   `(eshell-ls-picture-face ((t (:foreground ,fg))))
   `(eshell-ls-product-face ((t (:foreground ,fg))))
   `(eshell-ls-readonly-face ((t (:foreground ,fg))))
   `(eshell-ls-special-face ((t (:foreground ,fg))))
   `(eshell-ls-symlink-face ((t (:foreground ,fg))))
   `(eshell-ls-text-face ((t (:foreground ,fg))))
   `(eshell-ls-todo-face ((t (:foreground ,fg))))
   `(eshell-ls-unreadable-face ((t (:foreground ,fg))))
   `(eshell-prompt-face ((t (:foreground ,fg))))
   `(eshell-prompt ((t (:foreground ,fg :weight bold))))
   `(eshell-syntax-highlighting-alias-face ((t (:inherit default))))
   `(eshell-syntax-highlighting-invalid-face ((t (:inherit error))))
   `(eshell-syntax-highlighting-shell-builtin-face ((t (:inherit default))))
   `(eshell-syntax-highlighting-shell-command-face ((t (:inherit default))))

   ;; etc
   `(custom-group-tag ((t (:inherit (bold custom-group-tag)))))
   `(custom-variable-tag ((t (:inherit (bold custom-variable-tag)))))
   `(dired-directory ((t (:weight bold))))
   `(eww-valid-certificate ((t (:inherit bold))))
   `(eww-form-textarea ((t (:background ,bg-light-3 :box (:line-width 1 :color ,bg)))))
   `(eww-form-submit ((t (:inherit eww-form-submit :foreground ,fg :background ,bg-light-2 :box (:line-width 2 :style released-button)))))
   `(flyspell-duplicate ((t (:inherit error))))
   `(flyspell-incorrect ((t (:inherit error))))
   `(help-key-binding ((t (:foreground ,fg :background ,bg :slant italic))))
   `(info-header-node ((t (:inherit bold :italic t))))
   `(info-menu-star ((t (:inherit shadow))))
   `(info-node ((t (:inherit bold :italic t))))
   `(link-visited ((t (:inherit link :foreground ,fg-light :underline t))))
   `(gnus-header-content ((t (:foreground ,fg))))
   `(gnus-header-from ((t (:foreground ,fg))))
   `(gnus-header-name ((t (:foreground ,fg))))
   `(gnus-header-subject ((t (:foreground ,fg))))
   `(shr-link ((t (:inherit button))))
   `(which-key-separator-face ((t (:foreground ,fg-light))))

   ;; evil
   `(global-evil-search-highlight-persist ((t (:background ,bg-light))))
   `(evil-ex-search ((t (:background ,bg-light))))
   `(evil-ex-lazy-highlight ((t (:background ,bg-light))))
   `(evil-ex-substitute-matches ((t (:background ,bg-light))))
   `(evil-search-highlight-persist-highlight-face ((t (:background ,bg-light))))

   ;; magit
   `(magit-hash ((t (:foreground ,fg-light :slant italic))))
   `(magit-section-heading ((t (:inherit variable-pitch :weight bold))))
   `(magit-section-highlight ((t (:inherit magit-section-highlight :background ,bg-light-3))))
   `(magit-diff-context-highlight ((t (:inherit magit-section-highlight))))
   `(magit-diff-file-heading ((t (:inherit magit-diff-file-heading :weight normal))))
   `(magit-diff-hunk-heading ((t (:inherit magit-diff-hunk-heading :background ,bg-light))))
   `(magit-branch-local ((t (:weight bold))))
   `(magit-branch-remote ((t (:weight bold))))

   ;; misc. external packages
   `(cider-result-overlay-face ((t (:weight bold))))
   `(corfu-border ((t (:inherit corfu-border :background ,fg-light))))
   `(corfu-current ((t (:inherit corfu-current :background ,bg-light-3))))
   `(corfu-default ((t (:inherit corfu-default :background ,bg))))
   `(diff-hl-margin-change ((t (:foreground ,blue :background unspecified))))
   `(diff-hl-margin-insert ((t (:foreground ,green :background unspecified))))
   `(diff-hl-margin-delete ((t (:foreground ,red :background unspecified))))
   `(flycheck-error ((t (:inherit error))))
   `(flycheck-warning ((t (:inherit warning))))
   `(flycheck-info ((t (:inherit warning))))
   `(highlight-symbol-face ((t (:background ,bg-light))))
   `(idle-highlight ((t (:background ,bg-light))))
   `(markdown-url-face ((t (:inherit markdown-url-face :underline t))))
   `(orderless-match-face-0 ((t (:foreground ,fg :weight bold))))
   `(slime-repl-inputed-output-face ((t (:foreground ,fg))))
   `(transient-key-stay ((t (:foreground ,fg-light))))
   `(transient-key-exit ((t (:inherit bold))))
   `(yas-field-highlight-face ((t (:background ,bg-light-2 :foreground ,fg))))
   )

  (custom-theme-set-variables
   theme-name
   `(hl-todo-keyword-faces
     '(("HOLD"  . (:inherit org-todo))
       ("TODO"  . (:inherit org-todo))
       ("FIXME" . (:inherit org-todo))
       ("BUG"   . (:inherit org-todo))
       ("FAIL"  . (:inherit org-todo))
       ("DONE"  . (:inherit org-done))
       ("NOTE"  . (:inherit org-todo))))
   `(org-todo-keyword-faces
     '(("IN PROGRESS" . (:inherit org-todo))
       ("WAITING"     . (:inherit org-todo))
       ("ON HOLD"     . (:inherit org-todo)))))
  )

;;;###autoload
(when load-file-name
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'monotropa)
;;; monotropa-theme.el ends here
