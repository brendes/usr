;;; acme-theme.el --- Acme Theme -*- no-byte-compile: t; -*-

;;; Code:

;;;; faces
(deftheme acme
  "Dark red theme for low light.")

(defgroup acme-faces ()
  "Group for faces defined by acme-theme."
  :group 'faces
  :prefix "acme-")

(defface subtle nil
  "Text with subtle background."
  :group 'acme-faces)

(defface menu-sel nil
  "Menu selection face."
  :group 'acme-faces)

(defface faint nil
  "Faint foreground color face."
  :group 'acme-faces)

;;;; ui
(setq window-divider-default-places 'bottom-only)
(window-divider-mode 1)
(diff-hl-margin-mode -1)
(global-diff-hl-mode 1)

(let* ((base-0 "#ffffea")
       (base-1 "#f8f8e3")
       (base-2 "#f2f2dd")
       (base-3 "#e8e8d3")
       (base-4 "#888880")
       (red-1 "#bb5d5d")
       (red-2 "#ffa0aa")
       (yellow-1 "#99994c")
       (yellow-2 "#eeee9e")
       (green-1 "#448844")
       (green-2 "#aaddaa")
       (green-3 "#eaffea")
       (blue-1 "#4466bb")
       (blue-2 "#aabbff")
       (magenta-1 "#bb6699")
       (magenta-2 "#dd99cc")
       (purple-1 "#8888cc")
       (purple-2 "#bbbbee")
       (cyan-1 "#66aaaa")
       (cyan-2 "#aadddd")
       (cyan-3 "#9eeeee")
       (cyan-4 "#eaffff")
       (gray "#888888")
       (white "#ffffff")
       (black "#000000")
       (ui-text-height 0.9)
       ;;
       (fg black)
       (bg base-0)
       (bg-ui cyan-4)
       (bg-ui-hl cyan-3)
       (shadow base-4)
       (border gray))

  ;;;; core theme definition
  (custom-theme-set-faces
   'acme

   ;; editor and ui
   `(default         ((t (:foreground ,fg :background ,base-0))))
   `(bold            ((t (:foreground ,fg :weight bold))))
   `(cursor          ((t (:foreground ,base-0 :background ,fg))))
   `(faint           ((t (:foreground ,base-3))))
   `(shadow          ((t (:foreground ,base-4))))
   `(subtle          ((t (:foreground ,fg :background ,base-1))))
   `(success         ((t (:foreground ,green-1 :weight bold))))
   `(warning         ((t (:foreground ,red-1 :weight bold))))
   ;;
   `(completions-common-part    ((t (:inherit bold))))
   `(error                      ((t (:inherit warning))))
   `(fixed-pitch                ((t (:inherit default))))
   `(fringe                     ((t (:inherit default))))
   `(highlight                  ((t (:foreground ,fg :background ,base-3))))
   `(highlight-indentation-face ((t (:inherit subtle))))
   `(hl-line                    ((t (:foreground unspecified :background ,base-2))))
   `(isearch                    ((t (:inherit match :underline t))))
   `(isearch-fail               ((t (:foreground ,white :background ,red-1))))
   `(lazy-highlight             ((t (:foreground ,fg :background ,yellow-2))))
   `(link                       ((t (:foreground ,blue-1 :underline t))))
   `(link-visited               ((t (:inherit link :foreground ,base-4))))
   `(linum                      ((t (:inherit faint))))
   `(line-number                ((t (:inherit faint))))
   `(line-number-current-line   ((t (:inherit line-number :foreground ,fg))))
   `(match                      ((t (:foreground ,fg :background ,yellow-2))))
   `(menu-sel                   ((t (:foreground ,fg :background ,green-2))))
   `(minibuffer-prompt          ((t (:foreground ,fg :weight bold))))
   `(region                     ((t (:background ,yellow-2))))
   `(secondary-selection        ((t (:foreground ,yellow-1 :inverse-video t))))
   `(show-paren-match           ((t (:background ,base-3 :underline t))))
   `(show-paren-mismatch        ((t (:foreground ,fg :background ,red-2 :underline t))))
   `(vertical-border            ((t (:foreground ,border))))
   `(window-divider             ((t (:foreground ,border))))
   `(window-divider-first-pixel ((t (:foreground ,border))))
   `(window-divider-last-pixel  ((t (:inherit window-divider-first-pixel))))
   ;; header and mode-line
   `(mode-line
     ((t (:inherit mode-line
          :height ,ui-text-height
          :foreground ,fg
          :background ,bg-ui
          :overline ,border
          :box (:line-width 2 :style flat-button)))))
   `(mode-line-buffer-id ((t (:inherit variable-pitch :weight bold))))
   `(mode-line-inactive  ((t (:inherit mode-line :foreground ,gray))))
   `(mode-line-highlight
     ((t (:foreground ,fg :background ,bg-ui-hl :box (:line-width 2 :style flat-button)))))
   `(header-line         ((t (:inherit mode-line))))
   ;; tab-line
   `(tab-line
     ((t (:inherit (variable-pitch tab-line)
                   :height ,ui-text-height
                   :foreground ,fg :background ,bg-ui
                   :overline ,border
                   :box (:line-width 5 :style flat-button)))))
   `(tab-line-highlight         ((t (:foreground ,fg :background ,bg-ui-hl
                                     :box (:line-width 5 :style flat-button)))))
   `(tab-line-close-highlight   ((t (:inherit tab-line-highlight))))
   `(tab-line-tab               ((t (:inherit tab-line-tab))))
   `(tab-line-tab-current       ((t (:inherit tab-line-tab))))
   `(tab-line-tab-modified      ((t (:foreground ,fg))))
   `(tab-line-tab-inactive      ((t (:inherit tab-line-tab :foreground ,shadow))))
   ;; tab-bar
   `(tab-bar ((t (:inherit variable-pitch :foreground ,fg :height ,ui-text-height))))
   `(tab-bar-tab ((t (:inherit tab-bar :weight bold))))
   `(tab-bar-tab-inactive ((t (:inherit tab-bar :foreground ,shadow))))

   ;; syntax
   `(escape-glyph                        ((t (:foreground ,red-1))))
   `(font-lock-builtin-face              ((t (:inherit font-lock-keyword-face))))
   `(font-lock-comment-face              ((t (:inherit shadow))))
   `(font-lock-constant-face             ((t nil)))
   `(font-lock-doc-face                  ((t (:inherit font-lock-string-face))))
   `(font-lock-escape-face               ((t (:inherit shadow))))
   `(font-lock-function-call-face        ((t nil)))
   `(font-lock-function-name-face        ((t nil)))
   `(font-lock-keyword-face              ((t nil)))
   `(font-lock-negation-char-face        ((t (:inherit font-lock-keyword-face))))
   `(font-lock-number-face               ((t (:inherit font-lock-constant-face))))
   `(font-lock-operator-face             ((t (:inherit font-lock-keyword-face))))
   `(font-lock-preprocessor-face         ((t nil)))
   `(font-lock-punctuation-face          ((t nil)))
   `(font-lock-regexp-grouping-construct ((t nil)))
   `(font-lock-string-face               ((t nil)))
   `(font-lock-type-face                 ((t nil)))
   `(font-lock-variable-name-face        ((t nil)))
   `(font-lock-variable-use-face         ((t nil)))
   `(font-lock-warning-face              ((t (:inherit warning))))
   `(sh-heredoc                          ((t nil)))
   `(sh-quoted-exec                      ((t nil)))
   `(trailing-whitespace                 ((t (:background ,red-2))))

   ;; outline-mode and org-mode
   `(outline-1             ((t (:inherit bold))))
   `(outline-2             ((t (:inherit outline-1))))
   `(outline-3             ((t (:inherit outline-1))))
   `(outline-4             ((t (:inherit outline-1))))
   `(outline-5             ((t (:inherit outline-1))))
   `(outline-6             ((t (:inherit outline-1))))
   `(outline-7             ((t (:inherit outline-1))))
   `(outline-8             ((t (:inherit outline-1))))
   `(org-agenda-structure  ((t (:inherit bold))))
   `(org-block             ((t (:inherit subtle))))
   `(org-block-begin-line  ((t (:inherit shadow :background ,base-2 :extend t))))
   `(org-block-end-line    ((t (:inherit org-block-begin-line))))
   `(org-code              ((t (:foreground ,fg :background ,base-1))))
   `(org-date              ((t (:inherit link))))
   `(org-document-info     ((t (:inherit default))))
   `(org-document-title    ((t (:inherit default))))
   `(org-drawer            ((t (:inherit shadow))))
   `(org-formula           ((t (:inherit (org-formula org-code)))))
   `(org-headline-done     ((t (:inherit outline-1))))
   `(org-hide              ((t (:foreground ,base-0))))
   `(org-list-bullet       ((t (:inherit bold))))
   `(org-list-dt           ((t (:inherit bold))))
   `(org-meta-line         ((t (:inherit font-lock-comment-face))))
   `(org-quote             ((t (:inherit org-block :italic t))))
   `(org-table             ((t (:inherit org-block))))
   `(org-table-header      ((t (:inherit highlight :underline t))))
   `(org-todo              ((t (:foreground ,fg :background ,red-2 :weight bold))))
   `(org-done              ((t (:foreground ,fg :background ,green-2 :weight bold))))
   `(org-verbatim          ((t (:inherit org-code))))

   ;; etc
   `(dired-directory            ((t (:inherit default))))
   `(dired-broken-symlink       ((t (:inherit warning :inverse-video t))))
   `(eshell-ls-directory        ((t (:inherit default))))
   `(eshell-ls-symlink          ((t (:inherit default))))
   `(eshell-prompt              ((t (:inherit shadow))))
   `(eww-form-submit            ((t (:inherit link))))
   `(eww-form-text              ((t (:inherit subtle :box (:line-width 1 :color ,base-4)))))
   `(eww-form-textarea          ((t (:inherit subtle))))
   `(eww-valid-certificate      ((t (:foreground ,fg))))
   `(gnus-header-name           ((t (:inherit shadow))))
   `(gnus-header-content        ((t (:inherit default))))
   `(gnus-signature             ((t (:inherit default))))
   `(help-key-binding           ((t (:inherit org-code))))
   `(Info-quoted                ((t (:inherit (fixed-pitch-serif org-code)))))
   `(term-color-black           ((t (:foreground ,fg))))
   `(term-color-white           ((t (:inherit faint))))
   `(term-color-bright-black    ((t (:inherit shadow))))
   `(term-color-bright-white    ((t (:foreground ,white))))
   `(term-color-blue            ((t (:foreground ,blue-1))))
   `(term-color-cyan            ((t (:foreground ,cyan-1))))
   `(term-color-green           ((t (:foreground ,green-1))))
   `(term-color-magenta         ((t (:foreground ,magenta-1))))
   `(term-color-red             ((t (:foreground ,red-1))))
   `(term-color-yellow          ((t (:foreground ,yellow-1))))
   `(term-color-bright-blue     ((t (:foreground ,blue-2))))
   `(term-color-bright-cyan     ((t (:foreground ,cyan-3))))
   `(term-color-bright-green    ((t (:foreground ,green-2))))
   `(term-color-bright-magenta  ((t (:foreground ,magenta-2))))
   `(term-color-bright-red      ((t (:foreground ,red-2))))
   `(term-color-bright-yellow   ((t (:foreground ,yellow-2))))
   `(whitespace-newline         ((t (:foreground ,red-2))))
   `(whitespace-space           ((t (:inherit shadow))))
   `(whitespace-tab             ((t (:foreground ,blue-2))))

   ;;;; third-party
   `(clojure-keyword-face ((t (:inherit font-lock-keyword-face))))
   `(tuareg-font-double-semicolon-face ((t (:inherit font-lock-keyword-face))))
   `(tuareg-font-lock-governing-face ((t (:inherit font-lock-keyword-face))))
   `(tuareg-font-lock-operator-face ((t (:inherit font-lock-operator-face))))
   `(tuareg-font-lock-doc-verbatim-face ((t (:inherit font-lock-doc-face))))
   `(clojure-keyword-face              ((t nil)))
   ;; `(diff-hl-change                    ((t (:foreground ,base-6 :background ,bg))))
   ;; `(diff-hl-delete                    ((t (:inherit diff-hl-change))))
   ;; `(diff-hl-insert                    ((t (:inherit diff-hl-change))))
   `(corfu-border                      ((t (:background ,green-1))))
   `(corfu-current                     ((t (:inherit menu-sel))))
   `(corfu-default                     ((t (:foreground ,fg :background ,green-3))))
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
   ;;
   `(magit-branch-local ((t (:inherit bold))))
   `(magit-branch-remote ((t (:inherit bold))))
   `(magit-hash ((t (:foreground ,gray))))
   `(magit-diff-hunk-heading
     ((t (:inherit magit-diff-hunk-heading :background ,base-2))))
   `(magit-diff-hunk-heading-highlight
     ((t (:inherit magit-diff-hunk-heading-highlight :background ,base-3))))
   `(magit-diff-context-highlight
     ((t (:inherit magit-diff-context-highlight :background ,base-2))))
   `(magit-section-heading ((t (:inherit variable-pitch :weight bold))))
   `(magit-section-highlight ((t (:background ,base-2))))
   `(magit-tag ((t (:foreground ,yellow-1))))
   ;;
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
   `(mini-modeline-mode-line           ((t (:inherit(mini-modeline-mode-line variable-pitch) :background ,cyan-4 :height 0.15))))
   `(orderless-match-face-0            ((t (:inherit completions-common-part))))
   `(orderless-match-face-1            ((t (:inherit orderless-match-face-0))))
   `(orderless-match-face-2            ((t (:inherit orderless-match-face-0))))
   `(orderless-match-face-3            ((t (:inherit orderless-match-face-0))))
   `(popup-tip-face                    ((t (:inherit subtle))))
   `(popup-menu-mouse-face             ((t (:inherit menu-sel))))
   `(tab-bar-echo-area-tab             ((t (:inherit (variable-pitch bold)))))
   `(transient-key                     ((t (:inherit bold))))
   `(transient-key-exit                ((t (:inherit bold))))
   `(transient-key-stay                ((t (:inherit bold))))
   `(transient-key-return              ((t (:inherit bold))))
   `(tuareg-font-lock-governing-face   ((t (:inherit font-lock-keyword-face))))
   `(tuareg-font-lock-operator-face    ((t (:inherit font-lock-keyword-face))))
   `(vertico-current                   ((t (:inherit hl-line))))
   )

  ;; variables
  (custom-theme-set-variables
   'acme
   `(hl-todo-keyword-faces
     '(("HOLD"  . (:foreground ,fg :background ,yellow-2 :weight bold))
       ("TODO"  . (:inherit org-todo))
       ("FIXME" . (:inherit org-todo))
       ("BUG"   . (:inherit org-todo))
       ("FAIL"  . (:inherit org-todo))
       ("DONE"  . (:inherit org-done))
       ("NOTE"  . (:foreground ,fg :background ,purple-2 :weight bold))))
   `(org-todo-keyword-faces
     '(("IN PROGRESS" . (:foreground ,fg :background ,purple-2))
       ("WAITING"     . (:foreground ,fg :background ,yellow-2 :weight bold))
       ("ON HOLD"     . (:foreground ,fg :background ,base-3 :weight bold))))
   `(zoom-window-mode-line-color ,cyan-3)
     )
  )

(provide 'acme-theme)
(provide-theme 'acme)

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;;; acme-theme.el ends here
