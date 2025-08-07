(deftheme b
  "Simple theme with colorful UI."
  :background-mode 'light
  :kind 'color-scheme)

(defgroup b-faces ()
  "Group for faces defined by b-theme."
  :group 'faces
  :prefix "b-")

(defface subtle nil
  "Text with subtle background."
  :group 'b-faces)

(defface menu-sel nil
  "Menu selection face."
  :group 'b-faces)

(defface faint nil
  "Faint foreground color face."
  :group 'b-faces)

;;;; ui
(setq default-frame-alist
      (append default-frame-alist
              `((ns-transparent-titlebar . t)
                (ns-appearance . 'light))))
(add-to-list 'default-frame-alist '(internal-border-width . 20))
(dolist (sym '(left-curly-arrow right-curly-arrow))
  (set-fringe-bitmap-face sym 'shadow))

(setq window-divider-default-right-width 1)
(setq window-divider-default-bottom-width 1)
(setq window-divider-default-places t)
(window-divider-mode 1)
(diff-hl-margin-mode -1)

(let*
    ((base-0 "#ffffff")
     (base-1 "#fafafa")
     (base-2 "#f0f1f2")
     (base-3 "#e8eaec")
     (base-4 "#989b9e")
     (red-1 "#cc585d")
     (red-2 "#eeaaaa")
     (orange-1 "#d08868")
     (orange-2 "#fbb47b")
     (yellow-1 "#997744")
     (yellow-2 "#eeccaa")
     (yellow-3 "#f8d8b8")
     (green-1 "#668822")
     (green-2 "#aacc88")
     (green-3 "#eaffdd")
     (blue-1 "#5566bb")
     (blue-2 "#afbfff")
     (blue-3 "#ccdbff")
     (blue-4 "#e0e8f0")
     (blue-5 "#f0f4ff")
     (magenta-1 "#bb5577")
     (magenta-2 "#dd99cc")
     (magenta-3 "#eeb8dd")
     (purple-1 "#8888cc")
     (purple-2 "#bbbbee")
     (cyan-1 "#66aaaa")
     (cyan-2 "#aadddd")
     (cyan-3 "#bbebeb")
     (cyan-4 "#d0e8e8")
     (gray "#808080")
     (white "#ffffff")
     (fg "#000000")
     (blue-muted "#7788aa")
     (cyan-muted "#779999")
     (magenta-muted "#aa6688")
     (yellow-muted "#997766")
     )

  (custom-theme-set-faces
   'b

   ;;;; main faces from which the rest are derived
   `(default         ((t (:foreground ,fg :background ,base-0))))
   `(bold            ((t (:foreground ,fg :weight bold))))
   `(cursor          ((t (:foreground ,base-0 :background ,fg))))
   `(faint           ((t (:foreground ,base-3))))
   `(highlight       ((t (:foreground ,fg :background ,blue-4))))
   `(match           ((t (:foreground ,fg :background ,orange-2))))
   `(link            ((t (:foreground ,blue-muted :underline t))))
   `(lazy-highlight  ((t (:foreground ,fg :background ,yellow-3))))
   `(menu-sel        ((t (:foreground ,fg :background ,blue-3))))
   `(org-code        ((t (:foreground ,magenta-1 :background ,base-2))))
   `(org-todo        ((t (:foreground ,fg :background ,red-2 :weight bold))))
   `(region          ((t (:background ,base-3))))
   `(shadow          ((t (:foreground ,base-4))))
   `(subtle          ((t (:foreground ,fg :background ,base-1))))
   `(success         ((t (:inherit bold :foreground ,cyan-1))))
   `(vertical-border ((t (:foreground ,gray))))
   `(warning         ((t (:inherit bold :foreground ,red-1))))

   ;;;; syntax
   `(font-lock-builtin-face        ((t (:inherit font-lock-keyword-face))))
   `(font-lock-comment-face        ((t (:inherit shadow))))
   `(font-lock-constant-face       ((t (:foreground ,magenta-muted))))
   `(font-lock-function-name-face  ((t nil)))
   `(font-lock-regexp-face         ((t (:foreground ,yellow-muted))))
   `(font-lock-keyword-face        ((t nil)))
   `(font-lock-preprocessor-face   ((t nil)))
   `(font-lock-string-face         ((t (:foreground ,cyan-muted))))
   `(font-lock-type-face           ((t (:foreground ,blue-muted))))
   `(font-lock-variable-name-face  ((t nil)))
   `(sh-quoted-exec                ((t (:inherit font-lock-string-face))))
   `(trailing-whitespace           ((t (:background ,red-2))))

   ;;;; editor and ui
   `(completions-common-part    ((t (:foreground ,blue-1 :weight bold))))
   `(error                      ((t (:inherit warning))))
   `(fixed-pitch                ((t (:inherit default))))
   `(fringe                     ((t (:inherit default))))
   `(hl-line                    ((t (:background ,base-2))))
   `(highlight-indentation-face ((t (:inherit subtle))))
   `(isearch                    ((t (:inherit match))))
   `(linum                      ((t (:inherit faint))))
   `(line-number                ((t (:inherit faint))))
   `(line-number-current-line   ((t (:inherit highlight))))
   `(minibuffer-prompt          ((t (:foreground ,fg :weight bold))))
   `(secondary-selection        ((t (:inherit shadow :inverse-video t))))
   `(show-paren-match           ((t (:background ,base-3 :underline t))))
   `(show-paren-mismatch        ((t (:foreground ,fg :background ,red-2 :underline t))))
   `(tab-bar                    ((t (:inherit (tab-bar variable-pitch)
                                     :foreground ,fg
                                     :background ,base-0
                                     :height 120
                                     :overline nil
                                     :underline nil
                                     :box (:line-width 5 :color ,base-0)))))
   `(tab-bar-tab                ((t (:inherit tab-bar
                                     :foreground ,fg
                                     :background ,base-0
                                     :weight bold
                                     :overline nil
                                     :underline nil
                                     :box (:line-width 5 :color ,base-0)))))
   `(tab-bar-tab-inactive       ((t (:inherit tab-bar :foreground ,base-4
                                     :underline nil))))
   `(tab-line                   ((t (:inherit tab-bar))))
   ;; `(tab-line-highlight         ((t (:foreground ,fg :background ,base-4
   ;;                                   :box (:line-width 5 :color ,base-4)))))
   ;; `(tab-line-close-highlight   ((t (:inherit tab-line-highlight :background ,base-5))))
   `(tab-line-tab               ((t (:inherit tab-bar-tab))))
   `(tab-line-tab-current       ((t (:inherit tab-bar-tab))))
   `(tab-line-tab-modified      ((t (:foreground ,fg))))
   `(tab-line-tab-inactive      ((t (:inherit tab-bar-tab-inactive))))
   `(window-divider             ((t (:inherit vertical-border))))
   ;; `(window-divider-first-pixel ((t (:foreground ,base-4))))
   ;; `(window-divider-last-pixel  ((t (:inherit window-divider-first-pixel))))

   ;;;; header and mode-line
   `(mode-line-buffer-id ((t (:inherit mode-line-buffer-id :weight normal))))
   `(mode-line           ((t (:inherit (mode-line variable-pitch)
                              :height 120
                              :foreground ,fg
                              :background ,base-3
                              :overline ,gray
                              :box (:line-width 3 :style flat-button)))))
   `(mode-line-inactive  ((t (:inherit mode-line
                              :foreground ,base-4
                              :background ,base-2
                              :box (:line-width 3 :style flat-button)))))
   `(mode-line-highlight ((t (:foreground ,fg :background ,blue-2
                              :box (:line-width 3 :color ,blue-2)))))
   `(header-line         ((t (:inherit mode-line :background ,base-2 :box (:line-width 3 :style flat-button)))))

   ;; ;;;; outline-mode and org-mode
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
   `(org-done              ((t (:inherit success))))
   `(org-verbatim          ((t (:inherit org-code))))

   ;;;; etc
   `(dired-directory            ((t (:inherit bold))))
   `(dired-broken-symlink       ((t (:inherit warning :inverse-video t))))
   `(eshell-ls-directory        ((t (:inherit default))))
   `(eshell-ls-symlink          ((t (:inherit default))))
   `(eshell-prompt              ((t (:inherit shadow))))
   `(eww-form-submit            ((t (:inherit link))))
   ;; `(eww-form-text              ((t (:inherit subtle :box (:line-width 1 :color ,base-8)))))
   `(eww-form-textarea          ((t (:inherit eww-form-text))))
   `(eww-valid-certificate      ((t (:inherit success))))
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
   `(term-color-bright-cyan     ((t (:foreground ,cyan-2))))
   `(term-color-bright-green    ((t (:foreground ,green-2))))
   `(term-color-bright-magenta  ((t (:foreground ,magenta-2))))
   `(term-color-bright-red      ((t (:foreground ,red-2))))
   `(term-color-bright-yellow   ((t (:foreground ,yellow-2))))
   `(whitespace-newline         ((t (:foreground ,red-2))))
   `(whitespace-space           ((t (:inherit shadow))))
   `(whitespace-tab             ((t (:foreground ,blue-2))))

   ;;;; third-party
   `(clojure-keyword-face              ((t (:inherit default))))
   `(diff-hl-insert                    ((t (:foreground ,green-1 :background ,green-3))))
   `(diff-hl-delete                    ((t (:foreground ,red-1 :background ,red-2))))
   `(diff-hl-change                    ((t (:foreground ,blue-1 :background ,blue-4))))
   `(corfu-border                      ((t (:background ,blue-1))))
   `(corfu-current                     ((t (:inherit menu-sel))))
   `(corfu-default                     ((t (:foreground ,fg :background ,blue-5))))
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
   `(magit-branch-current              ((t (:inherit bold))))
   `(magit-branch-local                ((t (:inherit bold))))
   `(magit-branch-remote               ((t (:inherit bold))))
   `(magit-diff-context                ((t (:inherit default))))
   `(magit-diff-context-highlight      ((t (:inherit subtle))))
   ;; `(magit-diff-hunk-heading           ((t (:foreground ,fg :background ,base-3 :overline t))))
   ;; `(magit-diff-hunk-heading-highlight ((t (:foreground ,fg :background ,base-4 :overline t))))
   `(magit-hash                        ((t (:inherit shadow))))
   `(magit-section-heading             ((t (:inherit bold))))
   `(magit-section-highlight           ((t (:inherit hl-line))))
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
   ;; `(mini-modeline-mode-line           ((t (:inherit
   ;;                                          (mini-modeline-mode-line variable-pitch)
   ;;                                          :background ,base-3 :height 0.15))))
   `(orderless-match-face-0            ((t (:inherit completions-common-part))))
   `(orderless-match-face-1            ((t (:foreground ,magenta-1 :weight bold))))
   `(orderless-match-face-2            ((t (:foreground ,cyan-1 :weight bold))))
   `(orderless-match-face-3            ((t (:foreground ,orange-1 :weight bold))))
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

  ;;;; variables
  (custom-theme-set-variables
   'b
   `(ansi-color-names-vector
     [(,fg . ,base-4)
      (,red-1 . ,red-2)
      (,green-1 . ,green-2)
      (,yellow-1 . ,yellow-2)
      (,blue-1    . ,blue-2)
      (,magenta-1 . ,magenta-2)
      (,cyan-1    . ,cyan-2)
      (,base-3     . ,white)])
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
   `(zoom-window-mode-line-color ,cyan-2)
     )
  )

(provide 'b-theme)
(provide-theme 'b)

;;; b-theme.el ends here
