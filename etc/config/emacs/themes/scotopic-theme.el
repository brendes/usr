(deftheme scotopic
  "Dark red theme for low light."
  :background-mode 'dark
  :kind 'color-scheme)

(defgroup scotopic-faces ()
  "Group for faces defined by scotopic-theme."
  :group 'faces
  :prefix "scotopic-")

(defface subtle nil
  "Text with subtle background."
  :group 'scotopic-faces)

(defface faint nil
  "Faint foreground color face."
  :group 'scotopic-faces)

(defface strong-hl nil
  "Background highlight color face."
  :group 'scotopic-faces)

(defface border nil
  "Strong or bold border color."
  :group 'scotopic-faces)

;;;; ui
(when (eq system-type 'darwin)
  (let (mode dark)
    (setq default-frame-alist
          (append default-frame-alist
                  `((ns-transparent-titlebar . t)
                    (ns-appearance . mode))))
    (modify-all-frames-parameters
     `((ns-transparent-titlebar . t)
       (ns-appearance . mode)))))

(window-divider-mode -1)

(when (featurep 'diff-hl)
  (progn
    (global-diff-hl-mode 1)
    (diff-hl-margin-mode 1)))

(when (and (featurep 'fontaine)
           (boundp 'fontaine-mode)
           (fontaine-set-preset 'input)))

;;;; colors
(let* ((base-0  "#000000")
       (base-1  "#181010")
       (base-2  "#332222")
       (base-3  "#553333")
       (base-4  "#664444")
       (base-5  "#884444")
       (base-6  "#995555")
       (base-7  "#bb6666")
       (base-8  "#ee8888")
       (base-9  "#ff9999")
       ;;
       (bg         base-0)
       (bg-faint   base-1)
       (bg-subtle  base-2)
       (bg-hl      base-3)
       (bg-strong  base-5)
       (bg-bold    base-8)
       ;;
       (fg-faint   base-4)
       (fg-fade    base-5)
       (fg-dim     base-6)
       (fg-subtle  base-7)
       (fg         base-8)
       (fg-bold    base-9)
       (black      base-0)
       ;;
       (ui-font-height 0.9))

  ;;;; theme
  (custom-theme-set-faces
   'scotopic

   ;;;; main faces from which many are derived
   `(default    ((t (:foreground ,fg :background ,bg))))
   `(border     ((t (:foreground ,fg-dim :background ,fg-dim))))
   `(bold       ((t (:foreground ,fg-bold :weight bold))))
   `(cursor     ((t (:foreground ,black :background ,bg-bold))))
   `(faint      ((t (:foreground ,fg-faint))))
   `(highlight  ((t (:foreground ,fg-bold :background ,bg-hl))))
   `(link       ((t (:foreground ,fg-bold :underline t))))
   `(match      ((t (:foreground ,fg-bold :background ,bg :inverse-video t))))
   `(org-block  ((t (:foreground unspecified :background ,bg-faint))))
   `(org-code   ((t (:foreground ,fg-bold :background ,bg-subtle))))
   `(region     ((t (:foreground ,fg-bold :background ,bg-hl))))
   `(shadow     ((t (:foreground ,fg-dim))))
   `(strong-hl  ((t (:foreground ,fg-bold :background ,bg-strong))))
   `(subtle     ((t (:foreground unspecified :background ,bg-subtle))))
   `(success    ((t (:inherit bold))))
   `(error      ((t (:inherit bold :underline (:style wave)))))
   `(warning    ((t (:inherit bold))))

   ;;;; syntax
   `(font-lock-builtin-face        ((t nil)))
   `(font-lock-comment-face        ((t (:inherit shadow))))
   `(font-lock-constant-face       ((t nil)))
   `(font-lock-function-name-face  ((t nil)))
   `(font-lock-keyword-face        ((t nil)))
   `(font-lock-preprocessor-face   ((t nil)))
   `(font-lock-string-face         ((t (:foreground ,fg-subtle))))
   `(font-lock-type-face           ((t nil)))
   `(font-lock-variable-name-face  ((t nil)))
   `(sh-quoted-exec                ((t (:inherit font-lock-string-face))))
   `(trailing-whitespace           ((t (:inherit warning))))

   ;;;; editor and ui
   `(completions-common-part     ((t (:inherit bold))))
   `(completions-annotations     ((t (:inherit italic :foreground ,fg))))
   `(fixed-pitch                 ((t (:inherit default))))
   `(fringe                      ((t (:inherit default))))
   `(hl-line                     ((t (:inherit subtle))))
   `(highlight-indentation-face  ((t (:inherit subtle))))
   `(isearch                     ((t (:inherit highlight))))
   `(isearch-fail                ((t (:inherit error :inverse-video t))))
   `(lazy-highlight              ((t (:inherit strong-hl))))
   `(link-visited                ((t (:inherit link-visited :foreground ,fg-subtle))))
   `(linum                       ((t (:inherit faint))))
   `(line-number                 ((t (:inherit faint))))
   `(line-number-current-line    ((t (:inherit default))))
   `(minibuffer-prompt           ((t (:inherit bold))))
   `(secondary-selection         ((t (:inherit shadow :inverse-video t))))
   `(show-paren-match            ((t (:inherit region :underline t))))
   `(show-paren-mismatch         ((t (:foreground ,black :background ,bg-strong :underline t))))
   `(tooltip
     ((t (:inherit variable-pitch :foreground ,fg-bold :background ,bg-hl))))

   `(mode-line
     ((t (:inherit mode-line
          :height ,ui-font-height
          :foreground ,fg
          :background ,bg-subtle
          :overline ,fg-fade
          :underline nil
          :box (:line-width 3 :style flat-button)))))
   `(mode-line-buffer-id ((t (:inherit variable-pitch :weight bold))))
   `(mode-line-inactive ((t (:inherit mode-line :foreground ,fg-dim))))
   `(mode-line-highlight ((t (:inherit highlight))))

   `(header-line
     ((t (:inherit header-line
          :foreground ,fg-bold
          :background ,bg-subtle
          :overline ,fg-fade
          :box (:line-width 3 :style flat-button)))))

   `(tab-bar
     ((t (:inherit variable-pitch
          :foreground ,fg
          :background ,bg
          :height ,ui-font-height))))
   `(tab-bar-tab
     ((t (:inherit tab-bar :foreground ,fg-bold :weight bold))))
   `(tab-bar-tab-inactive ((t (:inherit tab-bar :foreground ,fg-subtle))))

   `(tab-line
     ((t (:inherit mode-line :height 120 :box (:line-width 8 :style flat-button)))))
   `(tab-line-highlight
     ((t (:inherit tab-line :foreground ,fg-bold :background ,bg-hl))))
   `(tab-line-close-highlight ((t (:inherit tab-line-highlight))))
   `(tab-line-tab             ((t (:inherit tab-line-tab))))
   `(tab-line-tab-current     ((t (:inherit tab-line-tab))))
   `(tab-line-tab-modified    ((t (:foreground ,fg-bold))))
   `(tab-line-tab-inactive    ((t (:inherit (tab-line-tab-inactive shadow)))))
   `(tab-line-tab-special     ((t (:inherit (tab-line-special shadow) :slant italic))))

   `(window-divider             ((t (:inherit border))))
   `(window-divider-first-pixel ((t (:inherit window-divider))))
   `(window-divider-last-pixel  ((t (:inherit window-divider-first-pixel))))
   `(vertical-border            ((t (:inherit border))))

   ;;;; outline-mode and org-mode
   `(outline-1             ((t (:inherit default :weight bold))))
   `(outline-2             ((t (:inherit outline-1))))
   `(outline-3             ((t (:inherit outline-1))))
   `(outline-4             ((t (:inherit outline-1))))
   `(outline-5             ((t (:inherit outline-1))))
   `(outline-6             ((t (:inherit outline-1))))
   `(outline-7             ((t (:inherit outline-1))))
   `(outline-8             ((t (:inherit outline-1))))
   `(org-agenda-structure  ((t (:inherit bold))))
   `(org-block-begin-line  ((t (:inherit (shadow org-block) :extend t))))
   `(org-block-end-line    ((t (:inherit org-block-begin-line))))
   `(org-checkbox-statistics-todo ((t (:inherit bold))))
   `(org-date              ((t (:inherit link))))
   `(org-document-info     ((t (:inherit default))))
   `(org-document-title    ((t (:inherit default))))
   `(org-drawer            ((t (:inherit shadow))))
   `(org-formula           ((t (:inherit (org-formula org-code)))))
   `(org-headline-done     ((t (:inherit outline-1))))
   `(org-hide              ((t (:foreground ,bg))))
   `(org-list-bullet       ((t (:inherit bold))))
   `(org-list-dt           ((t (:inherit bold))))
   `(org-meta-line         ((t (:inherit font-lock-comment-face))))
   `(org-quote             ((t (:inherit org-block :italic t))))
   `(org-table             ((t (:inherit org-block))))
   `(org-table-header      ((t (:inherit highlight :underline t))))
   `(org-todo              ((t (:inherit bold :inverse-video t))))
   `(org-done              ((t (:inherit success :underline t))))
   `(org-verbatim          ((t (:inherit org-code))))

   ;;;; etc
   `(comint-highlight-prompt ((t (:inherit shadow))))
   `(custom-button
     ((t (:inherit strong-hl :box (:line-width 2 :style released-button)))))
   `(custom-button-mouse
     ((t (:inherit custom-button :foreground ,black :background ,bg-bold))))
   `(custom-button-pressed
     ((t (:inherit custom-button :box (:line-width 2 :style pressed-button)))))
   `(dired-directory            ((t (:inherit bold))))
   `(dired-broken-symlink       ((t (:inherit warning :inverse-video t))))
   `(eshell-ls-directory        ((t (:inherit default))))
   `(eshell-ls-symlink          ((t (:inherit default))))
   `(eshell-prompt              ((t (:inherit shadow))))
   `(eww-form-select            ((t (:inherit subtle :box (:line-width 2 :style released-button)))))
   `(eww-form-submit            ((t (:inherit link))))
   `(eww-form-text              ((t (:inherit subtle :box (:line-width 1 :color ,bg)))))
   `(eww-form-textarea          ((t (:inherit eww-form-text))))
   `(eww-valid-certificate      ((t (:inherit header-line))))
   `(gnus-header-name           ((t (:inherit shadow))))
   `(gnus-header-content        ((t (:inherit default))))
   `(gnus-signature             ((t (:inherit default))))
   `(help-key-binding           ((t (:inherit org-code))))
   `(Info-quoted                ((t (:inherit org-code :background ,bg-subtle))))
   `(info-header-node           ((t (:inherit bold :italic t))))
   `(info-menu-star             ((t (:inherit bold))))
   `(info-node                  ((t (:inherit bold :italic t))))
   `(shr-code                   ((t (:inherit shr-code :background ,bg-subtle))))
   `(shr-text                   ((t (:inherit variable-pitch-text))))
   `(term-color-black           ((t (:foreground ,black))))
   `(term-color-white           ((t (:inherit faint))))
   `(term-color-bright-black    ((t (:inherit shadow))))
   `(term-color-bright-white    ((t (:foreground ,fg-bold))))
   `(term-color-blue            ((t (:inherit default))))
   `(term-color-cyan            ((t (:inherit default))))
   `(term-color-green           ((t (:inherit default))))
   `(term-color-magenta         ((t (:inherit default))))
   `(term-color-red             ((t (:inherit default))))
   `(term-color-yellow          ((t (:inherit default))))
   `(term-color-bright-blue     ((t (:inherit default))))
   `(term-color-bright-cyan     ((t (:inherit default))))
   `(term-color-bright-green    ((t (:inherit default))))
   `(term-color-bright-magenta  ((t (:inherit default))))
   `(term-color-bright-red      ((t (:inherit default))))
   `(term-color-bright-yellow   ((t (:inherit default))))
   `(widget-field               ((t (:inherit subtle :box (:line-width 1 :color ,fg-fade)))))
   `(widget-single-line-field   ((t (:inherit widget-field))))

   ;;;; third-party
   `(clojure-keyword-face            ((t (:inherit default))))
   `(diff-hl-change                  ((t (:foreground ,fg-subtle :background ,bg))))
   `(diff-hl-delete                  ((t (:inherit diff-hl-change))))
   `(diff-hl-insert                  ((t (:inherit diff-hl-change))))
   `(corfu-bar                       ((t (:background ,fg))))
   `(corfu-border                    ((t (:background ,fg))))
   `(corfu-current                   ((t (:inherit strong-hl))))
   `(corfu-default                   ((t (:inherit subtle))))
   `(elfeed-search-date-face         ((t (:inherit shadow))))
   `(elfeed-search-unread-title-face ((t (:inherit bold))))
   `(elfeed-search-feed-face         ((t (:inherit shadow))))
   `(elfeed-search-tag-face          ((t (:inherit shadow))))
   `(ess-assignment-face             ((t (:inherit bold))))
   `(ess-constant-face               ((t (:inherit font-lock-constant-face))))
   `(ess-modifiers-face              ((t (:inherit bold))))
   `(ess-%op%-face                   ((t (:inherit bold))))
   `(eyebrowse-mode-line-active      ((t (:inherit bold :underline t))))
   `(evil-ex-substitute-matches      ((t (:inherit match))))
   `(evil-ex-substitute-replacement  ((t (:inherit lazy-highlight :italic t))))
   `(font-latex-bold-face            ((t (:inherit bold))))
   `(font-latex-sedate-face          ((t (:inherit shadow))))
   `(highlight-indent-guides-odd-face ((t (:background ,bg-subtle))))
   `(highlight-indent-guides-even-face ((t (:background ,bg-subtle))))
   `(highlight-indent-guides-character-face ((t (:foreground ,bg-subtle))))
   `(highlight-indentation-face ((t (:background ,bg-subtle))))
   `(hl-todo                         ((t (:inherit bold :underline t))))
   `(magit-branch-current            ((t (:inherit bold))))
   `(magit-branch-local              ((t (:inherit bold))))
   `(magit-branch-remote             ((t (:inherit bold))))
   `(magit-diff-context              ((t (:inherit default))))
   `(magit-diff-context-highlight    ((t (:inherit subtle :background ,bg-faint))))
   `(magit-diff-hunk-heading
     ((t (:foreground ,fg :background ,bg-hl :overline t))))
   `(magit-diff-hunk-heading-highlight
     ((t (:foreground ,fg-bold :background ,bg-strong :overline t))))
   `(magit-hash                       ((t (:inherit shadow))))
   `(magit-section-heading            ((t (:inherit bold))))
   `(magit-section-highlight          ((t (:inherit hl-line))))
   `(markdown-code-face               ((t (:inherit org-block))))
   `(markdown-header-delimiter-face   ((t (:inherit markdown-header-face))))
   `(markdown-inline-code-face        ((t (:inherit org-code))))
   `(markdown-line-break-face         ((t (:inherit shadow :underline t))))
   `(markdown-link-face               ((t (:inherit shadow :underline t))))
   `(markdown-list-face               ((t (:inherit default))))
   `(markdown-markup-face             ((t (:inherit shadow))))
   `(markdown-pre-face                ((t (:inherit markdown-code-face))))
   `(markup-comment-face              ((t (:inherit shadow))))
   `(markup-meta-face                 ((t (:inherit shadow))))
   `(markup-gen-face                  ((t (:inherit outline-1))))
   `(markup-list-face                 ((t (:inherit default))))
   `(markup-meta-hide-face            ((t (:inherit shadow))))
   `(markup-title-0-face              ((t (:inherit markup-gen-face))))
   `(markup-title-1-face              ((t (:inherit markup-gen-face))))
   `(markup-title-2-face              ((t (:inherit markup-gen-face))))
   `(markup-title-3-face              ((t (:inherit markup-gen-face))))
   `(markup-title-4-face              ((t (:inherit markup-gen-face))))
   `(markup-title-5-face              ((t (:inherit markup-gen-face))))
   `(markup-verbatim-face             ((t (:inherit shadow))))
   `(mini-modeline-mode-line          ((t (:inherit subtle :height 0.15))))
   `(mini-modeline-mode-line-inactive ((t (:inherit mini-modeline-mode-line))))
   `(orderless-match-face-0           ((t (:inherit completions-common-part))))
   `(orderless-match-face-1           ((t (:inherit orderless-match-face-0))))
   `(orderless-match-face-2           ((t (:inherit orderless-match-face-0))))
   `(orderless-match-face-3           ((t (:inherit orderless-match-face-0))))
   `(popup-tip-face                   ((t (:inherit subtle))))
   `(popup-menu-mouse-face            ((t (:inherit strong-hl))))
   `(tab-bar-echo-area-tab            ((t (:inherit (variable-pitch bold)))))
   `(tab-bar-echo-area-tab-inactive
     ((t (:inherit (tab-bar-echo-area-tab-inactive variable-pitch shadow)))))
   `(tab-bar-echo-area-tab-ungrouped
     ((t (:inherit (tab-bar-echo-area-tab-ungrouped variable-pitch shadow)))))
   `(transient-key                    ((t (:inherit bold))))
   `(transient-key-exit               ((t (:inherit bold))))
   `(transient-key-stay               ((t (:inherit bold))))
   `(transient-key-return             ((t (:inherit bold))))
   `(tuareg-font-lock-governing-face  ((t (:inherit font-lock-keyword-face))))
   `(tuareg-font-lock-operator-face   ((t (:inherit font-lock-keyword-face))))
   `(vertico-current                  ((t (:inherit hl-line))))
   )

  ;;;; variables
  (custom-theme-set-variables
   'scotopic
   `(ansi-color-names-vector
     [(,bg . ,fg-dim)
      (,fg . ,fg)
      (,fg . ,fg)
      (,fg . ,fg)
      (,fg . ,fg)
      (,fg . ,fg)
      (,fg . ,fg)
      (,fg-faint . ,fg-bold)])
   `(hl-todo-keyword-faces
      '(("HOLD" . (:inherit org-todo))
        ("TODO" . (:inherit org-todo))
        ("FAIL" . (:inherit org-todo))
        ("BUG" . (:inherit org-todo))
        ("DONE" . (:inherit org-todo))
        ("NOTE" . (:inherit org-todo))))
   `(org-todo-keyword-faces
      '(("IN PROGRESS" . (:inherit org-todo))
        ("WAITING" . (:inherit org-todo))
        ("ON HOLD" . (:inherit org-todo))))
   `(zoom-window-mode-line-color ,bg-subtle)
     )
  )

(provide 'scotopic-theme)
(provide-theme 'scotopic)

;;; scotopic-theme.el ends here
