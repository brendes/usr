(deftheme chernila
  "Simple theme for e-ink displays."
  :background-mode 'light
  :kind 'color-scheme)

(frame-set-background-mode nil)
(window-divider-mode -1)
(diff-hl-margin-mode 1)
(setq chernila-ui-height 0.9)

(defconst chernila-colors
  '((white . ((base-0 . "#ffffff")
              (base-1 . "#f8f8f8")
              (base-2 . "#e8e8e8")
              (base-3 . "#dddddd")
              (base-4 . "#aaaaaa")
              (base-5 . "#888888")
              (base-6 . "#000000")))
      (sun . ((base-0 . "#fffffa")
              (base-1 . "#f8f8f3")
              (base-2 . "#e8e8e3")
              (base-3 . "#ddddd8")
              (base-4 . "#aaaaa5")
              (base-5 . "#888883")
              (base-6 . "#000000")))
     (acme . ((base-0 . "#ffffea")
              (base-1 . "#f8f8e3")
              (base-2 . "#e8e8d3")
              (base-3 . "#ddddc8")
              (base-4 . "#aaaa95")
              (base-5 . "#888880")
              (base-6 . "#000000")))))

(defmacro chernila--variant-with-colors (variant &rest body)
  "Execute BODY in a scope where the different colors for given
VARIANT is bound."
  `(let* ((colors (or (cdr (assoc ,variant chernila-colors))
                      (error "No such theme variant")))
          (base-0     (cdr (assoc 'base-0 colors)))
          (base-1     (cdr (assoc 'base-1 colors)))
          (base-2     (cdr (assoc 'base-2 colors)))
          (base-3     (cdr (assoc 'base-3 colors)))
          (base-4     (cdr (assoc 'base-4 colors)))
          (base-5     (cdr (assoc 'base-5 colors)))
          (base-6     (cdr (assoc 'base-6 colors)))
          (bg         (cdr (assoc 'base-0 colors)))
          (fg         (cdr (assoc 'base-6 colors)))
          (red        "IndianRed3")
          (blue       "RoyalBlue3")
          (ok         "")
          )
     ,@body))

(defmacro chernila--faces-spec ()
  "Provide the faces specification."
  (quote
   (mapcar
    (lambda (entry) (list (car entry) `((t ,@(cdr entry)))))
    `(
   ;;;; main faces from which others are derived
      (default         (:foreground ,fg :background ,bg))
      (cursor          (:foreground ,bg :background ,fg))
      (highlight       (:background ,base-2))
      (link            (:foreground ,blue :underline t))
      (match           (:foreground ,bg :background ,base-5 :underline t))
      (lazy-highlight  (:foreground ,fg :background ,base-3 :underline t))
      (org-code        (:foreground ,fg))
      (region          (:background ,base-2))
      (shadow          (:foreground ,base-5))
      (success         (:foreground ,ok :weight bold))
      (error           (:foreground ,fg :underline (:style wave :color ,red)))
      (warning         (:foreground ,red :weight bold))

   ;;;; syntax
      (font-lock-builtin-face        (:inherit font-lock-keyword-face))
      (font-lock-comment-face        (:inherit bold))
      (font-lock-constant-face       nil)
      (font-lock-function-name-face  nil)
      (font-lock-keyword-face        nil)
      (font-lock-preprocessor-face   nil)
      (font-lock-string-face         nil)
      (font-lock-type-face           nil)
      (font-lock-variable-name-face  nil)
      (sh-quoted-exec                (:inherit font-lock-string-face))
      (trailing-whitespace           (:inherit warning))

   ;;;; editor and ui
      (mode-line
       (:inherit mode-line
                 :box (:style released-button)
                 :background ,base-2
                 :height ,chernila-ui-height))
      (mode-line-buffer-id (:inherit (bold variable-pitch)))
      (mode-line-inactive (:inherit mode-line :foreground ,base-4))
      ;;
      (blink-matching-paren-offscreen
       (:inherit shadow))
      (bookmark-face              (:inherit default))
      (completions-common-part    (:inherit bold :underline t))
      (compilation-mode-line-fail (:inherit error))
      (confusingly-reordered      (:inherit error))
      (fixed-pitch                (:inherit default))
      (fringe                     (:inherit default))
      (hl-line                    (:background ,bg :underline t))
      (isearch                    (:inherit match))
      (isearch-fail               (:inherit error))
      (linum                      (:inherit shadow))
      (line-number                (:inherit shadow))
      (line-number-current-line   (:inherit default))
      (minibuffer-prompt          (:foreground ,fg :weight bold))
      (secondary-selection        (:inherit shadow :inverse-video t))
      (show-paren-match           (:foreground ,fg :background ,bg :underline t))
      (show-paren-mismatch        (:foreground ,bg :background ,fg :underline t))
      (vertical-border (:inherit shadow))
      ;;
      (tab-bar
       (:inherit (tab-bar variable-pitch)
                 :foreground ,fg
                 :background ,bg
                 :height 120))
      (tab-bar-tab
       (:inherit tab-bar
                 :foreground ,fg
                 :background ,bg
                 :weight bold))
      (tab-bar-tab-inactive (:inherit tab-bar :foreground ,base-5))
      ;;
      (tab-line
       (:inherit (tab-line variable-pitch)
                 :background ,base-2
                 :height ,chernila-ui-height
                 :box (:line-width 5 :color ,base-2 :style flat-button)))
      (tab-line-highlight    (:inherit tab-line-tab-current))
      (tab-line-tab          (:inherit tab-line-tab))
      (tab-line-tab-current
       (:inherit tab-line-tab-current
                 :foreground ,fg
                 :background ,bg
                 :box (:style flat-button)))
      (tab-line-tab-modified (:foreground ,fg))
      (tab-line-tab-inactive
       (:inherit tab-line-tab-inactive :foreground ,base-5))

   ;;;; outline-mode and org-mode
      (outline-1             (:inherit bold))
      (outline-2             (:inherit outline-1))
      (outline-3             (:inherit outline-1))
      (outline-4             (:inherit outline-1))
      (outline-5             (:inherit outline-1))
      (outline-6             (:inherit outline-1))
      (outline-7             (:inherit outline-1))
      (outline-8             (:inherit outline-1))
      (org-agenda-structure  (:inherit bold))
      (org-block             (:inherit subtle))
      (org-block-begin-line  (:inherit shadow))
      (org-block-end-line    (:inherit org-block-begin-line))
      (org-code              (:inherit shadow))
      (org-date              (:inherit link))
      (org-document-info     (:inherit default))
      (org-document-title    (:inherit default))
      (org-done              (:inherit success :underline t))
      (org-drawer            (:inherit shadow))
      (org-formula           (:inherit (org-formula org-code)))
      (org-headline-done     (:inherit outline-1))
      (org-hide              (:foreground ,base-0))
      (org-list-bullet       (:inherit bold))
      (org-list-dt           (:inherit bold))
      (org-meta-line         (:inherit font-lock-comment-face))
      (org-quote             (:inherit org-block :italic t))
      (org-table             (:inherit org-block))
      (org-table-header      (:background ,base-1 highlight :underline t))
      (org-todo              (:foreground ,red :weight bold :underline t))
      (org-verbatim          (:inherit org-code))

   ;;;; etc
      (dired-directory        (:inherit bold))
      (dired-broken-symlink   (:inherit warning :inverse-video t))
      (eshell-ls-directory    (:inherit default))
      (eshell-ls-symlink      (:inherit default))
      (eshell-prompt          (:inherit shadow))
      (eww-form-submit        (:inherit link))
      (eww-form-text          (:inherit default :box (:line-width 1 :color ,base-4)))
      (eww-form-textarea      (:inherit eww-form-text))
      (eww-valid-certificate  (:foreground ,fg))
      (gnus-header-name       (:inherit shadow))
      (gnus-header-content    (:inherit default))
      (gnus-signature         (:inherit default))
      (help-key-binding       (:inherit org-code))
      (Info-quoted            (:inherit org-code))
      ;;
      (term-color-black          (:foreground ,fg))
      (term-color-white          (:foreground ,base-2))
      (term-color-bright-black   (:inherit shadow))
      (term-color-bright-white   (:foreground ,base-1))
      (term-color-blue           (:foreground ,fg))
      (term-color-cyan           (:foreground ,fg))
      (term-color-green          (:foreground ,fg))
      (term-color-magenta        (:foreground ,fg))
      (term-color-red            (:foreground ,fg))
      (term-color-yellow         (:foreground ,fg))
      (term-color-bright-blue    (:inherit shadow))
      (term-color-bright-cyan    (:inherit shadow))
      (term-color-bright-green   (:inherit shadow))
      (term-color-bright-magenta (:inherit shadow))
      (term-color-bright-red     (:inherit shadow))
      (term-color-bright-yellow  (:inherit shadow))
      (whitespace-newline        (:inherit shadow))
      (whitespace-space          (:inherit shadow))
      (whitespace-tab            (:inherit shadow))

   ;;;; third-party
      (clojure-keyword-face              (:inherit default))
      (diff-hl-margin-change             (:inherit shadow))
      (diff-hl-margin-delete             (:inherit shadow))
      (diff-hl-margin-insert             (:inherit shadow))
      (corfu-border                      (:background ,fg))
      (corfu-current                     (:inherit region))
      (corfu-default                     (:foreground ,fg))
      (elfeed-search-date-face           (:inherit shadow))
      (elfeed-search-unread-title-face   (:inherit bold))
      (elfeed-search-feed-face           (:inherit shadow))
      (elfeed-search-tag-face            (:inherit shadow))
      (ess-assignment-face               (:inherit bold))
      (ess-constant-face                 (:inherit font-lock-constant-face))
      (ess-modifiers-face                (:inherit bold))
      (ess-%op%-face                     (:inherit bold))
      (eyebrowse-mode-line-active        (:inherit bold :underline t))
      (evil-ex-substitute-matches        (:inherit match))
      (evil-ex-substitute-replacement    (:inherit lazy-highlight :italic t))
      (font-latex-bold-face              (:inherit bold))
      (font-latex-sedate-face            (:inherit shadow))
      (hl-todo                           (:inherit org-todo))
      (magit-branch-current              (:inherit bold))
      (magit-branch-local                (:inherit bold))
      (magit-branch-remote               (:inherit bold))
      (magit-diff-context                (:inherit default))
      (magit-diff-context-highlight      (:background ,base-1))
      (magit-diff-hunk-heading           (:inherit bold :overline t))
      (magit-diff-hunk-heading-highlight (:inherit bold :overline t :background ,base-2))
      (magit-hash                        (:inherit shadow))
      (magit-section-heading             (:inherit bold))
      (magit-section-highlight           (:inherit highlight))
      (markdown-code-face                (:inherit org-block))
      (markdown-header-delimiter-face    (:inherit markdown-header-face))
      (markdown-inline-code-face         (:inherit org-code))
      (markdown-line-break-face          (:inherit shadow :underline t))
      (markdown-link-face                (:inherit link))
      (markdown-list-face                (:inherit default))
      (markdown-markup-face              (:inherit shadow))
      (markdown-pre-face                 (:inherit markdown-code-face))
      (markup-comment-face               (:inherit shadow))
      (markup-meta-face                  (:inherit shadow))
      (markup-gen-face                   (:inherit outline-1))
      (markup-list-face                  (:inherit default))
      (markup-meta-hide-face             (:inherit shadow))
      (markup-title-0-face               (:inherit markup-gen-face))
      (markup-title-1-face               (:inherit markup-gen-face))
      (markup-title-2-face               (:inherit markup-gen-face))
      (markup-title-3-face               (:inherit markup-gen-face))
      (markup-title-4-face               (:inherit markup-gen-face))
      (markup-title-5-face               (:inherit markup-gen-face))
      (markup-verbatim-face              (:inherit shadow))
      (orderless-match-face-0            (:inherit completions-common-part))
      (orderless-match-face-1            (:inherit orderless-match-face-0))
      (orderless-match-face-2            (:inherit orderless-match-face-0))
      (orderless-match-face-3            (:inherit orderless-match-face-0))
      (popup-tip-face                    (:inherit default))
      (popup-menu-mouse-face             (:inherit default))
      (transient-key                     (:inherit bold))
      (transient-key-exit                (:inherit bold))
      (transient-key-stay                (:inherit bold))
      (transient-key-return              (:inherit bold))
      (tuareg-font-lock-governing-face   (:inherit font-lock-keyword-face))
      (tuareg-font-lock-operator-face    (:inherit font-lock-keyword-face))
      (vertico-current                   (:inherit hl-line))
      )
    )
   )
  )

  (custom-theme-set-faces
   'chernila
   )

  ;;;; variables
  ;; (custom-theme-set-variables
  ;;  'chernila
  ;;  `(hl-todo-keyword-faces
  ;;    '(("HOLD"  . (:inherit org-todo))
  ;;      ("TODO"  . (:inherit org-todo))
  ;;      ("FIXME" . (:inherit org-todo))
  ;;      ("BUG"   . (:inherit org-todo))
  ;;      ("FAIL"  . (:inherit org-todo))
  ;;      ("DONE"  . (:inherit org-done))
  ;;      ("NOTE"  . (:inherit org-todo))))
  ;;  `(org-todo-keyword-faces
  ;;    '(("IN PROGRESS" . (:inherit shadow :underline t :weight bold))
  ;;      ("WAITING"     . (:inherit shadow :underline t :weight bold))
  ;;      ("ON HOLD"     . (:inherit shadow :underline t :weight bold))))
  ;;    )
  ;; )

(defun chernila--variant-name (variant)
  "Create symbol for color theme variant VARIANT."
  (intern (format "chernila-%s" (symbol-name variant))))

(defmacro chernila--define-theme (variant)
  "Define a theme for the Chernila variant VARIANT."
  (let ((name (chernila--variant-name variant))
        (doc (format "Chernila Theme (%s version)" variant)))
    `(progn
       (deftheme ,name ,doc)
       (put ',name 'theme-immediate t)
       (chernila--variant-with-colors
        ',variant
        (apply 'custom-theme-set-faces ',name
               (chernila--faces-spec)))
       (provide-theme ',name))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))
(provide 'chernila-theme)

;;; chernila-theme.el ends here
