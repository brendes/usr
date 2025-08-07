;;; calm-dark-theme.el --- Calm Theme, Dark Version

(require 'calm-build)

(calm-define-variant
 calm-dark
 "Calm and minimal theme with clear UI (dark)."
 dark
 '((bg-main    "#282828")
   (bg-subtle  "#303030")
   (bg-fade    "#383838")
   (bg-hl      "#484848")
   (fg-faint   "#585858")
   (fg-fade    "#787878")
   (fg-dim     "#989898")
   (fg-subtle  "#b8b8b8")
   (fg-main    "#e0e0e0")
   (fg-bold    "#f8f8f8")))

;;;###autoload
(when load-file-name
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory (file-name-directory load-file-name))))

;;; calm-dark-theme.el ends here
