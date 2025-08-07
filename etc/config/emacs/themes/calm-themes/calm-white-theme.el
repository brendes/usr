;;; calm-white-theme.el --- Calm White Mono Theme

(require 'calm-build)

(calm-define-variant
 calm-white
 "Calm and minimal theme with clear UI (white version)."
 light
 '((bg-main   "#ffffff")
   (bg-subtle  "#f8f8f8")
   (bg-fade    "#f2f2f2")
   (bg-hl      "#e8e8e8")
   (fg-faint   "#dddddd")
   (fg-fade    "#aaaaaa")
   (fg-dim     "#999999")
   (fg-subtle  "#777777")
   (fg-main    (calm--default-weight-fg "#303030" "#111111"))
   (fg-bold    "#000000")))

;;;###autoload
(when load-file-name
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory (file-name-directory load-file-name))))

;;; calm-white-theme.el ends here
