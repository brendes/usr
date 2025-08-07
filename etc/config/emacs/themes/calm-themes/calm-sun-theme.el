;;; calm-sun-theme.el --- Calm Sun Theme

;;; Code:

(require 'calm-build)

(calm-define-variant
 calm-sun
 "Calm and minimal theme with clear UI (off-white version)."
 light
 '((bg-main    "#fffffa")
   (bg-subtle  "#f8f8f3")
   (bg-fade    "#f2f2eb")
   (bg-hl      "#e8e8e3")
   (fg-faint   "#ddddd8")
   (fg-fade    "#aaaaa5")
   (fg-dim     "#90908a")
   (fg-subtle  "#70706a")
   (fg-main    (calm--default-weight-fg "#383838" "#000000"))
   (fg-bold    "#000000")))

;;;###autoload
(when load-file-name
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory (file-name-directory load-file-name))))

;;; calm-sun-theme.el ends here
