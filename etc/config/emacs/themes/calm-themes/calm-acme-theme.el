;;; calm-acme-theme.el --- Calm Acme Theme

(require 'calm-build)

(calm-define-variant
 calm-acme
 "Calm and minimal theme with clear UI (Acme version)."
 light
 '((bg-main    "#ffffea")
   (bg-subtle  "#f8f8e3")
   (bg-fade    "#f2f2dd")
   (bg-hl      "#e8e8d3")
   (fg-faint   "#d8d8c3")
   (fg-fade    "#aaaa99")
   (fg-dim     "#888877")
   (fg-subtle  "#666655")
   (fg-main    (calm--default-weight-fg "#303030" "#000000"))
   (fg-bold    "#000000")
   (blue-1     "#5570bb")
   (blue-2     "#b0c0e0")
   (green-1    "#669966")
   (green-2    "#b8d8b8")
   (green-3    "#d8e8d8")
   (yellow-1   "#99884c")
   (yellow-2   "#eeeeaa")
   (cyan-1     "#669999")
   (ok-fg      cyan-1)
   (region     yellow-2)))

;;;###autoload
(when load-file-name
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory (file-name-directory load-file-name))))

;;; calm-acme-theme.el ends here
