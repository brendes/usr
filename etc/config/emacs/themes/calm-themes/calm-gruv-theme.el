;;; calm-gruv-theme.el --- Calm Theme, Dark Warm Version

;;; Code:

(require 'calm-build)

(calm-define-variant
 calm-gruv
 "Calm and minimal theme with clear UI (warm dark version)."
 dark
 '((bg-main      "#32302f")
   (bg-subtle    "#3a3837")
   (bg-fade      "#3e3c3b")
   (bg-hl        "#4c4948")
   (fg-faint     "#595654")
   (fg-fade      "#888070")
   (fg-dim       "#a89888")
   (fg-subtle    "#c8b8a0")
   (fg-main      "#e0d0bb")
   (fg-bold      "#ffeecc")))

;;;###autoload
(when load-file-name
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory (file-name-directory load-file-name))))

;;; calm-gruv-theme.el ends here
