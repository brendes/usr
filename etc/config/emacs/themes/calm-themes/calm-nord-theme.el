;;; calm-nord-theme.el --- Calm Theme, monochrome Nord version

;;; Code:

(require 'calm-build)

(calm-define-variant
 calm-nord
 "Calm and minimal theme with clear UI (Nord version)."
 dark
 '((bg-main    "#2e3440")
   (bg-subtle  "#363d4c")
   (bg-fade    "#3c4352")
   (bg-hl      "#434c5e")
   (fg-faint   "#4c566a")
   (fg-fade    "#78859e")
   (fg-dim     "#98a5b8")
   (fg-subtle  "#b8c0d8")
   (fg-main    "#d8dee9")
   (fg-bold    "#e8eef9")
   ;;
   (red-1      "#bf616a")
   (orange-1   "#e09868")
   (yellow-1   "#ebcb8b")
   (green-1    "#a3be8c")
   (cyan-1     "#88c0d0")
   (blue-1     "#81a1c1")
   (magenta-1  "#b48ead")
   ;;
   (red-2      red-1)
   (red-3      red-1)
   (orange-2   orange-1)
   (yellow-2   yellow-1)
   (green-2    green-1)
   (green-3    green-1)
   (cyan-2     cyan-1)
   (blue-2     blue-1)
   (blue-3     blue-1)
   (magenta-2  magenta-1)
   (ok-fg      cyan-1)
   (ok-bg      cyan-2)
   (warn-fg    red-1)
   (warn-bg    red-2)
   (link       cyan-1)))

;;;###autoload
(when load-file-name
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory (file-name-directory load-file-name))))

;;; calm-nord-theme.el ends here
