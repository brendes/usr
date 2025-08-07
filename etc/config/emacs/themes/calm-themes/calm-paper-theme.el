;;; calm-paper-theme.el --- Calm Paper Theme

;;; Code:

(require 'calm-build)

;; from the flatwhite theme
;; base1            #605a52
;; base2            #92826c
;; base3            #b9a992
;; base4            #dcd2c6
;; base5            #e4dcd2
;; base6            #f0ebe4
;; base7            #f6f3ed
;; orange-text-sec  #957e5f
;; green-text-sec   #80885c
;; teal-text-sec    #5f8b7d
;; blue-text-sec    #73829f
;; purple-text-sec  #9b729b

(calm-define-variant
 calm-paper
 "Calm and minimal theme with clear UI (more off-white version)."
 light
 '((bg-main      "#f6f3ed")
   (bg-subtle    "#f0ebe4")
   (bg-fade      "#ece4da")
   (bg-hl        "#dcd2c6")
   (fg-faint     "#d8d0c8")
   (fg-fade      "#a89892")
   (fg-dim       "#82726c")
   (fg-subtle    "#605a52")
   ;; (fg-main      "#403a32")
   (fg-main      "#302a22")
   (fg-bold      "#000000")
   (red-1       "#aa4048")
   (cyan-1      "#5f8b7d")
   (blue-1      "#667799")))


;;;###autoload
  (when load-file-name
    (add-to-list
     'custom-theme-load-path
     (file-name-as-directory (file-name-directory load-file-name))))

;;; calm-paper-theme.el ends here
