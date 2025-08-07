;; File: calm-themes.el (Main package file)

;;; calm-themes.el --- Calm themes collection -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025 Your Name
;; Author: Your Name
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces, themes
;; URL: https://github.com/yourname/calm-themes

;;; Commentary:
;; A collection of minimal and calm themes with clear UI

;;; Code:

(require 'cl-lib)

(defgroup calm-themes nil
  "Calm theme collection options."
  :group 'faces)

(defvar calm-themes-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the calm-themes package.")

(add-to-list 'custom-theme-load-path calm-themes-dir)

(load (expand-file-name "calm-build.el" calm-themes-dir))

(defun calm-load-theme (theme)
  "Reload the theme builder and enable THEME."
  (interactive
   (list
    (intern (completing-read "Load calm theme: "
                             (cl-remove-if-not
                              (lambda (th)
                                (string-prefix-p "calm-" (symbol-name th)))
                              (custom-available-themes))))))
  (load-file (expand-file-name "calm-build.el" calm-themes-dir))
  (load-theme theme t))

(provide 'calm-themes)
;;; calm-themes.el ends here
