(defun eink-device-p ()
  "Return t if likely on an e-ink device like Onyx Boox Note Air 3."
  (when (eq system-type 'android)  ;; Android-specific check first
    (let ((model (shell-command-to-string "getprop ro.product.model 2>/dev/null"))
          (brand (shell-command-to-string "getprop ro.product.brand 2>/dev/null")))
      (or (string-match-p "BOOX" (or brand ""))
          (string-match-p "NoteAir3" (or model ""))))))
