;;; smart-window-resize.el --- Smart window resizing based on window topology -*- lexical-binding: t -*-

;; Author: Brendan Desmond
;; Version: 1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience, frames
;; URL: your-repo-url

;;; Commentary:
;; Provides intuitive window resizing based on window position within the frame.
;; Windows resize naturally based on their position relative to other windows.

;;; Code:

(defgroup smart-window-resize nil
  "Smart window resizing based on window topology."
  :group 'convenience)

(defcustom smart-window-resize-verbose nil
  "If non-nil, print debug messages during window resizing operations."
  :type 'boolean
  :group 'smart-window-resize)

(defmacro smart-window-message (&rest args)
  "Print message only when `smart-window-resize-verbose' is non-nil."
  `(when smart-window-resize-verbose
     (message ,@args)))

;;;###autoload
(defun window-has-window-right-p ()
  "Return t if there is a window to the right of the current window."
  (declare (side-effect-free t))
  (let* ((current-window (selected-window))
         (window-right (window-in-direction 'right current-window)))
    (smart-window-message "Window right: %s" window-right)
    (not (null window-right))))

;;;###autoload
(defun window-has-window-left-p ()
  "Return t if there is a window to the left of the current window."
  (declare (side-effect-free t))
  (let* ((current-window (selected-window))
         (window-left (window-in-direction 'left current-window)))
    (smart-window-message "Window left: %s" window-left)
    (not (null window-left))))

;;;###autoload
(defun window-has-window-below-p ()
  "Return t if there is a window below the current window."
  (declare (side-effect-free t))
  (let* ((current-window (selected-window))
         (window-below (window-in-direction 'below current-window)))
    (smart-window-message "Window below: %s" window-below)
    (not (null window-below))))

;;;###autoload
(defun window-has-window-above-p ()
  "Return t if there is a window above the current window."
  (declare (side-effect-free t))
  (let* ((current-window (selected-window))
         (window-above (window-in-direction 'above current-window)))
    (smart-window-message "Window above: %s" window-above)
    (not (null window-above))))

;;;###autoload
(defun smart-adjust-window-width (direction step-size)
  "Adjust window width based on position and direction.
DIRECTION should be 'left or 'right."
  (interactive)
  (if (one-window-p t)  ; t means also check for side windows
      (smart-window-message "Cannot resize single window")
    (let ((has-window-left (window-has-window-left-p))
          (has-window-right (window-has-window-right-p)))
      (smart-window-message "Has window left=%s right=%s direction=%s"
                           has-window-left has-window-right direction)
      (cond
       ;; Leftmost window (no window left): left shrinks, right grows
       ((and (not has-window-left) (eq direction 'left))
        (smart-window-message "Leftmost window, shrinking")
        (shrink-window-horizontally step-size))
       ((and (not has-window-left) (eq direction 'right))
        (smart-window-message "Leftmost window, growing")
        (enlarge-window-horizontally step-size))
       ;; Rightmost window (no window right): left grows, right shrinks
       ((and (not has-window-right) (eq direction 'left))
        (smart-window-message "Rightmost window, growing")
        (enlarge-window-horizontally step-size))
       ((and (not has-window-right) (eq direction 'right))
        (smart-window-message "Rightmost window, shrinking")
        (shrink-window-horizontally step-size))
       ;; Middle window: maintain consistent direction
       (t (smart-window-message "Middle window, direction: %s" direction)
          (if (eq direction 'left)
              (shrink-window-horizontally step-size)
            (enlarge-window-horizontally step-size)))))))

;;;###autoload
(defun smart-adjust-window-height (direction step-size)
  "Adjust window height based on position and direction.
DIRECTION should be 'up or 'down."
  (interactive)
  (if (one-window-p t)  ; t means also check for side windows
      (smart-window-message "Cannot resize single window")
    (let ((has-window-above (window-has-window-above-p))
          (has-window-below (window-has-window-below-p)))
      (smart-window-message "Has window above=%s below=%s direction=%s"
                           has-window-above has-window-below direction)
      (cond
       ;; Top window (no window above): up shrinks, down grows
       ((and (not has-window-above) (eq direction 'up))
        (smart-window-message "Top window, shrinking")
        (shrink-window step-size))
       ((and (not has-window-above) (eq direction 'down))
        (smart-window-message "Top window, growing")
        (enlarge-window step-size))
       ;; Bottom window (no window below): up grows, down shrinks
       ((and (not has-window-below) (eq direction 'up))
        (smart-window-message "Bottom window, growing")
        (enlarge-window step-size))
       ((and (not has-window-below) (eq direction 'down))
        (smart-window-message "Bottom window, shrinking")
        (shrink-window step-size))
       ;; Middle window: maintain consistent direction
       (t (smart-window-message "Middle window, direction: %s" direction)
          (if (eq direction 'up)
              (shrink-window step-size)
            (enlarge-window step-size)))))))

;;;###autoload
(defun inspect-window-frame-geometry ()
  "Print all available window and frame geometry information."
  (interactive)
  (let* ((window (selected-window))
         (frame (window-frame window))
         (window-edges (window-edges nil t t))
         (frame-edges (frame-edges nil t))
         (internal-border (frame-internal-border-width))
         (frame-width (frame-pixel-width))
         (frame-height (frame-pixel-height))
         (frame-params (frame-parameters)))
    (with-current-buffer (get-buffer-create "*Window Geometry*")
      (erase-buffer)
      (insert "Window Geometry:\n"
              (format "  window-edges (pixels): %s\n" window-edges)
              (format "  window-inside-pixel-edges: %s\n" (window-inside-pixel-edges))
              (format "  window-pixel-edges: %s\n" (window-pixel-edges))
              (format "  window-total-width (chars): %s\n" (window-total-width))
              (format "  window-total-height (chars): %s\n" (window-total-height))
              (format "  window-pixel-width: %s\n" (window-pixel-width))
              (format "  window-pixel-height: %s\n" (window-pixel-height))
              "\nFrame Geometry:\n"
              (format "  frame-edges (pixels): %s\n" frame-edges)
              (format "  frame-pixel-width: %s\n" frame-width)
              (format "  frame-pixel-height: %s\n" frame-height)
              (format "  internal-border-width: %s\n" internal-border)
              "\nFrame Parameters:\n"
              (format "  right-divider-width: %s\n" (cdr (assq 'right-divider-width frame-params)))
              (format "  left-fringe: %s\n" (cdr (assq 'left-fringe frame-params)))
              (format "  right-fringe: %s\n" (cdr (assq 'right-fringe frame-params)))
              (format "  scroll-bar-width: %s\n" (cdr (assq 'scroll-bar-width frame-params)))
              (format "  border-width: %s\n" (cdr (assq 'border-width frame-params))))
      (pop-to-buffer (current-buffer)))))

;; Auto-compilation
(eval-when-compile
  (require 'cl-lib))

(cl-eval-when (load eval)
  (let* ((this-file (or load-file-name
                        (bound-and-true-p byte-compile-current-file)))
         (this-elc (and this-file
                        (concat (file-name-sans-extension this-file) ".elc"))))
    (when (and this-file
               (file-exists-p this-file)
               (or (not this-elc)
                   (file-newer-than-file-p this-file this-elc)))
      (byte-compile-file this-file))))

(provide 'smart-window-resize)

;;; smart-window-resize.el ends here
