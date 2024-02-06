;;; Old configurations for the top bar patch.

(defun simple-top-bar-render (left right)
  "Return a string of `frame-width' length. Containing LEFT, and RIGHT aligned respectively."
  (let ((available-width (- (frame-width top-bar-current-frame)
                            (+ (length (format-mode-line left))
                               (length (format-mode-line right))
                               )
                            15)))
    (append left
            (list (format (format "%%%ds" available-width) ""))
            right)))

(defun mk/set-frame-top-bar-format (format)
  (dolist (frame (frame-list))
    (set-frame-parameter frame 'top-bar-format format)
    )
  )

(defvar mk/default-frame-top-bar-format
  '((:eval
     (simple-top-bar-render
      ;; Left.
      `("           "
        "MK's EMACS "
        ,(format "[%s]"
                 (buffer-name
                  (window-buffer
                   (get-mru-window top-bar-current-frame)))))
      ;; Right.
      '(mode-line-misc-info
        )))))
;; (mk/set-frame-top-bar-format mk/default-frame-top-bar-format)

(add-to-list
 'default-frame-alist
 `(
   (top-bar-lines . 1)                  ; Enable top bar
   (top-bar-format . ,mk/default-frame-top-bar-format)   ; Enable top bar
 ))

(when (eq system-type 'darwin)

  (require 'top-bar)

  (defun mk/enable-top-bar (frame)
    "Setup top-bar for FRAME."
    (set-frame-parameter frame 'top-bar-lines 1)
    (mk/set-frame-top-bar-format mk/default-frame-top-bar-format)
    )
  (defun mk/disable-top-bar (frame)
    "Setup top-bar for FRAME."
    (set-frame-parameter frame 'top-bar-lines 0)
    )

  ;; MK: DISABLE topbar
  ;; (mk/enable-top-bar nil)
  ;; (add-hook 'after-make-frame-function #'mk/enable-top-bar)

  (set-face-attribute 'top-bar nil :background "#EFEFEF" :font "Menlo-12") ;; default
  ;; (set-face-attribute 'top-bar nil :background "#2F2F2F" :foreground "#fFeF5F" :font "Menlo-12")
  )
(provide 'init-topbar)
