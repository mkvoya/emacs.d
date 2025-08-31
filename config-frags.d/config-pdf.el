;; -*- lexical-binding: t; -*-
;;; Emacs Config Fragement: PDF files

(use-package pdf-tools
  :init
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :bind (:map pdf-view-mode-map
              ("<wheel-right>" . image-forward-hscroll)
              ("<wheel-left>" . image-backward-hscroll)
              ("<pinch>" . mk/pdf-tools-scale-pinch)
              )
  :config

  (defun mk/pdf-tools-scale-pinch (event)
    "Adjust the height of the default face by the scale in the pinch event EVENT."
    (interactive "e")
    (when (not (eq (event-basic-type event) 'pinch))
      (error "bad event type"))
    (let ((window (posn-window (nth 1 event)))
          (scale (nth 4 event))
          (dx (nth 2 event))
          (dy (nth 3 event))
          (angle (nth 5 event)))
      (with-selected-window window
        (when (< scale 1)
          (pdf-view-shrink 1.1))
        (when (> scale 1)
          (pdf-view-enlarge 1.1)))))
  (pdf-loader-install)
  (add-hook 'pdf-view-mode-hook #'(lambda () (pixel-scroll-precision-mode -1)))
  )

(provide 'config-pdf)
