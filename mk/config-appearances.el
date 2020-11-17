;;; package -- summary
;;; Commentary:
;;;
;;; Theme

;;; Code:
;; ;; Dark and Light theme
;; (use-package heaven-and-hell
;;   :ensure t
;;   :init
;;   (setq heaven-and-hell-theme-type 'dark) ;; Omit to use light by default
;;   (setq heaven-and-hell-themes
;;         '((light . tsdh-light)
;;           (dark . tsdh-dark))) ;; Themes can be the list: (dark . (tsdh-dark wombat))
;;   ;; Optionall, load themes without asking for confirmation.
;;   (setq heaven-and-hell-load-theme-no-confirm t)
;;   :hook (after-init . heaven-and-hell-init-hook)
;;   :bind (("C-c <f6>" . heaven-and-hell-load-default-theme)
;;          ("<f6>" . heaven-and-hell-toggle-theme)))

(set-face-background 'hl-line "#e9e9e9")
(set-face-foreground 'highlight nil)

(provide 'config-appearances)
;;; config-appearances.el ends here
