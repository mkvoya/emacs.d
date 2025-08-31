;; -*- lexical-binding: t; -*-
;;; Emacs Config Fragement: Magit


(use-package cond-let
  :ensure (:host github :repo "tarsius/cond-let"))

;; *The main Magit
(use-package magit
  ;; :disabled t
  :ensure (:host github :repo "magit/magit")
  :after (project transient)
  :defer t
  :init
  (setq magit-diff-refine-hunk t)
  :config
  (use-package magit-extras
    :ensure nil
    :init
    (setq magit-bind-magit-project-status t)
    )
  (add-hook 'magit-diff-mode-hook #'(lambda () (visual-line-mode t)))
  (add-hook 'magit-status-mode-hook #'(lambda () (visual-line-mode t)))
  )
(use-package forge :after (magit emacsql) :defer t)
;; (use-package git-timemachine)

(use-package diff-hl
  :defer t
  :after (magit)
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package git-link :defer t)

;; for git blame
(use-package blamer
  :ensure t
  :bind (("s-i" . blamer-show-commit-info))
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background unspecified
                   :height 110
                   :italic t)))
  :config
  ;; (global-blamer-mode 1)
  )

(provide 'config-magit)
