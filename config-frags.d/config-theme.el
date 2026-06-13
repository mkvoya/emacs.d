;; -*- lexical-binding: t; -*-
;;; Emacs Configuration Fragment: Periphery Tools

(use-package shrink-path :vc (:url "https://github.com/zbelial/shrink-path.el.git"))

(use-package doom-modeline
  :after (shrink-path nerd-icons)
  :init
  (display-battery-mode +1)
  (doom-modeline-mode +1)
  :config
  (setq inhibit-compacting-font-caches t)
  (setq mode-line-right-align-edge 'right-fringe)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-buffer-modification-icon t)
  (setq doom-modeline-time t)
  (setq doom-modeline-time-icon t)
  (setq doom-modeline-time-live-icon t)
  (setq doom-modeline-time-analogue-clock t)
  (setq doom-modeline-time-clock-size 0.7)
  (setq doom-modeline-percent-position '(-3 "%p"))
  (setq doom-modeline-position-column-line-format '("%l:%c"))
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-enable-word-count nil)
  (setq doom-modeline-modal t)
  (setq doom-modeline-modal-icon nil)
  (setq doom-modeline-modal-modern-icon t)
  (setq doom-modeline-unicode-fallback t)
  (setq doom-modeline-enable-word-count t)
  (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
  (setq doom-modeline-always-show-macro-register t)
  (setq doom-modeline-support-imenu t)
  (setq doom-modeline-bar-width 2)
  (setq doom-modeline-hud nil)
  (display-battery-mode +1)
  (setq doom-modeline-battery t)
  (setq doom-modeline-project-detection 'auto)

  ;; (setq doom-modeline-window-width-limit 85)
  (setq doom-modeline-height 1) ; optional
  (custom-set-faces
   '(mode-line ((t (:height 0.95))))
   '(mode-line-active ((t (:height 0.95)))) ; For 29+
   '(mode-line-inactive ((t (:height 0.95))))
   '(doom-modeline-evil-emacs-state ((t (:italic nil)))))
  )

(use-package nyan-mode :vc (:url "https://github.com/TeMPOraL/nyan-mode.git")
  :after (doom-modeline)
  :disabled t
  :demand t
  :config (nyan-mode))

(use-package ef-themes
  :ensure t
  :after (hl-todo doom-modeline)
  :init
  ;; This makes the Modus commands listed below consider only the Ef
  ;; themes.  For an alternative that includes Modus and all
  ;; derivative themes (like Ef), enable the
  ;; `modus-themes-include-derivatives-mode' instead.  The manual of
  ;; the Ef themes has a section that explains all the possibilities:
  ;;
  ;; - Evaluate `(info "(ef-themes) Working with other Modus themes or taking over Modus")'
  ;; - Visit <https://protesilaos.com/emacs/ef-themes#h:6585235a-5219-4f78-9dd5-6a64d87d1b6e>
  (ef-themes-take-over-modus-themes-mode 1)
  :bind
  (("<f5>" . modus-themes-rotate)
   ("C-<f5>" . modus-themes-select)
   ("M-<f5>" . modus-themes-load-random))
  :config
  ;; All customisations here.
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-italic-constructs t)

  ;; Finally, load your theme of choice (or a random one with
  ;; `modus-themes-load-random', `modus-themes-load-random-dark',
  ;; `modus-themes-load-random-light').
  ;; (modus-themes-load-theme 'ef-summer)
  (modus-themes-load-theme 'ef-duo-light)
  )

;; (modus-themes-load-theme 'ef-cyprus)
;; (setq ef-cyprus-palette-overrides '((bg-main "#fdfefd")))

  (defun my-ef-themes-hl-todo-faces ()
    "Configure `hl-todo-keyword-faces' with Ef themes colors.
        The exact color values are taken from the active Ef theme."
    (ef-themes-with-colors
      (setq hl-todo-keyword-faces
            `(("HOLD" . ,yellow)
              ("TODO" . ,red)
              ("NEXT" . ,blue)
              ("THEM" . ,magenta)
              ("PROG" . ,cyan-warmer)
              ("OKAY" . ,green-warmer)
              ("DONT" . ,yellow-warmer)
              ("FAIL" . ,red-warmer)
              ("BUG" . ,red-warmer)
              ("DONE" . ,green)
              ("NOTE" . ,blue-warmer)
              ("KLUDGE" . ,cyan)
              ("HACK" . ,cyan)
              ("TEMP" . ,red)
              ("FIXME" . ,red-warmer)
              ("XXX+" . ,red-warmer)
              ("REVIEW" . ,red)
              ("DEPRECATED" . ,yellow)))))
  ;; (add-hook 'ef-themes-post-load-hook #'my-ef-themes-hl-todo-faces)


;; (use-package nano-theme :vc (:url "https://github.com/rougier/nano-theme.git")
;;   :config (load-theme 'nano t))

;; (use-package nano-theme :vc (:url "https://github.com/rougier/nano-emacs.git") :main "nano-theme.el")


;; (use-package page-break-lines :ensure t :defer nil)

(use-package dashboard
  :if (< (length command-line-args) 2)
  :diminish dashboard-mode
;;  :after (recentf)
  :ensure t
  :config
  (setq dashboard-banner-logo-title "What a nice day!")
  ;;(setq dashboard-startup-banner "/path/to/image")
  (setq dashboard-projects-backend 'project-el)
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)
                          ;; (registers . 5)
                          ))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  ;; (setq dashboard-display-icons-p t)     ; display icons on both GUI and terminal
  ;; (setq dashboard-icon-type 'nerd-icons) ; use `nerd-icons' package
  ;; (setq dashboard-center-content t)
  ;; (setq dashboard-vertically-center-content t)
  (setq dashboard-agenda-sort-strategy '(time-up todo-state-up))
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (add-hook 'after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'after-init-hook #'dashboard-initialize)
  (dashboard-setup-startup-hook))



(provide 'config-theme)
