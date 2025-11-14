;; -*- lexical-binding: t; -*-
;;; Emacs Configuration Fragment: Periphery Tools

(use-package shrink-path :ensure (:host github :repo "zbelial/shrink-path.el"))

(use-package doom-modeline
  :after (shrink-path)
  :init (doom-modeline-mode 1)
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
  (setq doom-modeline-project-detection 'auto)

  ;; (setq doom-modeline-window-width-limit 85)
  (setq doom-modeline-height 1) ; optional
  (custom-set-faces
   '(mode-line ((t (:height 0.95))))
   '(mode-line-active ((t (:height 0.95)))) ; For 29+
   '(mode-line-inactive ((t (:height 0.95))))
   '(doom-modeline-evil-emacs-state ((t (:italic nil)))))
  )

(use-package nyan-mode :ensure (:repo "TeMPOraL/nyan-mode")
  :after (doom-modeline)
  :demand t
  :config (nyan-mode))

(use-package ef-themes :ensure (:host github :repo "protesilaos/ef-themes")
  :after (hl-todo doom-modeline)
  :demand t
  :config
  (modus-themes-include-derivatives-mode 1)
  (setq ef-cyprus-palette-overrides '((bg-main "#fdfefd")))
  ;; (setq ef-themes-to-toggle '(ef-cyprus ef-frost))
  (setq ef-themes-headings ; read the manual's entry or the doc string
        '((0 variable-pitch bold 1.1)
          (1 variable-pitch bold 1.1)
          (2 variable-pitch regular 1.1)
          (3 variable-pitch regular 1.1)
          (4 variable-pitch regular 1.1)
          (5 variable-pitch 1.1) ; absence of weight means `bold'
          (6 variable-pitch 1.1)
          (7 variable-pitch 1.1)
          (t variable-pitch 1.1)))
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-italic-constructs t)

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

  ;; Disable all other themes to avoid awkward blending:
  (mapc #'disable-theme custom-enabled-themes)
;;  (ef-themes-select 'ef-cyprus)
  (modus-themes-load-theme 'ef-cyprus)
  )


;; (use-package page-break-lines :ensure t :defer nil)

(use-package dashboard
  :if (< (length command-line-args) 2)
  :diminish dashboard-mode
  :after (org-agenda)
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
  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
  (dashboard-setup-startup-hook))



(provide 'config-theme)
