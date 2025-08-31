;; -*- lexical-binding: t; -*-
;;; Emacs Config Fragement: MK's customized packages




(use-package motd
  :after (dash)
  :ensure nil
  :load-path "~/.emacs.d/site-lisp/"
  :config
     ;; (progn
     ;;   (setq motd-background-color "#EFE0C0")
      ;;  (setq motd-border-color "#910471")
      ;;  )
    (setq motd-background-color "#204230")
    (setq motd-border-color "#444444")

  (setq motd--git-commit-dir "~/Dropbox/Dreams")
  (motd-start-timer)
  )
(use-package xwidget-apps
  :after (websocket)
  :ensure nil
  :load-path "~/.emacs.d/site-lisp/xwidget-apps/"
  :config
  (xwidget-dict-mode)
  )

(use-package emacs-badge
  :defer t
  :ensure (:type git :host github :repo "mkvoya/emacs-badge" :files ("*"))
  :config
  (require 'emacs-badge)
  (setq emacs-badge-timer
        (run-with-timer
         30 30
         '(lambda()
            (emacs-badge-update (format "%s" (mk/count-today-todos))))))

  )


(provide 'init-writing)
(load-file "~/.emacs.d/site-lisp/tex-autogen.el")
(load-file "~/.emacs.d/site-lisp/wc.el")

(provide 'config-mk)
