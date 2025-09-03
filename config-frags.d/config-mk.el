;; -*- lexical-binding: t; -*-
;;; Emacs Config Fragement: MK's customized packages


(use-package motd
  :after (dash org)
  :defer 20
  :ensure nil
  :load-path "~/.emacs.d/site-lisp/"
  :config
  (setq motd-background-color "#204230")
  (setq motd-border-color "#444444")
  (setq motd--git-commit-dir "~/Dropbox/Dreams")
  (motd-start-timer)
  )

(use-package xwidget-apps
  :after (websocket)
  :defer t
  :ensure nil
  :load-path "~/.emacs.d/site-lisp/xwidget-apps/"
  :config
  (xwidget-dict-mode)
  )

(use-package emacs-badge
  :demand
  :ensure (:type git :host github :repo "mkvoya/emacs-badge" :files ("*"))
  :config
  (setq emacs-badge-timer
        (run-with-timer
         30 30
         '(lambda()
            (emacs-badge-update (format "%s" (mk/count-today-todos)))))))


(provide 'init-writing)
(load-file "~/.emacs.d/site-lisp/tex-autogen.el")
(load-file "~/.emacs.d/site-lisp/wc.el")

(provide 'config-mk)
