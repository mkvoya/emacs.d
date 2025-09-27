;; -*- lexical-binding: t; -*-
;;; Emacs Config Fragement: MK's customized packages

(use-package async-cmd
  :ensure (:type git :host github :repo "mkvoya/async-cmd.el"))


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
  :commands (xwidget-dict-lookup-at-point)
  :load-path "~/.emacs.d/site-lisp/xwidget-apps/"
  :config
  (xwidget-dict-mode)
  )

(defun mk/count-today-todos ()
  "Count the number of today's to-do tasks."
  (interactive)
  (let ((count 0)
        (today (format-time-string "%Y-%m-%d")))
    (when (fboundp 'org-compile-prefix-format)
      (org-compile-prefix-format 'todo)
      (org-map-entries
       (lambda ()
         (when (and (string= (org-get-todo-state) "TODO")
                    (or (string= (org-entry-get nil "SCHEDULED") today)
                        (and (org-entry-get nil "DEADLINE")
                             (string< today (org-entry-get nil "DEADLINE")))
                        ))
           (setq count (1+ count))))
       nil 'agenda))
    count)
  )

(defun mk/count-weekly-todos ()
  "Count the number of TODO entries in an Org FILE using org-map-entries."
  (with-current-buffer (find-file-noselect "~/Dropbox/Dreams/Org/Tasks.org")
    (org-with-wide-buffer
     (length (org-map-entries t "/+TODO")))))


(use-package emacs-badge
  :demand
  :ensure (:type git :host github :repo "mkvoya/emacs-badge" :files ("*"))
  :config
  (setq emacs-badge-timer
        (run-with-timer
         30 30
         '(lambda()
            (emacs-badge-update (format "%s" (mk/count-weekly-todos)))))))


(provide 'init-writing)
(load-file "~/.emacs.d/site-lisp/tex-autogen.el")
(load-file "~/.emacs.d/site-lisp/wc.el")

(provide 'config-mk)
