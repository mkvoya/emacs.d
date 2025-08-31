;; -*- lexical-binding: t; -*-
;;; Emacs Config Fragement: Tramp


(use-package tramp
  :ensure nil ; built in
  :init
  (use-package tramp-sh :ensure nil :defer t)
  ;; (setq tramp-debug-buffer t)
  (setq tramp-verbose 10)
  (setq tramp-ssh-controlmaster-options
        (concat
         "-o ControlPath=/Volumes/ramfs/ssh-ControlPath-%%r@%%h:%%p "
         "-o ControlMaster=auto -o ControlPersist=yes"))

  :defer t
  :config

  (setq tramp-remote-path
        (append tramp-remote-path
                '(tramp-own-remote-path)))

  ;; Speedup the C++ file over Tramp.
  (setq remote-file-name-inhibit-cache nil)
  (setq vc-ignore-dir-regexp
        (format "%s\\|%s"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))
  (setq tramp-verbose 1)
  )

(provide 'config-tramp)
