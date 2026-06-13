;; -*- lexical-binding: t; -*-
;;; Emacs Config Fragement: LSP

(use-package eglot :after (exec-path-from-shell)
  :preface
  (defun mk/eglot-ensure ()
    (unless (file-remote-p (buffer-file-name (current-buffer)))
      (exec-path-from-shell-initialize))
    (eglot-ensure))
  :hook (
         (rust-mode . mk/eglot-ensure)
         (go-mode   . mk/eglot-ensure)
         (python-base-mode . mk/eglot-ensure)
         )
  :config
  (eglot-inlay-hints-mode -1)
  (add-to-list 'eglot-server-programs '(LaTeX-mode . ("texlab")))
  (add-to-list 'eglot-server-programs '(c-ts-mode . ("clangd")))
  (add-to-list 'eglot-server-programs '(c++-ts-mode . ("clangd")))
  (setenv "LANG" "en_US.UTF-8")
  (setenv "LC_ALL" "en_US.UTF-8")
  (add-to-list 'process-coding-system-alist '("texlab" . utf-8-unix))
  )
(use-package eldoc-box :after eglot
  :hook (eglot-managed-mode . eldoc-box-hover-at-point-mode)
                                        ;(eglot-managed-mode . eldoc-box-hover-mode)
  :custom (eldoc-box-clear-with-C-g t)
  ;; (eldoc-box-offset 1)
  :config (add-to-list 'eldoc-box-frame-parameters '(alpha . 0.90))
  )
(use-package eglot-x :vc (:url "https://github.com/nemethf/eglot-x.git")
  :after eglot
  :config (eglot-x-setup)
  )

(use-package eglot-booster :vc (:url "https://github.com/jdtsmith/eglot-booster.git")
  :after eglot
  :config
  (add-to-list 'process-coding-system-alist '("emacs-lsp-booster" . utf-8-unix))
  (add-to-list 'process-coding-system-alist '(".*\\(booster\\).*" . (utf-8-unix . utf-8-unix)))
  (setenv "LANG" "en_US.UTF-8")
  (setenv "LC_ALL" "en_US.UTF-8")
  (setq eglot-booster-io-only t)
  (eglot-booster-mode +1)
  )
(use-package consult-eglot :after eglot)

(provide 'config-lsp)
