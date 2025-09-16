;; -*- lexical-binding: t; -*-
;;; Emacs Config Fragement: LSP

(use-package eglot :after (exec-path-from-shell)
  :preface
  (defun mk/eglot-ensure ()
    (exec-path-from-shell-initialize)
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
  )
(use-package eldoc-box :after eglot
  :hook (eglot-managed-mode . eldoc-box-hover-at-point-mode)
                                        ;(eglot-managed-mode . eldoc-box-hover-mode)
  :custom (eldoc-box-clear-with-C-g t)
  ;; (eldoc-box-offset 1)
  :config (add-to-list 'eldoc-box-frame-parameters '(alpha . 0.90))
  )
(use-package eglot-x :ensure (:host github :repo "nemethf/eglot-x")
  :after eglot
  :config (eglot-x-setup)
  )

(use-package eglot-booster :ensure (:host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode)
  )
(use-package consult-eglot :after eglot)

(provide 'config-lsp)
