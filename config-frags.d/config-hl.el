;; -*- lexical-binding: t; -*-
;;; Emacs Config Fragement: Highlight

(use-package highlight-indent-guides :defer t)
(use-package rainbow-mode
  :defer t
  :config (rainbow-mode t)
  (add-hook 'after-change-major-mode-hook #'rainbow-mode)
  )
(use-package rainbow-delimiters
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package emacs
  :ensure nil
  :config
  (setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "λ")  ; previously ✎
                                         ("#+END_SRC" . "□")
                                         ("#+begin_src" . "λ")
                                         ("#+end_src" . "□")
                                         ("#+begin_quote" . ?»)
                                         ("#+end_quote" . ?«)
                                         ("#+BEGIN_QUOTE" . ?»)
                                         ("#+END_QUOTE" . ?«)
                                         ))
  (global-prettify-symbols-mode)

  (setq-default indicate-buffer-boundaries 'left)
  (setq window-divider-default-right-width 2)
  (setq window-divider-default-bottom-width 2)
  (setq window-divider-default-places t)
  (window-divider-mode 1))

(use-package symbol-overlay
  :hook ((prog-mode html-mode yaml-mode conf-mode) . symbol-overlay-mode)
  :bind (:map symbol-overlay-mode-map
              ("M-i" . symbol-overlay-put)
              ("M-n" . symbol-overlay-jump-next)
              ("M-p" . symbol-overlay-jump-prev)))
(use-package volatile-highlights
  :delight
  :ensure (:host github :repo "k-talo/volatile-highlights.el")
  :config
  ;;-----------------------------------------------------------------------------
  ;; Supporting evil-mode.
  ;;-----------------------------------------------------------------------------
  (vhl/define-extension 'evil 'evil-paste-after 'evil-paste-before
                        'evil-paste-pop 'evil-move)
  (vhl/install-extension 'evil)
  (volatile-highlights-mode t)
  )

(use-package hl-todo :ensure (:host github :repo "tarsius/hl-todo"))

(provide 'config-hl)
