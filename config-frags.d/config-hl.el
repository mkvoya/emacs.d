;; -*- lexical-binding: t; -*-
;;; Emacs Configuration Fragment: Highlight


;; Highlight current line
(use-package hl-line :ensure nil
  :hook (elpaca-after-init . global-hl-line-mode))

;; Highlight todo
(use-package hl-todo :ensure (:host github :repo "tarsius/hl-todo")
  :custom (hl-todo-color-background t))

;; Highlight matching parenthesis
(use-package paren :ensure nil
  :config
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t)
  (show-paren-mode))

;; Highlight indentations
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode))

;; Show color codes
(use-package rainbow-mode
  :hook ((prog-mode org-mode) . rainbow-mode))

;; Show color delimiters like parenthesis
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package emacs :ensure nil
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

(use-package volatile-highlights :ensure (:host github :repo "k-talo/volatile-highlights.el")
  :config
  ;;-----------------------------------------------------------------------------
  ;; Supporting evil-mode.
  ;;-----------------------------------------------------------------------------
  (vhl/define-extension 'evil 'evil-paste-after 'evil-paste-before
                        'evil-paste-pop 'evil-move)
  (vhl/install-extension 'evil)
  (volatile-highlights-mode t)
  )

(provide 'config-hl)
