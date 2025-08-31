;; -*- lexical-binding: t; -*-
;;; Emacs Config Fragement: Programming

;; Citre: Tag jumps
(use-package citre
  :defer t
  :after (evil)
  :init
  ;; This is needed in `:init' block for lazy load to work.
  (require 'citre-config)
  :config
  ;; Bind your frequently used commands.
  (global-set-key (kbd "C-x c j") 'citre-jump)
  (global-set-key (kbd "C-x c J") 'citre-jump-back)
  (global-set-key (kbd "C-x c p") 'citre-ace-peek)
  (global-set-key (kbd "C-]") 'citre-jump)
  (global-set-key (kbd "C-t") 'citre-jump-back)
  (define-key evil-motion-state-map (kbd "C-]") 'citre-jump)
  (define-key evil-motion-state-map (kbd "C-t") 'citre-jump-back)
  (define-key evil-normal-state-map (kbd "C-]") 'citre-jump)
  (define-key evil-normal-state-map (kbd "C-t") 'citre-jump-back)
  (setq citre-project-root-function
        #'(lambda ()
            (when-let* ((project (project-current nil)))
              (expand-file-name (nth 2 project)))))
  )


;; Built-in native line number display
(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode)
  :config
  ;; (setq-default display-line-numbers-width 3)
  )

(use-package ws-butler
  :delight ws-butler-mode
  :config (progn
            ;; adding it to prog-mode-hook causes problems for emacsclient
            (add-hook 'cython-mode-hook     #'ws-butler-mode)
            (add-hook 'LaTeX-mode-hook      #'ws-butler-mode)
            (add-hook 'emacs-lisp-mode-hook #'ws-butler-mode)))
;; * C/C++
;; style I want to use in c++ mode
(c-add-style "my-style"
             '("stroustrup"
               (c-basic-offset . 4)            ; indent by four spaces
               (tab-width . 4)
               (indent-tabs-mode . t)        ; use tabs
               (c-offsets-alist . ((inline-open . 0)  ; custom indentation rules
                                   (brace-list-open . 0)
                                   (innamespace . [0])
                                   (statement-case-open . +)))))

(use-package c-ts-mode
  :ensure nil
  :bind (:map c-ts-base-mode-map
              ("M-<up>" . drag-stuff-up)
              ("M-<down>" . drag-stuff-down)
              ("<home>"  .  malb/beginning-of-line-dwim))
  :hook ((c-ts-base-mode . hs-minor-mode)
         (c-ts-base-mode . display-line-numbers-mode)
         (c-ts-base-mode . ws-butler-mode)
         ;; (c-ts-base-mode . ggtags-mode)
         ;; (c-ts-base-mode . helm-gtags-mode)
         (c-ts-base-mode . clang-format+-mode))
  :init (progn
          (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
          (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
          (add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-ts-mode))))

(use-package clang-format :config (setq clang-format-executable "clang-format"))
(use-package clang-format+ :commands clang-format+-mode)


;; * treesitter
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (setq treesit-font-lock-level 4)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))


(setq python-shell-completion-native-disabled-interpreters nil)


  ;;; Use whitespace (instead of column-marker, column-enforce-mode)
(use-package whitespace
  :ensure nil
  :config
  (setq whitespace-style
        '(face trailing tabs newline tab-mark newline-mark))
  ;; '(face trailing tabs newline tab-mark newline-mark lines-tail)
  (setq whitespace-display-mappings
        '((newline-mark 10 [8617 10])
          (tab-mark 9 [8594 9] [92 9])))
  (set-face-background 'trailing-whitespace "#ffaf5f")
  (set-face-background 'whitespace-trailing "#ffaf5f")
      (set-face-background 'whitespace-tab "#FAFAFA")
  ;; (global-whitespace-mode t)
  (add-hook 'prog-mode-hook 'whitespace-mode)
  )


(use-package gitlab-ci-mode :defer t)
(use-package dockerfile-mode :mode "Dockerfile" :defer t)
(use-package lua-mode :defer)
(use-package swift-mode :disabled t)
(use-package typescript-mode)
(use-package adoc-mode :defer t :ensure (:host github :repo "sensorflo/adoc-mode"))
(use-package elm-mode)
(use-package jinja2-mode :mode "\\.jinja2\\'" :defer t)
(use-package vue-mode :mode "\\.vue\\'" :defer t)
;; Python Support
(use-package flymake-ruff
  :ensure (flymake-ruff :type git :host github :repo "erickgnavar/flymake-ruff")
  :hook (eglot-managed-mode . flymake-ruff-load)
  :config

  ;; (defun my-filter-eglot-diagnostics (diags)
  ;;   "Drop Pyright 'variable not accessed' notes from DIAGS."
  ;;   (list (seq-remove (lambda (d)
  ;;                       (and (eq (flymake-diagnostic-type d) 'eglot-note)
  ;;                            (s-starts-with? "Pyright:" (flymake-diagnostic-text d))
  ;;                            (s-ends-with? "is not accessed" (flymake-diagnostic-text d))))
  ;;                     (car diags))))
  ;; (advice-add 'eglot--report-to-flymake :filter-args #'my-filter-eglot-diagnostics)
  )

(use-package ein :defer t)
(use-package live-py-mode :defer t)
;; Markdown Support
(use-package markdown-mode :defer t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  ;; (setq markdown-command "multimarkdown")
  (setq markdown-command "/usr/local/bin/pandoc")
  (setq markdown-preview-stylesheets (list "https://raw.githubusercontent.com/sindresorhus/github-markdown-css/gh-pages/github-markdown.css"))
  ;;"http://thomasf.github.io/solarized-css/solarized-light.min.css"
  )
(use-package flymd :after (markdown-mode))


(provide 'config-prog)
