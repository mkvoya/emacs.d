;; -*- lexical-binding: t; -*-
;;; Emacs Config Fragement: M-x

(use-package vertico
  :init
  (setq vertico-scroll-margin 0)
  (setq vertico-count 20)  ; Show more candidates.
  (setq vertico-resize nil)  ; Do NOT grow and shrink the Vertico minibuffer.
  (setq vertico-cycle t)  ; Cycling the candidates. E.g., the next to the last is the first.

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (setq enable-recursive-minibuffers nil)
  (vertico-mode)
  )

(use-package orderless
  :defer t
  :init
  (setq completion-styles '(orderless))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :defer t
  :init (marginalia-mode))

(use-package rg
  :defer t
  :ensure-system-package
  (rg . ripgrep))

(use-package recentf :ensure nil :defer t)
(use-package consult
  :after (recentf)
  :bind (
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ;; Other custom bindings
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch

  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; Configure the register formatting
  (setq register-preview-delay 0)
  (setq register-preview-function #'consult-register-format)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  )


(use-package embark
  :defer t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(provide 'config-mx)
