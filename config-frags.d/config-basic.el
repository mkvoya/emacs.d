;; -*- lexical-binding: t; -*-
;;; Emacs Config Fragement: Basic Editing

(setq
 ;; Backups
 backup-by-copying t
 backup-directory-alist '(("." . "~/.emacs.d/backups"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t
 vc-make-backup-files t
 ;;(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
 ;;(setq delete-old-versiojns -1)
 ;;(setq version-control t)
 auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t))
 ;; History
 savehist-file "~/.emacs.d/savehist"
 history-length t
 history-delete-duplicates t
 savehist-save-minibuffer-history 1
 savehist-additional-variables '(kill-ring search-ring regex-search-ring))
;; (savehist-mode 1)
(setq-default
 ;; From: https://stackoverflow.com/questions/4657142/how-do-i-encourage-emacs-to-follow-the-compilation-buffer
 compilation-scroll-output t
 ;; (setq compilation-scroll-output 'first-error)
 ;; Prevent Extraneous Tabs
 indent-tabs-mode nil
 fill-column 100
 ;; line-spacing 0.1
 )
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)
(defun mkvoya/better-wrap ()
  "Make the word wrap better."
  (interactive)
  (progn
    (visual-line-mode t)
    ;; (setq word-wrap nil)
    ))

(blink-cursor-mode 1)

(setq delete-by-moving-to-trash t)

;; Let's disable the precision mode since it brings some troubles with the precision mode
;; (pixel-scroll-precision-mode)
(setq scroll-preserve-screen-position 'always)

;; Mouse scrolling in terminal emacs
(unless (display-graphic-p)
  ;; activate mouse-based scrolling
  ;; ensure mouse
  (xterm-mouse-mode t)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

(use-package undo-fu
  :ensure (:host github :repo "emacsmirror/undo-fu")
  :config
  (setq undo-fu-allow-undo-in-region t))

(use-package vundo
  :ensure (:host github :repo "casouri/vundo"))

(use-package ctrlf :defer t
  :config (ctrlf-mode +1))

;; * Smart Tab
(use-package smart-tab :ensure nil :defer t
  :config
  (smart-tabs-insinuate 'c 'javascript))

(use-package imenu-list
  :demand
  :after (org)
  :bind (("C-\"" . #'imenu-list-smart-toggle))
  :config
  (setq imenu-list-auto-resize nil)
  (setq imenu-list-position 'left)
  (setq org-imenu-depth 5)
  )


(use-package crux :defer t)
(use-package ranger :defer t :disabled t)  ; The ranger mode
(use-package vlf :defer t)  ; View large files
;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist :ensure nil :hook (elpaca-after-init . savehist-mode))

;; Check https://emacs-china.org/t/emacs-builtin-mode/11937
;; Winner mode
(use-package winner :ensure nil :hook (elpaca-after-init . winner-mode))
;; Highlight current line
(use-package hl-line :ensure nil :hook (elpaca-after-init . global-hl-line-mode))

;; Remember the cursor position of files
;; (use-package saveplace :ensure nil :hook (elpaca-after-init . save-place-mode))
(use-package so-long :ensure nil :config (global-so-long-mode 1))

(use-package paren :ensure nil :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)
  (show-paren-mode))
(use-package simple :ensure nil
  :hook (elpaca-after-init . (lambda ()
                               (line-number-mode)
                               (column-number-mode)
                               (size-indication-mode)
                               ;; better line wrapping for cjk. Try =toggle-word-wrap=
                               ;; (setq-default word-wrap nil)
                               ;; (setq word-wrap nil)
                               )))

(modify-syntax-entry ?_ "w")

;; * which-key: displays available keybindings in popup
(use-package which-key
  :bind (
         ("C-h ,m" . which-key-show-major-mode)
         ("C-h ,t" . which-key-show-top-level)
         ("C-h ,n" . which-key-show-next-page)
         )
  :defer t
  :init
  (setq which-key-show-remaining-keys t)

  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 2)
  (setq which-key-allow-imprecise-window-fit t)
  (setq which-key-sort-order 'which-key-prefix-then-key-order)
  ;; (which-key-setup-minibuffer)
  (which-key-mode)
  )

(use-package ffap
  :ensure nil ; built-in
  :defer t)

(use-package visual-replace
   :defer t
   :bind (("C-c r" . visual-replace)
          :map isearch-mode-map
          ("C-c r" . visual-replace-from-isearch)))

(provide 'config-basic)
