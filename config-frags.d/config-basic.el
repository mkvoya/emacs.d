;; -*- lexical-binding: t; -*-
;;; Emacs Config Fragement: Basic Editing


;; Backups
(setq backup-by-copying t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)
(setq vc-make-backup-files t)
;;(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
;;(setq delete-old-versiojns -1)
;;(setq version-control t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; History
(setq savehist-file "~/.emacs.d/savehist")
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables '(kill-ring search-ring regex-search-ring))

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

;; Undo
(use-package undo-fu :ensure (:host github :repo "emacsmirror/undo-fu")
  :config (setq undo-fu-allow-undo-in-region t))
;; This is vundo is not used
(use-package vundo :ensure (:host github :repo "casouri/vundo"))

(use-package ctrlf :defer t
  :config (ctrlf-mode +1))

;; * Smart Tab
(use-package smart-tab :ensure nil :defer t
  :config
  (smart-tabs-insinuate 'c 'javascript))

(use-package imenu-list
  :bind (("C-\"" . #'imenu-list-smart-toggle))
  :config
  (setq imenu-list-auto-resize nil)
  (setq imenu-list-position 'left))

(use-package crux :defer t)
(use-package ranger :defer t :disabled t)  ; The ranger mode
(use-package vlf :defer t)  ; View large files
;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist :ensure nil :hook (elpaca-after-init . savehist-mode))

;; Check https://emacs-china.org/t/emacs-builtin-mode/11937
;; Winner mode
(use-package winner :ensure nil :hook (elpaca-after-init . winner-mode))

;; Remember the cursor position of files
;; (use-package saveplace :ensure nil :hook (elpaca-after-init . save-place-mode))
(use-package so-long :ensure nil :config (global-so-long-mode 1))

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
(use-package which-key :defer t
  :bind (("C-h ,m" . which-key-show-major-mode)
         ("C-h ,t" . which-key-show-top-level)
         ("C-h ,n" . which-key-show-next-page))
  :init
  (setq which-key-show-remaining-keys t)
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 2)
  (setq which-key-allow-imprecise-window-fit t)
  (setq which-key-sort-order 'which-key-prefix-then-key-order)
  ;; (which-key-setup-minibuffer)
  (which-key-mode))

(use-package ffap :ensure nil :defer t) ; built-in

(use-package visual-replace :defer t
  :bind (("C-c r" . visual-replace)
         :map isearch-mode-map
         ("C-c r" . visual-replace-from-isearch)))

(pixel-scroll-precision-mode t)
(setq pixel-scroll-precision-interpolate-page t)

(use-package ultra-scroll :ensure (:host github :repo "jdtsmith/ultra-scroll")
  :init
  (setq scroll-conservatively 3 ; or whatever value you prefer, since v0.4
        scroll-margin 0)        ; important: scroll-margin>0 not yet supported
  :config
  (ultra-scroll-mode 1))

(setq elisp-fontify-semantically t)

(provide 'config-basic)
