;; -*- lexical-binding: t; -*-
;;; Emacs Configuration Fragment: Bootstrap (should be loaded first)

(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))

(setq package-archive-priorities
      '(("gnu" . 99)
        ("nongnu" . 80)
        ("melpa" . 70)))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Install use-package support
(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-vc-prefer-newest t)
(setq use-package-compute-statistics t)

(use-package cua-base :ensure nil)

(setq custom-file "~/.emacs.d/customs.el")
(add-hook 'after-init-hook (lambda () (load custom-file 'noerror)))

(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; (use-package bind-key :ensure (:wait t))
(use-package compat :ensure t)
(use-package use-package-ensure-system-package :ensure nil)
(use-package delight :ensure t)

;; Get shell env from user shell.
;; https://apple.stackexchange.com/questions/51677/how-to-set-path-for-finder-launched-applications
;; $ sudo launchctl config user path /usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin
;; We need to at least make the /usr/local/bin in the path so that imagemagick can use rsgv rather than its built-in svg renderer.
;; The above command works.
(use-package exec-path-from-shell
  :init
  ;; Specify the environment variables ECA needs
  (setq exec-path-from-shell-variables
        '("ANTHROPIC_API_KEY"
          "OPENAI_API_KEY"
          "OLLAMA_API_BASE"
          "OPENAI_API_URL"
          "ANTHROPIC_API_URL"
          "ECA_CONFIG"
          "XDG_CONFIG_HOME"
          "PATH"
          "MANPATH"))
  ;; For macOS and Linux GUI environments
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


(provide 'config-bootstrap)
