;;; init.el --- -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:


(setq gc-cons-threshold (* 4 (expt 2 20))
      gc-cons-percentage 0.6)

;; 将lisp目录放到加载路径的前面以加快启动速度
(add-to-list 'load-path (locate-user-emacs-file "config-frags.d"))

(setq global-auto-revert-mode nil) ; hack for bug
(setq savehist-mode nil) ; hack for bug

(require 'config-bootstrap)

;; The server part may not be configured so early.
(require 'server) ; Load and start server if it's not running
(unless (server-running-p) (server-start))

(require 'config-hl)

(require 'config-basic)     ; basic setups first
(require 'config-cjk)       ; we need to setup fonts early
(require 'config-theme)     ; set theme early
(require 'config-mx)

(require 'config-autocomp)
(require 'config-autofix)
(require 'config-auctex)
(require 'config-bib)
(require 'config-bindings)
(require 'config-cal)

(require 'config-dired)
(require 'config-evil)
(require 'config-feed)

(require 'config-icons)
(require 'config-lsp)
(require 'config-magit)
(require 'config-mk)
(require 'config-nsgui)
(require 'config-org)
(require 'config-pdf)
(require 'config-periphery)
(require 'config-prog)
(require 'config-term)
(require 'config-tramp)
(require 'config-webkit)
(require 'config-ai)
(require 'config-misc)

(setq gc-cons-threshold (expt 2 28)
      gc-cons-percentage 0.3)

(message "Everything is up. Wish you a nice day. :)")
(put 'narrow-to-region 'disabled nil)

(provide 'init)
;;; init.el ends here
