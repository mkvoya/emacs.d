;;; init.el --- Initialization file for eMaKs
;;; Commentary:
;;;   Emacs Startup File --- initialization for Emacs

;;; Code:
;; (load "server") ; Load and start server if it's not running
;; (unless (server-running-p) (server-start))

;;; Optimized according to http://blog.lujun9972.win/emacs-document/blog/2019/03/15/降低emacs启动时间的高级技术/index.html

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 100 1000 1000))

(eval-when-compile
  (require 'use-package))

(use-package package
  :config
  ;; (require 'package)
  (setq package-enable-at-startup nil)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")) ;;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-initialize)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  )

;;; Special Keys for MacOS GUI
;;(setq mac-command-modifier 'meta) ; Switch Cmd and Opt(Alt)
;;(setq mac-option-modifier 'super) ; Switch Cmd and Opt(Alt)

;;; (require 'diminish); => use delight
(use-package bind-key)

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;;; Backups
(setq
 backup-by-copying t
 backup-directory-alist '(("." . "~/.emacs.d/backups"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t
 vc-make-backup-files t)
;(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
;(setq delete-old-versiojns -1)
;(setq version-control t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;;; History
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring search-ring regex-search-ring))

;;; Window
(tool-bar-mode -1) ; close tool bar (-1 is switch)
(menu-bar-mode -1) ; close menu bar
(scroll-bar-mode -1) ; close the scroll bar
(global-yascroll-bar-mode 1); Yet Another scroll bar

; Winner mode
;(use-package winner :defer t)
(if (fboundp 'winner-mode)
    (progn
      (winner-mode 1)
      (message "Winner-mode enabled"))
  (message "No Winner-Mode, Skip"))

; Sentence
(setq sentence-end-double-space nil) ; Use only one space to end a sentence

;; Mode line format
;; Instead of powerline
(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'respectful)
  ;; (setq sml/theme 'light)
  (setq sml/shorten-modes t)
  (setq sml/shorten-directory t)
  (sml/setup)
  )

; lazy answer
(fset 'yes-or-no-p 'y-or-n-p)

;;minibuffer editing
;(use-package miniedit
;  :ensure t
;  :defer t
;  :commands minibuffer-edit
;  :init (miniedit-install))

;; light-on-dark color scheme
;(defadvice color-theme-alist (around sacha activate)
;  (if (ad-get-arg 0)
;      ad-do-it
;    nil))
;(use-package color-theme :ensure t)
;(use-package color-theme-solarized :ensure t)
;(defun my/setup-color-theme ()
;  (interactive)
;  (color-theme-solarized-dark)
;  (set-face-foreground 'secondary-selection "darkblue")
;  (set-face-background 'secondary-selection "lightblue")
;  (set-face-background 'font-lock-doc-face "black")
;  (set-face-foreground 'font-lock-doc-face "wheat")
;  (set-face-background 'font-lock-string-face "black")
;  (set-face-foreground 'org-todo "green")
;  (set-face-background ' org-todo "black"))
;
;;(eval-after-load 'color-theme (my/setup-color-theme))


(use-package undo-tree
  :ensure t
  :defer t
  :delight
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

;; which-key is a fork of guide-key
(use-package which-key
  :ensure t
  :defer t
  :config
  (which-key-mode)
  (which-key-setup-minibuffer))

; UTF-8
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPUND_TEXT TEXT STRING)))


; Unicode
(defmacro my/insert-unicode (unicode-name)
  `(lambda () (interactive)
     (insert-char (cdr (assoc-string, unicode-name (ucs-names))))))
(bind-key "C-x 8 s" (my/insert-unicode "ZERO WIDTH SPACE"))
(bind-key "C-x 8 S" (my/insert-unicode "SNOWMAN"))
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

; Clean up spaces
;(bind-key "M-SPC" 'cycle-spacing)

; Show column number
(column-number-mode 1)

(use-package yasnippet
  :ensure t
  :defer t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after (yasnippet)
  :ensure t)

; Autocomplete
(use-package company
  :ensure t
  :defer t
  :config
  ;(add-hook 'prog-mode-hook 'company-mode)
  (setq company-dabbrev-downcase nil
        company-show-numbers t
        company-minimum-prefix-length 2)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode 1)
  ;; (company-statistics-mode 1)
  ;; (require 'company-emoji)
  (setq company-backends
        '((company-files
           company-keywords
           company-capf
           company-yasnippet
           company-ispell
           company-lsp
           )
          (company-abbrev company-dabbrev)))
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "\C-n") #'company-select-next)
    (define-key company-active-map (kbd "\C-p") #'company-select-previous)
    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)
    (define-key (make-sparse-keymap) [down-mouse-1] 'ignore)
    (define-key (make-sparse-keymap) [down-mouse-3] 'ignore)
    (define-key (make-sparse-keymap) [mouse-1] 'company-complete-mouse)
    (define-key (make-sparse-keymap) [mouse-3] 'company-select-mouse)
    (define-key (make-sparse-keymap) [up-mouse-1] 'ignore)
    (define-key (make-sparse-keymap) [up-mouse-3] 'ignore)
    )
  (advice-add 'company-complete-common :before (lambda ()
                                                 (setq my-company-point (point))))
  (advice-add 'company-complete-common :after (lambda ()
                                                (when (equal my-company-point (point)) (yas-expand))))
  )

(use-package company-auctex
  :ensure t
  :after (company)
  :config
  (company-auctex-init))
(use-package company-reftex
  :ensure t
  :after (company)
  :config
  (add-to-list 'company-backends 'company-reftex-labels)
  (add-to-list 'company-backends 'company-reftex-citations))


;; Powerline, airline, smart-mode-line
;; ; Powerline
;; (use-package powerline
;;   :ensure t
;;   :config
;;   (powerline-center-evil-theme)
;;   (setq powerline-default-separator 'utf-8)
;;   (setq powerline-utf-8-separator-left #x27bd)
;;   (setq powerline-utf-8-separator-right #x2b05)
;;   (setq-default powerline-height (truncate (* 0.6 (frame-char-height))))
;;   )

;; (setq line-number-mode 0)

(setq linum-format "%d ")
;; (global-linum-mode t)
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'ps-mode-hook 'doc-view-toggle-display)

;; (load-theme 'manoj-dark)
(load-theme 'doom-one-light t)

(setq c-default-style "linux"
      c-basic-offset 8)
(setq-default c-basic-offset 8
              tab-width 8
              indent-tabs-mode t)

; Whitespace[built-in], check: http://ergoemacs.org/emacs/whitespace-mode.html
(use-package whitespace
  :config
  (setq whitespace-style
        '(face trailing tabs newline tab-mark newline-mark))
  ;; '(face trailing tabs newline tab-mark newline-mark lines-tail))
  (setq whitespace-display-mappings
        '((newline-mark 10 [8617 10])
          (tab-mark 9 [8594 9] [92 9])))
  (set-face-background 'trailing-whitespace "#ffaf5f")
  (set-face-background 'whitespace-trailing "#ffaf5f")
  (global-whitespace-mode t)
  )

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

; style I want to use in c++ mode
(c-add-style "my-style"
             '("stroustrup"
               (c-basic-offset . 4)            ; indent by four spaces
               (tab-width . 4)
               (indent-tabs-mode . t)        ; use tabs
               (c-offsets-alist . ((inline-open . 0)  ; custom indentation rules
                                   (brace-list-open . 0)
                                   (innamespace . [0])
                                   (statement-case-open . +)))))

(defun my-c++-mode-hook ()
  (c-set-style "my-style"))        ; use my-style defined above

(add-hook 'c++-mode-hook 'my-c++-mode-hook)


(use-package monokai-theme
  :defer t
  :config
;;  (load-theme 'monokai t)
;;  (setq monokai-background "#080C14")
  )
(use-package grandshell-theme
  :defer t
  :config
;;  (load-theme 'grandshell t)
  )
(use-package alect-themes
  :defer t
  :config
;;  (load-theme 'alect-black t)
  )

;;(use-package spaceline-config
;;  :ensure t
;;  :config
;;  (spaceline-spacemacs-theme))

;; (use-package airline-themes
;;   :ensure t
;;   :config
;;   (load-theme 'airline-light t))

;;; Better evil
(use-package evil
  :ensure t
  :after (smart-mode-line)
  :config
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (require 'evil-numbers)
  (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-S-a") 'evil-numbers/dec-at-pt)
;;; Evil rebind
  ;; :q should kill the current buffer rather than quitting emacs entirely
  (defun mkvoya/ex-quit ()
    "Evil ex quit."
    (interactive)
    (if (one-window-p "visible")
        (kill-this-buffer)
      (evil-window-delete)))
  (evil-ex-define-cmd "q" #'mkvoya/ex-quit)
  ;; (evil-ex-define-cmd "q" 'mkvoya/betterq)
  ;; Need to type out :quit to close emacs
  (evil-ex-define-cmd "quit" 'evil-quit)
  ;; (setq evil-emacs-state-cursor '("SkyBlue2" bar))
  (setq evil-emacs-state-cursor '(hollow))
  (evil-mode 1))

 ;; '(auto-dark-emacs/dark-theme 'manoj-dark)
 ;; '(auto-dark-emacs/light-theme 'doom-one-light)
 ;; '(auto-dark-emacs/polling-interval-seconds 600)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-source-correlate-method 'synctex)
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(column-number-mode t)
 '(compilation-message-face 'default)
 '(custom-safe-themes
   '("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" default))
 '(fci-rule-color "#3C3D37")
 '(global-yascroll-bar-mode t)
 '(highlight-changes-colors '("#FD5FF0" "#AE81FF"))
 '(highlight-tail-colors
   '(("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100)))
 '(jdee-db-active-breakpoint-face-colors (cons "#f0f0f0" "#4078f2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#f0f0f0" "#50a14f"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#f0f0f0" "#9ca0a4"))
 '(magit-diff-use-overlays nil)
 '(menu-bar-mode nil)
 '(objed-cursor-color "#e45649")
 '(org-agenda-files
   '("~/Dropbox/Dreams/org/Plans.org" "~/Dropbox/Dreams/org/IPADS.sched.org" "~/Dropbox/Dreams/Projects/DNA/DNA材料-持续更新/Survey.org" "~/Dropbox/Dreams/org/Inbox.org" "~/Dropbox/Dreams/org/main.org"))
 '(org-clock-mode-line-total 'current)
 '(package-selected-packages
   '(esup ns-auto-titlebar pyim beacon smart-cursor-color org-roam-bibtex org-noter-pdftools org-noter org-roam ctrlf consult-flycheck consult-selectrum selectrum-prescient selectrum marginalia dap-mode org-ref mu4e-alert evil-mu4e org-caldav org-wild-notifier dired-launch calfw-org org-time-budgets org-timeline calfw git-timemachine centaur-tabs rainbow-mode delight nameframe-perspective org-alert languagetool dired-sidebar maple-explorer company-lsp peep-dired auto-complete-auctex reveal-in-osx-finder webkit-color-picker zenity-color-picker wucuo langtool smex ebib cdlatex company-auctex company-reftex nameframe-projectile nameframe rg projectile-ripgrep org-sidebar svg-tag-mode quelpa-use-package quelpa ssh vs-light-theme color-theme-sanityinc-tomorrow hemisu-theme heaven-and-hell ov svg-clock vlf projectile-sift projectile dashboard which-key-posframe smart-mode-line exec-path-from-shell rainbow-delimiters rainbow-blocks all-the-icons kaolin-themes doom-themes atom-one-dark-theme telega pdf-tools org-superstar jinja2-mode csv-mode sdcv posframe unicode-fonts flymd diff-hl helm-descbinds buttons texfrag evil-numbers smart-tabs-mode smart-tab cheatsheet org-d20 jumblr 2048-game yascroll zone-nyan markdown-toc markdown-preview-mode markdown-mode+ org-agenda-property dired-ranger ## synonymous define-word auctex evil-magit magit neotree flycheck-status-emoji flycheck-color-mode-line flycheck evil-easymotion avy modern-cpp-font-lock evil-vimish-fold vimish-fold use-package miniedit guide-key evil company color-theme-solarized))
 '(pdf-view-midnight-colors (cons "#383a42" "#fafafa"))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(rustic-ansi-faces
   ["#fafafa" "#e45649" "#50a14f" "#986801" "#4078f2" "#a626a4" "#0184bc" "#383a42"])
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#fafafa" :foreground "#383a42" :inverse-video nil :box nil :strike-through nil :extend nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Sarasa Mono SC"))))
 '(mode-line ((t (:background "#afffff" :foreground "#005fff" :box nil :overline nil :underline nil)))))

;; (set-face-attribute 'default nil :height 160)

(set-face-attribute 'default nil :height 160)

(add-hook 'prog-mode-hook #'hs-minor-mode)

(global-hl-line-mode 1)
;(set-face-background 'hl-line "#3e4446")
;(set-face-foreground 'highlight nil)
(set-face-foreground 'hl-line nil)

;;; Use whitespace (instead of column-marker, column-enforce-mode)

; ensure moues
(xterm-mouse-mode t)

(use-package modern-cpp-font-lock
  :defer t
  :config
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))

;(add-hook 'prog-mode-hook
;         (lambda () (add-to-list 'write-file-functions
;                                 'delete-trailing-whitespace)))

(use-package neotree
  :defer t)

; (use-package perspective
;   :ensure t
;   :defer t)
; (persp-mode)

(use-package flycheck
  :defer t
  :config (global-flycheck-mode))

(add-hook 'after-init-hook #'global-flycheck-mode)

(setq flycheck-indication-mode 'left-fringe)

(use-package flycheck-color-mode-line
  :after (flycheck)
  :defer t
  :config
  (eval-after-load "flycheck"
    '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))

(use-package flycheck-status-emoji
  :after (flycheck)
  :defer t)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;(use-package icicles
;  :ensure t)
;(use-package etags-select
;  :ensure t)

;;;; Mouse scrolling in terminal emacs
(unless (display-graphic-p)
  ;; activate mouse-based scrolling
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line)
  )

;; optional: this is the evil state that evil-magit will use
;; (setq evil-magit-state 'normal)
;; optional: disable additional bindings for yanking text
;; (setq evil-magit-use-y-for-yank nil)
(use-package evil-magit
  :after (evil magit))

(global-set-key (kbd "C-c d") 'define-word-at-point)
(global-set-key (kbd "C-c D") 'define-word)






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq confirm-kill-emacs 'y-or-n-p)

;;; MK's cheatsheet
(use-package cheatsheet
  :ensure t
  :config
  (cheatsheet-add :group 'Emacs
                  :key "C-x u"
                  :description "Emacs Undo.")
  (cheatsheet-add :group 'Emacs
                  :key "C-x e"
                  :description "Execute the e-lisp expression under the cursor.")
  (cheatsheet-add :group 'OrgMode
                  :key "C-c h"
                  :description "Open Home Org.")
  (cheatsheet-add :group 'OrgMode
                  :key "C-c a"
                  :description "Open Org Agenda.")
  (cheatsheet-add :group 'OrgMode
                  :key "C-c c"
                  :description "Capture.")
  )

;;; Smart Tab
(use-package smart-tab
  :config
  (smart-tabs-insinuate 'c 'javascript))

;;; Easy motion
;; Options includes:
;; - https://github.com/abo-abo/avy
;; - https://github.com/PythonNut/evil-easymotion
;; - https://github.com/hlissner/evil-snipe <= This is chosen by now.
(use-package evil-easymotion
  :after (evil)
  :config
  (evilem-default-keybindings "SPC")
  ;; (evilem-define (kbd "SPC c") 'avy-goto-char)
  ;; (global-set-key (kbd "SPC") 'avy-goto-char)
  (define-key evil-normal-state-map (kbd "SPC") 'avy-goto-char))

(use-package ctrlf
  :ensure t
  :config
  (ctrlf-mode +1)
  )





(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
        ("\\.md\\'" . markdown-mode)
        ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  ;; (custom-set-variables '(markdown-command "/usr/local/bin/pandoc"))
  (setq markdown-command "/usr/local/bin/pandoc")

  (setq markdown-preview-stylesheets (list "https://raw.githubusercontent.com/sindresorhus/github-markdown-css/gh-pages/github-markdown.css"))
  ;;"http://thomasf.github.io/solarized-css/solarized-light.min.css"))
  )

(use-package flymd
  :after (markdown-mode))

;; (use-package emojify
;;   :hook (after-init . global-emojify-mode))

(use-package unicode-fonts
   :ensure t
   :config
    (unicode-fonts-setup))

;;; Flycheck + proselint
(flycheck-define-checker proselint
  "A linter for prose."
  :command ("proselint" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
    (id (one-or-more (not (any " "))))
    (message) line-end))
  :modes (text-mode markdown-mode gfm-mode org-mode))
(add-to-list 'flycheck-checkers 'proselint)

;;; Dictionary, from https://github.com/manateelazycat/sdcv
;; brew install stardict sdcv
;; (use-package posframe)
;;(require 'posframe)
;; (use-package sdcv)
;;(require 'sdcv)

;; Default config
(setq sdcv-say-word-p t)               ;say word after translation

(setq sdcv-dictionary-data-dir "startdict_dictionary_directory") ;setup directory of stardict dictionary

(setq sdcv-dictionary-simple-list    ;setup dictionary list for simple search
      '("懒虫简明英汉词典"
        "懒虫简明汉英词典"
        "KDic11万英汉词典"))

(setq sdcv-dictionary-complete-list     ;setup dictionary list for complete search
      '(
        "懒虫简明英汉词典"
        "英汉汉英专业词典"
        "XDICT英汉辞典"
        "stardict1.3英汉辞典"
        "WordNet"
        "XDICT汉英辞典"
        "Jargon"
        "懒虫简明汉英词典"
        "FOLDOC"
        "新世纪英汉科技大词典"
        "KDic11万英汉词典"
        "朗道汉英字典5.0"
        "CDICT5英汉辞典"
        "新世纪汉英科技大词典"
        "牛津英汉双解美化版"
        "21世纪双语科技词典"
        "quick_eng-zh_CN"
        ))


;; (set-face-attribute 'default nil :height 160)


;;; Telega --- telegram client
(setq telega-proxies
      (list
       '(:server "127.0.0.1" :port 1086 :enable t
                 :type (:@type "proxyTypeSocks5"))
       ))


;;; AucTex
(use-package tex
  :ensure auctex
  :config
  ;; make latexmk available via C-c C-c
  ;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
  ;; (add-to-list 'TeX-command-list '("latexmk" "latexmk -pdf -escape-shell %s" TeX-run-TeX nil t :help "Run latexmk on file"))
  (add-to-list 'TeX-command-list '("Make" "make" TeX-run-command nil t))
  (add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "Make")))
  ;; (setq-default TeX-command-default "Make")
  ;; from https://gist.github.com/stefano-meschiari/9217695
  (setq TeX-auto-save t)
  (setq Tex-parse-self t)
  ;; Guess/Ask for the master file.
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t)
  (setq TeX-PDF-mode t)

  ;; From https://emacs.stackexchange.com/questions/19472/how-to-let-auctex-open-pdf-with-pdf-tools
  ;; ;; Use pdf-tools to open PDF files
  ;; (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
  ;;       TeX-source-correlate-start-server t)
  ;; ;; Update PDF buffers after successful LaTeX runs
  ;; (add-hook 'TeX-after-compilation-finished-functions
  ;;           #'TeX-revert-document-buffer)

  (setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
  (setq TeX-view-program-list
        '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -g %n %o %b")))
        ;; '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

  ;; (add-hook 'LaTeX-mode-hook
  ;;           (lambda ()
  ;;             (push
  ;;              '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
  ;;                :help "Run latexmk on file")
  ;;              TeX-command-list)))


  ;; From https://www.reddit.com/r/emacs/comments/4ew1s8/blurry_pdf_in_pdftools_and_docviewmode/
  (require 'pdf-view)
  (setq pdf-view-midnight-colors `(,(face-attribute 'default :foreground) .
                                   ,(face-attribute 'default :background)))
  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
  (add-hook 'pdf-view-mode-hook (lambda ()
                                  (pdf-view-midnight-minor-mode)))
  (setq TeX-error-overview-open-after-TeX-run t)
  ;; (setq mkvoya/tex-auto-compile nil)
  ;; (defun mkvoya/tex-try-auto-compile ()
  ;;   (when (and (eq major-mode 'TeX-mode)
  ;;              (mkvoya/tex-auto-compile))
  ;;     (TeX-command-run))
  ;;   )
  ;; (add-hook 'after-save-hook #'mkvoya/tex-try-auto-compile)
  )
(use-package auctex-latexmk)
(use-package reftex
  :config
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
  (add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode
  )


;;; Get shell env from user shell.
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;; Disable Helm and use ivy.
;;; Disable ivy, swiper, counsel, use selectrum and consult (and ctrlf?)
(use-package selectrum
  :ensure t
  :config
  (selectrum-mode +1)
  )
(use-package selectrum-prescient
  :ensure t
  :after (selectrum)
  :config
  ;; to make sorting and filtering more intelligent
  (selectrum-prescient-mode +1)
  ;; to save your command history on disk, so the sorting gets more
  ;; intelligent over time
  (prescient-persist-mode +1)
  )
;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due to use-package.
  :bind (("C-c h" . consult-history)
         ("C-c o" . consult-outline)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r x" . consult-register)
         ("C-x r b" . consult-bookmark)
         ("M-g o" . consult-outline) ;; "M-s o" is a good alternative
         ("M-g m" . consult-mark)    ;; "M-s m" is a good alternative
         ("M-g l" . consult-line)    ;; "M-s l" is a good alternative
         ("M-g i" . consult-imenu)   ;; "M-s i" is a good alternative
         ("M-g e" . consult-error)   ;; "M-s e" is a good alternative
         ("M-s m" . consult-multi-occur)
         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos))
  ;; The :init configuration is always executed (Not lazy!)
  :init
  ;; Replace functions (consult-multi-occur is a drop-in replacement)
  (fset 'multi-occur #'consult-multi-occur)
  ;; Configure other variables and modes in the :config section, after lazily loading the package
  :config
  ;; Optionally configure narrowing and widening keys.
  ;; Both < and C-+ work reasonably well.
  ;; (setq consult-narrow-key "<"
  ;;       consult-widen-key "< ")
  ;; (setq consult-narrow-key (kbd "C-+")
  ;;       consult-widen-key (kbd "C-+ SPC"))
  ;; Optional configure a "view" library to be used by `consult-buffer`.
  ;; The view library must provide two functions, one to open the view by name,
  ;; and one function which must return a list of views as strings.
  ;; Example: https://github.com/minad/bookmark-view/
  ;; (setq consult-view-open-function #'bookmark-jump
  ;;       consult-view-list-function #'bookmark-view-names)

  ;; Optionally enable previews. Note that individual previews can be disabled
  ;; via customization variables.
  (consult-preview-mode))

;; Enable Consult-Selectrum integration.
;; This package should be installed if Selectrum is used.
(use-package consult-selectrum
  :demand t)

;; Optionally add the consult-flycheck command.
(use-package consult-flycheck
  :bind (:map flycheck-command-map
         ("!" . consult-flycheck)))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :ensure t
  ;; The :init configuration is always executed (Not lazy!)
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode)
  ;; Prefer richer, more heavy, annotations over the lighter default variant.
  ;; E.g. M-x will show the documentation string additional to the keybinding.
  ;; By default only the keybinding is shown as annotation.
  ;; Note that there is the command `marginalia-cycle-annotators` to
  ;; switch between the annotators.
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light)))

(use-package embark
  :after selectrum
  :bind (:map minibuffer-local-map
              ("C-o" . embark-act)
              ("C-S-o" . embark-act-noexit)
              :map embark-file-map
              ("j" . dired-jump)))



(when window-system (set-frame-size (selected-frame) 80 60))

(use-package projectile
  :ensure t
  :delight '(:eval (concat " P[" (projectile-project-name) "]"))
  :config
  (projectile-mode +1)
  ;; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  ;; (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  )

(use-package dashboard
    :ensure t
    :diminish dashboard-mode
    :config
    (setq dashboard-banner-logo-title "What a nice day!")
    ;;(setq dashboard-startup-banner "/path/to/image")
    (setq dashboard-items '((recents  . 10)
                            (bookmarks . 10)
                            (projects . 5)
                            (agenda . 5)
                            (registers . 5)))
    (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
    (dashboard-setup-startup-hook))

;;; Prevent Extraneous Tabs
(setq-default indent-tabs-mode nil)
(setq-default fill-column 80)

;; https://emacs.stackexchange.com/questions/45546/per-mode-value-for-fill-column
(defun mkvoya/tex-mode-hook ()
  (setq fill-column 1024))
(add-hook 'TeX-mode-hook #'mkvoya/tex-mode-hook)

(use-package quelpa
  :init
  (setq quelpa-update-melpa-p nil)
  :ensure t)
(use-package quelpa-use-package
  :ensure t)
(use-package svg-tag-mode
  :quelpa (svg-tag-mode :repo "rougier/svg-tag-mode"
                        :fetcher github
                        :files ("svg-tag-mode.el")))

(use-package langtool
  :quelpa (langtool :repo "mhayashi1120/Emacs-langtool"
                    :fetcher github
                    :files ("langtool.el"))
  :init
  (setq langtool-language-tool-server-jar "/usr/local/Cellar/languagetool/5.1.3_2/libexec/languagetool-server.jar")
  ;; (setq langtool-bin "/usr/local/bin/langtool")
  (setq langtool-server-user-arguments '("-p" "8099"))

  ;; (defun langtool-autoshow-detail-popup (overlays)
  ;;   (when (require 'popup nil t)
  ;;     ;; Do not interrupt current popup
  ;;     (unless (or popup-instances
  ;;                 ;; suppress popup after type `C-g` .
  ;;                 (memq last-command '(keyboard-quit)))
  ;;       (let ((msg (langtool-details-error-message overlays)))
  ;;         (popup-tip msg)))))
  ;; (setq langtool-autoshow-message-function
  ;;       'langtool-autoshow-detail-popup)
  )

(use-package wucuo
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'wucuo-start)
  (add-hook 'text-mode-hook #'wucuo-start)
  (setq ispell-program-name "aspell")
  ;; You could add extra option "--camel-case" for since Aspell 0.60.8
  ;; @see https://github.com/redguardtoo/emacs.d/issues/796
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=16" "--camel-case")))

;;; View Large Files
(use-package vlf
  :defer t)

;; from https://stackoverflow.com/questions/1250846/wrong-type-argument-commandp-error-when-binding-a-lambda-to-a-key
(global-set-key (kbd "C-c h") (lambda () (interactive) (find-file "~/Dropbox/Dreams/Org/Main.org")))
;; Open ibuffer upon "C-c i"
(global-set-key (kbd "C-c i") 'ibuffer)

;; with use-package
(use-package maple-explorer
  :quelpa (maple-explorer
           :fetcher github
           :repo "honmaple/emacs-maple-explorer")
  :commands (maple-explorer-file maple-explorer-buffer maple-explorer-imenu maple-explorer-recentf)
  :config
  (setq maple-explorer-file-display-alist '((side . left) (slot . -1))))


;;preview files in dired
(use-package peep-dired
  :ensure t
  :defer t ; don't access `dired-mode-map' until `peep-dired' is loaded
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

;; Prevent polluting the system slipboard
;; (setq select-enable-clipboard nil)

;; Dired-sidebar is not good to use.
;; (use-package dired-sidebar
;;   :ensure t
;;   :commands (dired-sidebar-toggle-sidebar))

;; (use-package dired-subtree
;;   :config
;;   (bind-keys :map dired-mode-map
;;              ("<S-return>" . dired-subtree-toggle)))


(defun make-underscore-part-of-words () (modify-syntax-entry ?_ "w"))
(add-hook 'prog-mode-hook #'make-underscore-part-of-words)

(use-package tramp
  :config
  (setq tramp-debug-buffer t)
  (setq tramp-verbose 10))

(setq alert-default-style 'libnotify)

(setq org-alert-headline-regexp "\\(SCHEDULED:.+\\|DEADLINE:.+\\)")

(use-package delight
  :config
  (delight '((abbrev-mode " Abv" "abbrev")
             (smart-tab-mode " \\t" "smart-tab")
             (eldoc-mode nil "eldoc")
             (yas-mode)
             (Undo-Tree)
             (overwrite-mode " Ov" t)))
  (delight 'rainbow-mode)
  (delight 'emacs-lisp-mode "Elisp" :major))

(add-to-list 'auto-mode-alist '("\\.eps\\'" . image-mode))


(use-package calfw
  :ensure t)
(use-package calfw-org
  :ensure t
  :after (calfw org))

(use-package async
  :ensure t)

(modify-syntax-entry ?_ "w")

;; From: https://stackoverflow.com/questions/4657142/how-do-i-encourage-emacs-to-follow-the-compilation-buffer
;; Compilation output
(setq compilation-scroll-output t)
;; (setq compilation-scroll-output 'first-error)


(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
(use-package mu4e)
(use-package mu4e-alert
  :ensure t
  :after (mu4e)
  :config
  (setq mu4e-alert-interesting-mail-query
        "flag:unread maildir:/Subscribe/INBOX "
        )
  (mu4e-alert-enable-mode-line-display)
  (defun gjstein-refresh-mu4e-alert-mode-line ()
    (interactive)
    (mu4e~proc-kill)
    (mu4e-alert-enable-mode-line-display)
    )
  (run-with-timer 0 60 'gjstein-refresh-mu4e-alert-mode-line)
  (setq exec-path (append '("/usr/local/opt/terminal-notifier/bin/") exec-path))
  (mu4e-alert-set-default-style 'notifier)
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
  ;; Choose the style you prefer for desktop notifications
  ;; On Mac OSX you can set style to
  ;; 1. notifier      - Notifications using the `terminal-notifier' program, requires `terminal-notifier' to be in PATH
  ;; 1. growl         - Notifications using the `growl' program, requires `growlnotify' to be in PATH
  (global-set-key (kbd "C-c m") 'mu4e)
  (bind-keys :map mu4e-headers-mode-map
             ("<mouse-1>" . mu4e-headers-view-message))
  )



;;;; LSP
;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(setq lsp-keymap-prefix "s-l")

(use-package lsp-mode
    :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
            (python-mode . lsp)
            ;; if you want which-key integration
            (lsp-mode . lsp-enable-which-key-integration))
    :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are helm user
;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
    :config
    (which-key-mode))


(use-package beacon
  :config
  (beacon-mode 1))

;; From the official doc <https://github.com/tumashu/pyim>.
(use-package pyim
  :ensure nil
  :demand t
  :config
  ;; 激活 basedict 拼音词库，五笔用户请继续阅读 README
  (use-package pyim-basedict
    :ensure nil
    :config (pyim-basedict-enable))

  (setq default-input-method "pyim")

  (setq pyim-default-scheme 'xiaohe-shuangpin)

  ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
  ;; 我自己使用的中英文动态切换规则是：
  ;; 1. 光标只有在注释里面时，才可以输入中文。
  ;; 2. 光标前是汉字字符时，才能输入中文。
  ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
  ;;(setq-default pyim-english-input-switch-functions
  ;;              '(pyim-probe-dynamic-english
  ;;                pyim-probe-isearch-mode
  ;;                pyim-probe-program-mode
  ;;                pyim-probe-org-structure-template))
  ;;(setq-default pyim-punctuation-half-width-functions
  ;;              '(pyim-probe-punctuation-line-beginning
  ;;                pyim-probe-punctuation-after-punctuation))

  ;; 开启拼音搜索功能
  (pyim-isearch-mode 1)

  ;; 使用 popup-el 来绘制选词框, 如果用 emacs26, 建议设置
  ;; 为 'posframe, 速度很快并且菜单不会变形，不过需要用户
  ;; 手动安装 posframe 包。
  (setq pyim-page-tooltip 'posframe)
  ;; (setq pyim-page-tooltip 'popup)

  ;; 选词框显示5个候选词
  (setq pyim-page-length 5)
  (global-set-key (kbd "M-n") 'toggle-input-method))

  ;; :bind
  ;; (("M-n" . pyim-convert-string-at-point) ;与 pyim-probe-dynamic-english 配合
  ;;  ("C-;" . pyim-delete-word-from-personal-buffer)))

(add-hook 'org-mode-hook
          (lambda () (add-to-list 'write-file-functions
                                  'delete-trailing-whitespace)))

;; (when (memq window-system '(mac ns))
;;   (add-to-list 'default-frame-alist '(ns-appearance . light)) ;; {light, dark}
;;   (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;;   (setq ns-use-proxy-icon nil)
;;   ;; (setq frame-title-format nil)
;;   )
(when (eq system-type 'darwin) (ns-auto-titlebar-mode))

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(use-package config-fonts
  :load-path "~/.emacs.d/mkvoya"
  :ensure nil) ; local package does not need ensure
(use-package config-appearances
  :load-path "~/.emacs.d/mkvoya"
  :ensure nil) ; local package does not need ensure
(use-package config-org
  :load-path "~/.emacs.d/mkvoya"
  :ensure nil) ; local package does not need ensure



;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

(provide 'init)
;;; init.el ends here
