;;; init.el --- Initialization file for eMaKs
;;; Commentary:
;;;   Emacs Startup File --- initialization for Emacs

;;; Code:
(load "server") ; Load and start server if it's not running
(unless (server-running-p) (server-start))

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
;;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; Special Keys for MacOS GUI
;;(setq mac-command-modifier 'meta) ; Switch Cmd and Opt(Alt)
;;(setq mac-option-modifier 'super) ; Switch Cmd and Opt(Alt)



(eval-when-compile
  (require 'use-package))
;;; (require 'diminish)
(require 'bind-key)


;;; Tabbar
(use-package centaur-tabs
  :ensure t
  :demand
  :config
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-gray-out-icons 'buffer)
  (setq centaur-tabs-set-bar 'over)
  (setq centaur-tabs-modified-marker "*")
  (centaur-tabs-mode t)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))

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

; Mode line format
(use-package smart-mode-line :defer t)

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
  :diminish undo-tree-mode
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
  ;(company-auctex-init)
  ;;(company-statistics-mode 1)
  )



; Powerline
(use-package powerline
  :ensure t
  :config
  (powerline-center-evil-theme))

;; (setq line-number-mode 0)

(setq linum-format "%d ")
;; (global-linum-mode t)
(add-hook 'prog-mode-hook 'linum-on)

(load-theme 'manoj-dark)

(setq c-default-style "linux"
      c-basic-offset 8)
(setq-default c-basic-offset 8
	      tab-width 8
	      indent-tabs-mode t)

; Whitespace[built-in], check: http://ergoemacs.org/emacs/whitespace-mode.html
(use-package whitespace)
(setq whitespace-style
'(face trailing tabs newline tab-mark newline-mark lines-tail))
(setq whitespace-display-mappings
'((newline-mark 10 [8617 10])
  (tab-mark 9 [8594 9] [92 9])))
(set-face-background 'trailing-whitespace "#ffaf5f")
(set-face-background 'whitespace-trailing "#ffaf5f")
(global-whitespace-mode t)


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
  :ensure t
  :config
  (load-theme 'monokai t)
  (setq monokai-background "#080C14")
  (message "HI"))
(use-package grandshell-theme
  :ensure t
  :config
;  (load-theme 'grandshell t)
  )
(use-package alect-themes
  :ensure t
  :config
;  (load-theme 'alect-black t)
  )

;(use-package spaceline-config
;  :ensure t
;  :config
;  (spaceline-spacemacs-theme))

(use-package airline-themes
  :ensure t)
(load-theme 'airline-light t)


(require 'evil)
  (evil-mode 1)


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
 '(centaur-tabs-mode t nil (centaur-tabs))
 '(compilation-message-face 'default)
 '(custom-safe-themes
   '("9e39a8334e0e476157bfdb8e42e1cea43fad02c9ec7c0dbd5498cf02b9adeaf1" "9089d25e2a77e6044b4a97a2b9fe0c82351a19fdd3e68a885f40f86bbe3b3900" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "3eb93cd9a0da0f3e86b5d932ac0e3b5f0f50de7a0b805d4eb1f67782e9eb67a4" "962dacd99e5a99801ca7257f25be7be0cebc333ad07be97efd6ff59755e6148f" "f78de13274781fbb6b01afd43327a4535438ebaeec91d93ebdbba1e3fba34d3c" default))
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
 '(magit-diff-use-overlays nil)
 '(org-agenda-files '("~/Dropbox/Dreams/org/main.org"))
 '(package-selected-packages
   '(heaven-and-hell ov svg-clock swiper-helm counsel ivy vlf projectile-sift projectile dashboard powerline-evil which-key-posframe smart-mode-line exec-path-from-shell rainbow-delimiters rainbow-blocks all-the-icons kaolin-themes doom-themes atom-one-dark-theme centaur-tabs telega pdf-tools org-superstar jinja2-mode csv-mode smex sdcv posframe unicode-fonts company-emoji emojify flymd diff-hl helm-descbinds buttons texfrag evil-numbers smart-tabs-mode smart-tab cheatsheet org-d20 jumblr 2048-game yascroll zone-nyan markdown-toc markdown-preview-mode markdown-mode+ org-agenda-property dired-ranger ## synonymous define-word auctex evil-magit magit neotree flycheck-status-emoji flycheck-color-mode-line flycheck evil-easymotion avy modern-cpp-font-lock evil-vimish-fold vimish-fold powerline use-package miniedit guide-key evil company color-theme-solarized))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
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
 )

;;; FONT
;;;;;;;;; DO NOT WORK FOR CLI
;;;;; INSTALL FILES
;(call-process "git" nil t nil "clone" "https://github.com/powerline/fonts.git" "/tmp/fonts")
;(call-process "ls" nil t nil "/tmp/fonts")
;(call-process "/tmp/fonts/install.sh" nil t)

;;;;;;(add-to-list 'default-frame-alist '(font . "3270"))
;;;;;;(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono for Powerline"))
;;;;;;
;;;;;;(set-face-attribute 'default nil :family "DejaVu Sans Mono for Powerline")
;;;;;;;(set-face-attribute 'default nil :height 94)
;;;;;;(set-fontset-font "fontset-default" 'unicode "DejaVu Sans Mono for Powerline")
;;;;;;
;;;;;;;;
;;;;;;
;;;;;;(set-face-attribute 'mode-line nil :family "DejaVu Sans Mono for Powerline")
;;;;;;(set-face-attribute 'mode-line-inactive nil :family "DejaVu Sans Mono for Powerline")
;;;;;;(set-face-attribute 'powerline-inactive1 nil :family "DejaVu Sans Mono for Powerline")
;;;;;;(set-face-attribute 'powerline-active1 nil :family "DejaVu Sans Mono for Powerline")

(set-face-attribute 'default nil :height 160)

(setq powerline-default-separator 'utf-8)
(setq powerline-utf-8-separator-left #x27bd)
(setq powerline-utf-8-separator-right #x2b05)

(add-hook 'prog-mode-hook #'hs-minor-mode)

(global-hl-line-mode 1)
;(set-face-background 'hl-line "#3e4446")
;(set-face-foreground 'highlight nil)
(set-face-foreground 'hl-line nil)

;;;;; Use whitespace instead
;;(use-package column-marker
;;  :ensure t
;;  :defer t)
;;
;;(add-hook 'prog-mode-hook (lambda () (interactive) (column-marker-1 80)))
;(use-package column-enforce-mode
;  :ensure t
;  :defer t)
;(add-hook 'prog-mode-hook 'column-enforce-mode)
;(setq column-enforce-comments nil)

; ensure moues
(xterm-mouse-mode t)

(use-package modern-cpp-font-lock
  :ensure t
  :defer t)
(add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)

;(add-hook 'prog-mode-hook
;	  (lambda () (add-to-list 'write-file-functions
;				  'delete-trailing-whitespace)))

(use-package neotree
  :ensure t
  :defer t)

; (use-package perspective
;   :ensure t
;   :defer t)
; (persp-mode)

;(use-package avy
;  :ensure t
;  :defer t)


(use-package flycheck
  :ensure t
  :defer t
  :init (global-flycheck-mode))

(add-hook 'after-init-hook #'global-flycheck-mode)

(setq flycheck-indication-mode 'left-fringe)

(use-package flycheck-color-mode-line
  :ensure t
  :defer t)

(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(use-package flycheck-status-emoji
  :ensure t
  :defer t)

;(use-package helm
;  :ensure t
;  :defer t
;  :config
;  (helm-mode 1))
;;(use-package helm-config
;;  :ensure t
;;  :defer t)
;(use-package helm-etags-plus
;  :bind
;  ("M-." . helm-etags-plus-select)
;  :ensure t
;  :defer t)


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
(require 'evil-magit)

;(use-package auctex)
;(use-package auctex-latexmk)



(global-set-key (kbd "C-c d") 'define-word-at-point)
(global-set-key (kbd "C-c D") 'define-word)




(add-hook 'LaTeX-mode-hook
(lambda ()
  (push
   '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
     :help "Run latexmk on file")
    TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
      '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq confirm-kill-emacs 'y-or-n-p)

;;; Org mode
; Enable Org mode
(require 'org)
; Org mode TODO states
(setq org-todo-keywords
      '((sequence "TODO" "HAND" "WAIT" "DONE")))

(global-set-key (kbd "C-c a") 'org-agenda)


;;; MK's cheatsheet
(require 'cheatsheet)
(cheatsheet-add :group 'Common
		:key "C-x u"
		:description "Emacs Undo")

;;; Smart Tab
(require 'smart-tab)
(smart-tabs-insinuate 'c 'javascript)

;;; Easy motion
;; Options includes:
;; - https://github.com/abo-abo/avy
;; - https://github.com/PythonNut/evil-easymotion
;; - https://github.com/hlissner/evil-snipe <= This is chosen by now.
(require 'evil-easymotion)
(evilem-default-keybindings "SPC")
;; (evilem-define (kbd "SPC c") 'avy-goto-char)
;; (global-set-key (kbd "SPC") 'avy-goto-char)
(define-key evil-normal-state-map (kbd "SPC") 'avy-goto-char)


;;; Better evil
(require 'evil)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(require 'evil-numbers)
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-S-a") 'evil-numbers/dec-at-pt)
;;; Evil rebind
;; :q should kill the current buffer rather than quitting emacs entirely
(evil-ex-define-cmd "q" '(if (one-window-p t) (kill-this-buffer) (close)))
;; (evil-ex-define-cmd "q" 'mk/betterq)
;; Need to type out :quit to close emacs
(evil-ex-define-cmd "quit" 'evil-quit)


(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
        ("\\.md\\'" . markdown-mode)
        ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; (custom-set-variables '(markdown-command "/usr/local/bin/pandoc"))
(setq markdown-command "/usr/local/bin/pandoc")

(setq markdown-preview-stylesheets (list "https://raw.githubusercontent.com/sindresorhus/github-markdown-css/gh-pages/github-markdown.css"))
;;"http://thomasf.github.io/solarized-css/solarized-light.min.css"))

(require 'flymd)

(use-package emojify
  :hook (after-init . global-emojify-mode))

;(require 'company-emoji)
;(add-to-list 'company-backends 'company-emoji)

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


;;; Smex --- enhaced M-x
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;; Org Style
;; from https://www.lijigang.com/blog/2018/08/08/神器-org-mode/#org4288876
;; 打开 org-indent mode
(setq org-startup-indented t)
(require 'org-superstar)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

;; 设置 bullet list
;; (setq org-bullets-bullet-list '("☰" "☷" "☯" "☭"))
(with-eval-after-load 'org-superstar
  (set-face-attribute 'org-superstar-item nil :height 1.2)
  (set-face-attribute 'org-superstar-header-bullet nil :height 1.2)
  (set-face-attribute 'org-superstar-leading nil :height 1.3))
;; Set different bullets, with one getting a terminal fallback.
(setq org-superstar-headline-bullets-list
      '("◉" "◈" "○" "▷"))
;; Stop cycling bullets to emphasize hierarchy of headlines.
(setq org-superstar-cycle-headline-bullets nil)
;; Hide away leading stars on terminal.
(setq org-superstar-leading-fallback ?\s)


;; 调试好久的颜色，效果超赞！todo keywords 增加背景色
(setf org-todo-keyword-faces '(("TODO" . (:foreground "white" :background "#95A5A6"   :weight bold))
			       ("HAND" . (:foreground "white" :background "#2E8B57"  :weight bold))
			       ("DONE" . (:foreground "white" :background "#3498DB" :weight bold))))
;; agenda 里面时间块彩色显示
;; From: https://emacs-china.org/t/org-agenda/8679/3
(defun ljg/org-agenda-time-grid-spacing ()
  "Set different line spacing w.r.t. time duration."
  (save-excursion
    (let* ((background (alist-get 'background-mode (frame-parameters)))
           (background-dark-p (string= background "dark"))
           (colors (list "#1ABC9C" "#2ECC71" "#3498DB" "#9966ff"))
           pos
           duration)
      (nconc colors colors)
      (goto-char (point-min))
      (while (setq pos (next-single-property-change (point) 'duration))
        (goto-char pos)
        (when (and (not (equal pos (point-at-eol)))
                   (setq duration (org-get-at-bol 'duration)))
          (let ((line-height (if (< duration 30) 1.0 (+ 0.5 (/ duration 60))))
                (ov (make-overlay (point-at-bol) (1+ (point-at-eol)))))
            (overlay-put ov 'face `(:background ,(car colors)
                                                :foreground
                                                ,(if background-dark-p "black" "white")))
            (setq colors (cdr colors))
            (overlay-put ov 'line-height line-height)
            (overlay-put ov 'line-spacing (1- line-height))))))))
(add-hook 'org-agenda-finalize-hook #'ljg/org-agenda-time-grid-spacing)


(set-face-attribute 'default nil :height 160)


;;; Telega --- telegram client
(setq telega-proxies
      (list
       '(:server "127.0.0.1" :port 1086 :enable t
                 :type (:@type "proxyTypeSocks5"))
       ))


;;; AucTex
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

;; make latexmk available via C-c C-c
;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
(add-hook 'LaTeX-mode-hook (lambda ()
  (push
    '("latexmk" "latexmk -pdf -escape-shell %s" TeX-run-TeX nil t
:help "Run latexmk on file")
    TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
      '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))


;;; Get shell env from user shell.
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;; Disable Helm for now.
;;;; Helm
;;; from https://tuhdo.github.io/helm-intro.html
;(require 'helm)
;(require 'helm-config)
;;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
;(global-set-key (kbd "C-c h") 'helm-command-prefix)
;(global-unset-key (kbd "C-x c"))
;
;(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
;(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
;(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
;
;
;(when (executable-find "curl")
;  (setq helm-google-suggest-use-curl-p t))
;
;(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
;      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
;      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
;      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
;      helm-ff-file-name-history-use-recentf t
;      helm-echo-input-in-header-line t)
;
;(defun spacemacs//helm-hide-minibuffer-maybe ()
;  "Hide minibuffer in Helm session if we use the header line as input field."
;  (when (with-helm-buffer helm-echo-input-in-header-line)
;    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
;      (overlay-put ov 'window (selected-window))
;      (overlay-put ov 'face
;                   (let ((bg-color (face-background 'default nil)))
;                     `(:background ,bg-color :foreground ,bg-color)))
;      (setq-local cursor-type nil))))
;
;
;(add-hook 'helm-minibuffer-set-up-hook
;          'spacemacs//helm-hide-minibuffer-maybe)
;
;(setq helm-autoresize-max-height 0)
;(setq helm-autoresize-min-height 20)
;(helm-autoresize-mode 1)
;
;(global-set-key (kbd "M-x") 'helm-M-x)
;(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
;(global-set-key (kbd "M-y") 'helm-show-kill-ring)
;(global-set-key (kbd "C-x b") 'helm-mini)
;(setq helm-buffers-fuzzy-matching t
;      helm-recentf-fuzzy-match    t)
;(global-set-key (kbd "C-x C-f") 'helm-find-files)
;
;
;
;(helm-mode 1)

;;; Now try with ivy

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(counsel-mode 1) ; parts of the following are duplicated with this mode.
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)


(when window-system (set-frame-size (selected-frame) 80 60))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

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
(defun mk/tex-mode-hook ()
  (setq fill-column 999))
(add-hook 'TeX-mode-hook #'mk/tex-mode-hook)


;;; View Large Files
(use-package vlf
  :defer t)

(global-set-key (kbd "C-c c") 'org-capture)
(setq org-capture-templates nil)
(add-to-list 'org-capture-templates
             '("j" "Journals" entry
               (file+datetree "~/Dropbox/Dreams/Org/Journals/Index.org" "Journals")
               "* %U - %^{heading}\n  %?"))
;; (setq org-default-notes-file "~/Dropbox/Dreams/Org/Inbox.org")
(add-to-list 'org-capture-templates
             '("t" "Tasks" entry
               (file+headline "~/Dropbox/Dreams/Org/Inbox.org" "Tasks")
               "* TODO %?\n  %u\n  %a"))

;;; Paste Image From https://emacs-china.org/t/topic/6601/4
(defun org-insert-image ()
  "insert a image from clipboard"
  (interactive)
  (let* ((path (concat default-directory "img/"))
         (image-file (concat
                      path
                      (buffer-name)
                      (format-time-string "_%Y%m%d_%H%M%S.png"))))
    (if (not (file-exists-p path))
        (mkdir path))
    (do-applescript (concat
                     "set the_path to \"" image-file "\" \n"
                     "set png_data to the clipboard as «class PNGf» \n"
                     "set the_file to open for access (POSIX file the_path as string) with write permission \n"
                     "write png_data to the_file \n"
                     "close access the_file"))
    ;; (shell-command (concat "pngpaste " image-file))
    (org-insert-link nil
                     (concat "file:" image-file)
                     "")
    (message image-file))
  (org-display-inline-images)
  )

;; (add-to-list 'load-path "~/.emacs.d/3rd-parties/baohaojun")
;; (load-library "bhj-fonts")

(add-to-list 'load-path "~/.emacs.d/mk/")
(load-library "config-fonts")
(load-library "config-appearances")

;; from https://stackoverflow.com/questions/1250846/wrong-type-argument-commandp-error-when-binding-a-lambda-to-a-key
(global-set-key (kbd "C-c h") (lambda () (interactive) (find-file "~/Dropbox/Dreams/Org/Main.org")))


(provide 'init)
;;; init.el ends here
