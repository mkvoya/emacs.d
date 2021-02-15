;;; init.el --- Bootstrap for the org configuration files cloned from https://github.com/angrybacon/dotemacs/blob/master/init.el

;;; Commentary:

;;; Code:

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
 '(notmuch-saved-searches
   '((:name "inbox" :query "tag:inbox" :key "i")
     (:name "unread" :query "tag:unread" :key "u")
     (:name "flagged" :query "tag:flagged" :key "f")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a")
     (:name "Unread" :query "is:unread")))
 '(objed-cursor-color "#e45649")
 '(org-agenda-files
   '("~/Dropbox/Dreams/Org/Plans.org" "~/Dropbox/Dreams/Org/IPADS.sched.org" "~/Dropbox/Dreams/Projects/DNA/DNA材料-持续更新/Survey.org" "~/Dropbox/Dreams/Org/Inbox.org" "~/Dropbox/Dreams/Org/Main.org"))
 '(org-clock-mode-line-total 'current)
 '(package-selected-packages
   '(pdf-view smart-mode-line nyan-mode toml-mode rust-mode rustic hide-mode-line notmuch-unread notmuch-maildir notmuch good-scroll cal-china-x orderless polymode org-brain zetteldeft deft smooth-scrolling sublimity esup ns-auto-titlebar pyim beacon smart-cursor-color org-roam-bibtex org-noter-pdftools org-noter org-roam ctrlf consult-flycheck consult-selectrum selectrum-prescient selectrum marginalia dap-mode org-ref mu4e-alert evil-mu4e org-caldav org-wild-notifier dired-launch calfw-org org-time-budgets org-timeline calfw git-timemachine rainbow-mode delight nameframe-perspective org-alert languagetool dired-sidebar maple-explorer company-lsp peep-dired auto-complete-auctex reveal-in-osx-finder webkit-color-picker zenity-color-picker wucuo langtool smex ebib cdlatex company-auctex company-reftex nameframe-projectile nameframe rg projectile-ripgrep org-sidebar svg-tag-mode quelpa-use-package quelpa ssh vs-light-theme color-theme-sanityinc-tomorrow hemisu-theme heaven-and-hell ov svg-clock vlf projectile-sift projectile dashboard which-key-posframe exec-path-from-shell rainbow-delimiters rainbow-blocks all-the-icons kaolin-themes doom-themes atom-one-dark-theme telega pdf-tools org-superstar jinja2-mode csv-mode sdcv posframe unicode-fonts flymd diff-hl helm-descbinds buttons texfrag evil-numbers smart-tabs-mode smart-tab cheatsheet org-d20 jumblr 2048-game yascroll zone-nyan markdown-toc markdown-preview-mode markdown-mode+ org-agenda-property dired-ranger ## synonymous define-word auctex evil-magit magit neotree flycheck-status-emoji flycheck-color-mode-line flycheck evil-easymotion avy modern-cpp-font-lock evil-vimish-fold vimish-fold use-package miniedit guide-key evil company color-theme-solarized))
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
 '(mode-line ((t (:background "#afffff" :foreground "#005fff" :box nil :overline nil :underline nil)))))

(let ((default-directory user-emacs-directory)
      (file-name-handler-alist nil)
      (gc-cons-percentage .6)
      (gc-cons-threshold most-positive-fixnum)
      (read-process-output-max (* 1024 1024)))

  ;; Disable that pesky echo message
  (setq inhibit-startup-echo-area-message user-login-name)

  ;; Mark safe variables early so that tangling won't break
  (put 'after-save-hook 'safe-local-variable
       (lambda (value) (equal value '(org-babel-tangle t))))
  (put 'display-line-numbers-width 'safe-local-variable 'integerp)

  ;; Tangle and compile if necessary only, then load the configuration
  (let* ((.org "emacs-config.org")
         (.el (concat (file-name-sans-extension .org) ".el"))
         (modification-time
          (file-attribute-modification-time (file-attributes .org))))
    (require 'org-macs)
    (unless (org-file-newer-than-p .el modification-time)
      (require 'ob-tangle)
      (org-babel-tangle-file .org .el "emacs-lisp")
      (byte-compile-file .el))
    (load-file .el))

  ;; Set the working directory to home regardless of where Emacs was started from
  (cd "~/")

  ;; Collect garbage when all else is done
  (garbage-collect))





;;; init.el ends here
