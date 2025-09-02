;; -*- lexical-binding: t; -*-
;;; Emacs Config Fragement: macOS GUI


;; We need to setup fonts early. Or displayed characters will be rendered and cached.

(use-package fontaine
  :if (not is-android)
  :ensure t
  :when (and (display-graphic-p) (not is-android))
  ;; :hook (kill-emacs . fontaine-store-latest-preset)
  :config
  (setq fontaine-latest-state-file
        (locate-user-emacs-file "etc/fontaine-latest-state.eld"))
  (setq fontaine-presets
        '((regular
           :default-height 120
           :default-weight regular
           :fixed-pitch-height 1.0
           :variable-pitch-height 1.0
           )
          (large
           :default-height 140
           :default-weight normal
           :fixed-pitch-height 1.0
           :variable-pitch-height 1.05
           )
          (t
           :default-family "Monaco"
           :fixed-pitch-family "Monaco"
           :variable-pitch-family "Monaco"
           :italic-family "Monaco"
           :variable-pitch-weight normal
           :bold-weight bold
           :italic-slant italic
           ;;:line-spacing 0.1
           )
          ))
  ;; (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  (fontaine-set-preset 'regular)

  ;; set emoji font
  (set-fontset-font
   t
   (if (version< emacs-version "28.1")
       '(#x1f300 . #x1fad0)
     'emoji)
   (cond
    ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
    ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
    ((member "Symbola" (font-family-list)) "Symbola")
    ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
    ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
    ))


  ;; set Chinese font
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font
     (frame-parameter nil 'font)
     charset
     (font-spec :family
                (cond
                 ((member "LXGW WenKai Screen" (font-family-list)) "LXGW WenKai Screen")
                 ((member "Sarasa Mono SC Nerd" (font-family-list)) "Sarasa Mono SC Nerd")
                 ((member "PingFang SC" (font-family-list)) "PingFang SC")
                 ((member "WenQuanYi Zen Hei" (font-family-list)) "WenQuanYi Zen Hei")
                 ((member "Microsoft YaHei" (font-family-list)) "Microsoft YaHei")
                 ))))

  (set-fontset-font (frame-parameter nil 'font) '(#x25c7 . #x2733) "Segoe UI Emoji" nil 'prepend)

  ;; (set-face-attribute 'default nil :font (font-spec :family "ia Writer" :size 14))
  ;; (set-fontset-font t 'unicode (font-spec :family "Noto Color Emoji" :size 14))
  ;; (set-fontset-font t '(#x2ff0 . #x9ffc) (font-spec :family "LXGW WenKai Screen" :size 18 :weight 'bold))
  ;; (set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji") nil 'prepend)
  ;; (set-fontset-font t '(#x2ff0 . #x9ffc) (font-spec :name "TsangerJinKai01" :size 14))
  ;; (set-fontset-font t 'unicode-bmp (font-spec :name "JuliaMono" :size 12) nil 'prepend)

  ;; set Chinese font scale
  (setq face-font-rescale-alist `(
                                  ("LXGW WenKai Screen"  . 1.24)
                                  ("Symbola"             . 1.3)
                                  ("Microsoft YaHei"     . 1.2)
                                  ("WenQuanYi Zen Hei"   . 1.2)
                                  ("Sarasa Mono SC Nerd" . 1.2)
                                  ("PingFang SC"         . 1.16)
                                  ("Lantinghei SC"       . 1.16)
                                  ("Kaiti SC"            . 1.16)
                                  ("Yuanti SC"           . 1.16)
                                  ("Apple Color Emoji"   . 0.91)
                                  ))
  )

(use-package reveal-in-osx-finder :defer t)

(use-package image-click-mode
  :delight
  :after (org)
  :ensure (:host github :repo "mkvoya/image-click-mode" :files ("*.el"))
  :config
  ;;(setq org-image-actual-width 400)
  ;; (setq org-image-actual-width '(300))
  (setq org-image-actual-width '(500))
  (add-hook 'org-mode-hook (lambda () (image-click-mode))))

(use-package image-slicing
  :ensure (image-slicing :type git
                         :host github
                         :repo "ginqi7/image-slicing")
  :defer t)

;; (use-package org-sliced-images
;;   :ensure t
;;   :config (org-sliced-images-mode))


(use-package nova
  :ensure (:host github :repo "thisisran/nova" :files ("*.el")))

(defun childframe-workable-p ()
  "Whether childframe is workable."
  (and (>= emacs-major-version 26)
       (not noninteractive)
       (not emacs-basic-display)
       (or (display-graphic-p)
           (featurep 'tty-child-frames))
       (eq (frame-parameter (selected-frame) 'minibuffer) 't)))
(setq blink-matching-paren-highlight-offscreen t
      show-paren-context-when-offscreen
      (if (childframe-workable-p) 'child-frame 'overlay))


(provide 'config-nsgui)
