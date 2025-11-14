;; -*- lexical-binding: t; -*-
;;; Emacs Configuration Fragment: CJK

;; * Rime: the input method
(use-package rime :ensure (:host github :repo "DogLooksGood/emacs-rime" :files ("*.el" "Makefile" "lib.c"))
  :if (eq system-type 'darwin)
  :defer t
  :custom
  (default-input-method "rime")
  (rime-disable-predicates
   '(rime-predicate-org-latex-mode-p
     rime-predicate-hydra-p
     rime-predicate-org-in-src-block-p
     rime-predicate-ace-window-p
     ))
  (rime-librime-root "~/.emacs.d/librime/dist")
  (rime-share-data-dir "~/Library/Rime")
  (rime-user-data-dir "~/Library/Rime")
  (rime-emacs-module-header-root "/opt/homebrew/opt/emacs-plus@31/include")
  (rime-show-candidate 'posframe)
  (rime-posframe-properties
   (list :background-color "#000000"  ; 不要在这里设置字体，会影响后面 face 里字体的
         :foreground-color "#f33333"
         :internal-border-width 3
         :internal-border-color "#5fc187"))
  :config
  (set-face-attribute 'rime-default-face nil
                      :background "#000000"
                      :foreground "honeydew1"
                      :font "Hei-20")
  (set-face-attribute 'rime-highlight-candidate-face nil
                      :background "#5fc187"
                      :foreground "#efefef"
                      :font "Hei-20")
  (set-face-attribute 'rime-candidate-num-face nil
                      :background "#000000"
                      :foreground "#5fc187"
                      :font "Hei-20")
  (set-face-attribute 'rime-code-face nil
                      :background "#719ae7"
                      :foreground "#efefef"
                      :font "Hei-20"))

;; * CJK jumping
(setq word-wrap-by-category t)
(use-package emt :ensure (:host github :repo "roife/emt" :files ("*.el" "module/*" "module"))
  :if (eq system-type 'darwin)
  :after (evil)
  :hook (elpaca-after-init . emt-mode)
  :config
  (define-key evil-motion-state-map (kbd "w") #'emt-forward-word)
  (define-key evil-motion-state-map (kbd "b") #'emt-backward-word))


(provide 'config-cjk)
