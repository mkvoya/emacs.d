;; -*- lexical-binding: t; -*-
;;; Emacs Config Fragement: Evil
;;; The vim inside Emacs

;; * Evil
(use-package evil
  :ensure t
  :demand t
  :after (undo-fu bind-key)
  :bind (:map evil-normal-state-map ("C-u" . #'evil-scroll-up))
  :config
  ;; ;; Use man (instead of WoMan) for man pages, although is slow in Emacs.
  ;; ;; Install man-db, check this: https://www.reddit.com/r/emacs/comments/mfmg3x/disabling_ivy_for_a_specific_command/
  ;; (evil-define-motion evil-lookup ()
  ;;   "Look up the keyword at point. Calls `evil-lookup-func'."
  ;;   (call-interactively #'man))

  (setq evil-want-C-i-jump nil)
  (setq evil-move-beyond-eol t)
  (setq evil-want-fine-undo t)
  (setq evil-symbol-word-search t)
  (evil-set-undo-system 'undo-fu)
  ;; Evil rebind
  ;; :q should kill the current buffer rather than quitting emacs entirely
  (defun mk/ex-quit ()
    "Evil ex quit."
    (interactive)
    (if (one-window-p "visible")
        (kill-this-buffer)
      (evil-window-delete)))
  (evil-ex-define-cmd "q" #'mk/ex-quit)
  ;; Need to type out :quit to close emacs
  (evil-ex-define-cmd "quit" 'evil-quit)
  (if (featurep 'ef-themes)
      (ef-themes-with-colors
        (setq evil-emacs-state-cursor `((bar . 3) ,cursor))
        (setq evil-insert-state-cursor `((bar . 1) ,cursor)))
    (setq evil-emacs-state-cursor '((bar . 3) "#E90074"))
    (setq evil-insert-state-cursor '((bar . 1) "#874CCC"))
    )
  ;; Disable Evil in some mode
  (dolist (nonevil-mode '(snails-mode
                          notdeft-mode
                          vterm-mode
                          netease-cloud-music-mode
                          cnfonts-ui-mode
                          Ilist-mode
                          TeX-output-mode
                          ebib-index-mode
                          ebib-entry-mode
                          ebib-strings-mode
                          minibuffer-mode
                          motd-message-mode
                          elfeed-search-mode
                          elfeed-show-mode
                          special-mode
                          evil-collection-mode
                          eca-chat-mode
                          ))
    (evil-set-initial-state nonevil-mode 'emacs))

  (evil-mode 1))

(use-package evil-numbers
  :after evil
  :demand t
  :bind (:map evil-normal-state-map
              ("C-a" . #'evil-numbers/inc-at-pt)
              ("C-A" . #'evil-numbers/dec-at-pt)))

;; *Evil's Easy Motion

(use-package avy :defer t)
(use-package evil-easymotion
  :init
  (defun avy-goto-char (char &optional arg)
    "Jump to the currently visible CHAR.
       The window scope is determined by `avy-all-windows' (ARG negates it)."
    (interactive (list (read-char "char: " t)
                       current-prefix-arg))
    (if (= ?  char)
        (call-interactively 'execute-extended-command)
      (avy-with avy-goto-char
        (avy-jump
         (if (= 13 char)
             "\n"
           (regexp-quote (string char)))
         :window-flip arg))))
  :bind (:map evil-normal-state-map
              ("SPC" . #'avy-goto-char))
  :after (evil avy)
  :defer t)

;; optional: this is the evil state that evil-magit will use
;; (setq evil-magit-state 'normal)
;; optional: disable additional bindings for yanking text
;; (setq evil-magit-use-y-for-yank nil)
(use-package evil-magit
  :ensure nil
  :after (evil magit)
  :defer t)

(provide 'config-evil)
