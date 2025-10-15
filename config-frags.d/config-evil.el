;; -*- lexical-binding: t; -*-
;;; Emacs Config Fragement: Evil
;;; The vim inside Emacs

;; * Evil
(use-package evil
  :disabled t
  :ensure t
  :demand t
  :after (undo-fu bind-key)
  :bind (:map evil-normal-state-map ("C-u" . #'evil-scroll-up))
  :init
  :config
  ;; ;; Use man (instead of WoMan) for man pages, although is slow in Emacs.
  ;; ;; Install man-db, check this: https://www.reddit.com/r/emacs/comments/mfmg3x/disabling_ivy_for_a_specific_command/
  ;; (evil-define-motion evil-lookup ()
  ;;   "Look up the keyword at point. Calls `evil-lookup-func'."
  ;;   (call-interactively #'man))

  (setopt evil-want-C-i-jump nil)
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

(defun meow-toggle-case ()
  "Toggle case of character at point or region (like Vim's `~`)."
  (interactive)
  (if (use-region-p)
      ;; 如果有选区，就在整个选区上切换大小写
      (let ((beg (region-beginning))
            (end (region-end)))
        (save-excursion
          (goto-char beg)
          (while (< (point) end)
            (let ((ch (char-after)))
              (when (and ch (char-equal (upcase ch) (downcase ch)))
                (insert (if (eq ch (upcase ch))
                            (downcase ch)
                          (upcase ch)))
                (delete-char 1)))
            (forward-char 1))))
    ;; 否则只切换光标所在字符
    (let ((ch (char-after)))
      (when ch
        (insert (if (eq ch (upcase ch))
                    (downcase ch)
                  (upcase ch)))
        (delete-char 1)))))



(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   ;; '("R" . meow-swap-grab)
   '("R" . undo-fu-only-redo)
   '("s" . meow-kill)
   '("t" . meow-till)
   ;; '("u" . meow-undo)
   '("u" . undo-fu-only-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("~" . meow-toggle-case)
   '("<escape>" . ignore)))

(use-package meow
  :config
  (meow-setup)
  (meow-global-mode 1))


(provide 'config-evil)
