;; -*- lexical-binding: t; -*-
;;; Emacs Config Fragement: Evil
;;; The vim inside Emacs

;; * Evil

(use-package evil
  ;; :disabled t
  :ensure t
  :demand t
  :preface
  (defun mk/scroll-page-forward ()
    "Scroll page forward."
    (interactive)
    (let* ((pix (floor (* (window-body-height (selected-window) t) -0.618))))
      (pixel-scroll-precision-interpolate pix nil 1)))
  (defun mk/scroll-page-backward ()
    "Scroll page backward."
    (interactive)
    (let* ((pix (floor (* (window-body-height (selected-window) t) +0.618))))
      (pixel-scroll-precision-interpolate pix nil 1)))
  :after (undo-fu)
  :bind (:map evil-normal-state-map
              ("C-u" . #'mk/scroll-page-backward)
              ("C-f" . #'mk/scroll-page-forward))
  :init
  (defvar mk/original-evil-normal-spc-binding nil
    "Original SPC binding for Evil normal mode.")
  (defun mk/evil-toggle-checkbox ()
    "Toggle [ ] and [X] at the point"
    (interactive)
    (if (and (eq evil-state 'normal)
               (not (region-active-p)))
      (save-excursion
        ;; 获取当前位置附近的两个字符
        (let ((two-chars (buffer-substring-no-properties
                          (max (point-min) (- (point) 0))
                          (min (point-max) (+ (point) 3)))))
          (cond
           ;; 如果是 [ ]
           ((string-match "\\[ \\]" two-chars)
            (search-backward "[" (line-beginning-position) t)
            (delete-char 3)
            (insert "[X]"))
           ;; 如果是 [X]
           ((string-match "\\[X\\]" two-chars)
            (search-backward "[" (line-beginning-position) t)
            (delete-char 3)
            (insert "[ ]")))))
      (when (commandp mk/original-evil-normal-spc-binding)
        (call-interactively mk/original-evil-normal-spc-binding))))
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
  (setq mk/original-evil-normal-spc-binding
        (lookup-key evil-normal-state-map (kbd "SPC")))
  (define-key evil-normal-state-map (kbd "SPC") #'mk/evil-toggle-checkbox)

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

(defun meow-undo-fu-undo ()
  "Cancel current selection then undo."
  (interactive)
  (when (region-active-p)
    (meow--cancel-selection))
  (undo-fu-only-undo))

(defun meow-undo-fu-redo ()
  "Cancel current selection then undo."
  (interactive)
  (when (region-active-p)
    (meow--cancel-selection))
  (undo-fu-only-redo))

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
   '("{" . meow-page-up)
   '("}" . meow-page-down)
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
   '("R" . meow-undo-fu-redo)
   '("s" . meow-kill)
   '("t" . meow-till)
   ;; '("u" . meow-undo)
   '("u" . meow-undo-fu-undo)
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
  :disabled t
  :config
  (meow-setup)
  (meow-global-mode 1))


(provide 'config-evil)
