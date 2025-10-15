;; -*- lexical-binding: t; -*-
;;; Emacs Config Fragement: AI Tools

(defun remove-trailing-newline (str)
  "Remove the trailing newline character from STR, if it exists."
  (if (string-suffix-p "\n" str)
      (substring str 0 -1)
    str))

(defun read-file-contents (file-path)
  "Read the contents of FILE-PATH and return it as a string."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun gptel-api-key ()
  (remove-trailing-newline (read-file-contents "~/.secrets/ipads-chatgpt.key")))

(use-package copilot
  :ensure (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :bind (:map copilot-completion-map ("<tab>" . #'copilot-accept-completion)
              ("TAB" . #'copilot-accept-completion))
  :defer t)

(use-package elysium
  :ensure (:host github :repo "lanceberge/elysium" :branch "master" :files ("*.el"))
  :defer t
  :custom
  ;; Below are the default values
  (elysium-window-size 0.33) ; The elysium buffer will be 1/3 your screen
  (elysium-window-style 'vertical)) ; Can be customized to horizontal

(use-package smerge-mode
  :ensure nil
  :hook (prog-mode . smerge-mode))

(use-package gptel
  :ensure (:host github :repo "karthink/gptel" :branch "master" :files ("*"))
  :config
  (setq gptel-use-curl t)
  (setq gptel-model 'gpt-4.1)
  (setq gptel-backend
   (gptel-make-openai "IPADS GPT"
     ;; :host "10.0.0.10:3006"
     ;; :host "ipads.chat.gpt:3006"
     :host "dong.mk:3006"
     :protocol "http"
     :stream t
     :key #'gptel-api-key
     :models '(
               "gpt-4.1"
               "gemini-2.5-flash"
               "gemini-2.5-pro"
               "claude-3-5"
               "gpt-5"
               "gpt-5-mini"
               "gpt-5-nano"
               "gpt-image-1"
               "anthropic/claude-sonnet-4"
               "anthropic/claude-opus-4.1"
               "x-ai/grok-4"
               "deepseek-r1"
               "deepseek-v3"))))

(use-package inline-diff
  :ensure (:repo "https://code.tecosaur.net/tec/inline-diff")
  :after gptel-rewrite)

;; Updated version available at https://github.com/karthink/gptel/wiki
(use-package gptel-rewrite
  :ensure nil
  :after gptel
  :bind (:map gptel-rewrite-actions-map
         ("C-c C-i" . gptel--rewrite-inline-diff))
  :config
  (defun gptel--rewrite-inline-diff (&optional ovs)
    "Start an inline-diff session on OVS."
    (interactive (list (gptel--rewrite-overlay-at)))
    (unless (require 'inline-diff nil t)
      (user-error "Inline diffs require the inline-diff package."))
    (when-let* ((ov-buf (overlay-buffer (or (car-safe ovs) ovs)))
                ((buffer-live-p ov-buf)))
      (with-current-buffer ov-buf
        (cl-loop for ov in (ensure-list ovs)
                 for ov-beg = (overlay-start ov)
                 for ov-end = (overlay-end ov)
                 for response = (overlay-get ov 'gptel-rewrite)
                 do (delete-overlay ov)
                 (inline-diff-words
                  ov-beg ov-end response)))))
  (when (boundp 'gptel--rewrite-dispatch-actions)
    (add-to-list
     'gptel--rewrite-dispatch-actions '(?i "inline-diff")
     'append)))

;; install claude-code.el:
(use-package claude-code
  :ensure (:type git :host github :repo "stevemolitor/claude-code.el" :branch "main"
                 :files ("*.el" (:exclude "images/*")))
  :bind-keymap ("C-c c" . claude-code-command-map) ;; or your preferred key
  :config
  (setq claude-code-terminal-backend 'vterm)

  (defun read-claude-auth-token ()
    "Read the contents of FILE-PATH and return it as a string."
    (with-temp-buffer
      (insert-file-contents "~/.secrets/claude-api.key")
      (remove-trailing-newline (buffer-string))))
  (add-to-list 'vterm-environment "ANTHROPIC_BASE_URL=https://anyrouter.top")
  (add-to-list 'vterm-environment (concat "ANTHROPIC_AUTH_TOKEN=" (read-claude-auth-token)))

  ;; macOS notification
  (defun my-claude-notify (title message)
    "Display a macOS notification with sound."
    (call-process "osascript" nil nil nil
                  "-e" (format "display notification \"%s\" with title \"%s\" sound name \"Glass\""
                               message title)))
  (setq claude-code-notification-function #'my-claude-notify)

  ;; Allow vterm windows to be as narrow as 40 columns
  (setopt vterm-min-window-width 40)

  (add-to-list 'display-buffer-alist
               '("^\\*claude"
                 (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 90)))

  ;; important - tell emacs to use our fontset settings
  (setq use-default-font-for-symbols nil)

  ;; add least preferred fonts first, most preferred last
  (set-fontset-font t 'symbol "STIX Two Math" nil 'prepend)
  (set-fontset-font t 'symbol "Zapf Dingbats" nil 'prepend)
  (set-fontset-font t 'symbol "Menlo" nil 'prepend)

  ;; add your default, preferred font last
  (set-fontset-font t 'symbol "Maple Mono" nil 'prepend)

  ;; increase the buffer size for claude code
  (add-hook 'claude-code-start-hook
            (lambda ()
              ;; Only increase scrollback for vterm backend
              (when (eq claude-code-terminal-backend 'vterm)
                (setq-local vterm-max-scrollback 100000))))

  (claude-code-mode))

(use-package claude-code-ide
  :ensure (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (claude-code-ide-emacs-tools-setup)) ; Optionally enable Emacs MCP tools



(provide 'config-ai)
;;; config-ai.el ends here
