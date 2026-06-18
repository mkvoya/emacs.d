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
  :vc (:url "https://github.com/copilot-emacs/copilot.el.git")
  :bind (:map copilot-completion-map ("<tab>" . #'copilot-accept-completion)
              ("TAB" . #'copilot-accept-completion))
  :defer t)

(use-package llm
  :vc (:url "https://github.com/ahyatt/llm")
  :config
  (setq llm-warn-on-nonfree nil))

(setq llm-sjtu-url "https://models.sjtu.edu.cn/api/v1")
(setq llm-sjtu-key (remove-trailing-newline (read-file-contents "~/.secrets/sjtu-api.key")))
(setq llm-sjtu-models '("qwen3.5-27b"
                        "glm-5.1"
                        "deepseek-v3.2"
                        "deepseek-chat"
                        "deepseek-reasoner"
                        "minimax-m2.7"))


(use-package ellama
  :ensure t
  :bind ("C-c e" . ellama)
  ;; send last message in chat buffer with C-c C-c
  :hook (org-ctrl-c-ctrl-c-hook . ellama-chat-send-last-message)
  :init (setopt ellama-auto-scroll t)
  :config
  ;; show ellama context in header line in all buffers
  (ellama-context-header-line-global-mode +1)
  ;; show ellama session id in header line in all buffers
  (ellama-session-header-line-global-mode +1)

  (setopt ellama-language "Chinese")
  (require 'llm-openai)
  (setopt ellama-provider
          (make-llm-openai-compatible
           :url llm-sjtu-url
           :key llm-sjtu-key
           :chat-model "qwen3.5-27b"))
  )

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el.git")
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (setenv "ANTHROPIC_BASE_URL" "https://anyrouter.top")
  (setenv "ANTHROPIC_AUTH_TOKEN" (remove-trailing-newline (read-file-contents "~/.secrets/anyrouter.key")))
  ;; (setenv "ANTHROPIC_BASE_URL" "https://open.bigmodel.cn/api/anthropic")
  ;; (setenv "ANTHROPIC_AUTH_TOKEN" (remove-trailing-newline (read-file-contents "~/.secrets/bigmodel.key")))
  (claude-code-ide-emacs-tools-setup)) ; Optionally enable Emacs MCP tools



(provide 'config-ai)
;;; config-ai.el ends here
