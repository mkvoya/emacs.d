;; -*- lexical-binding: t; -*-
;;; Emacs Config Fragement: MK's customized packages

(use-package async-cmd
  :ensure (:type git :host github :repo "mkvoya/async-cmd.el"))


(use-package motd
  :after (dash org)
  :defer 20
  :ensure nil
  :load-path "~/.emacs.d/site-lisp/"
  :config
  (setq motd-background-color "#204230")
  (setq motd-border-color "#444444")
  (setq motd--git-commit-dir "~/Dropbox/Dreams")
  (motd-start-timer)
  )

(use-package xwidget-apps
  :after (websocket)
  :defer t
  :ensure nil
  :commands (xwidget-dict-lookup-at-point)
  :load-path "~/.emacs.d/site-lisp/xwidget-apps/"
  :config
  (xwidget-dict-mode)
  )

(defun mk/count-today-todos ()
  "Count the number of today's to-do tasks."
  (interactive)
  (let ((count 0)
        (today (format-time-string "%Y-%m-%d")))
    (when (fboundp 'org-compile-prefix-format)
      (org-compile-prefix-format 'todo)
      (org-map-entries
       (lambda ()
         (when (and (string= (org-get-todo-state) "TODO")
                    (or (string= (org-entry-get nil "SCHEDULED") today)
                        (and (org-entry-get nil "DEADLINE")
                             (string< today (org-entry-get nil "DEADLINE")))
                        ))
           (setq count (1+ count))))
       nil 'agenda))
    count)
  )

(defun mk/count-weekly-todos ()
  "Count the number of TODO entries in an Org FILE using org-map-entries."
  (with-current-buffer (find-file-noselect "~/Dropbox/Dreams/Org/Tasks.org")
    (org-with-wide-buffer
     (length (org-map-entries t "/+TODO")))))


(use-package emacs-badge
  :demand
  :ensure (:type git :host github :repo "mkvoya/emacs-badge" :files ("*"))
  :config
  (setq emacs-badge-timer
        (run-with-timer
         30 30
         '(lambda()
            (emacs-badge-update (format "%s" (mk/count-weekly-todos)))))))


(provide 'init-writing)
(load-file "~/.emacs.d/site-lisp/tex-autogen.el")
(load-file "~/.emacs.d/site-lisp/wc.el")




;; Helper: 判断当前是否交互打开文件
(defun my/find-file-interactive-p ()
  "Return non-nil if the current file is opened interactively (e.g., via C-x C-f)."
  (or (called-interactively-p 'any)
      (not (bound-and-true-p noninteractive))))

;; Advice: 当非交互调用 `find-file` 时临时禁用 Tree-sitter
(defun my/disable-treesit-when-noninteractive (orig-fun &rest args)
  "Disable Tree-sitter remapping when not opening interactively."
  (if (my/find-file-interactive-p)
      ;; 正常交互打开：照常启用 Tree-sitter
      (apply orig-fun args)
    ;; 非交互打开：临时禁用 major-mode remapping
    (let ((major-mode-remap-alist nil))
      (apply orig-fun args))))

(advice-add 'find-file :around #'my/disable-treesit-when-noninteractive)
(advice-add 'find-file-noselect :around #'my/disable-treesit-when-noninteractive)



;;; --- Tree-sitter Performance Optimization ---

(defvar my/treesit-available-cache (make-hash-table :test 'equal)
  "Cache for treesit-language-available-p results.")

(advice-add 'treesit-language-available-p :around
            (lambda (orig-fun &rest args)
              (let ((lang (car args))) ;; treesit-language-available-p 只有一个参数
                (or (gethash lang my/treesit-available-cache)
                    (puthash lang
                             (apply orig-fun args)
                             my/treesit-available-cache)))))

;; 非交互模式下禁用 Tree-sitter
(when noninteractive
  (setq major-mode-remap-alist nil)
  (defun treesit-language-available-p (&rest _) nil))


(provide 'config-mk)
