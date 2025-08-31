;; -*- lexical-binding: t; -*-
;;; Emacs Config Fragement: Terminal

(use-package vterm
  :defer t
  :config
  (defun vterm-new()
    "Add a new vterm session with given name."
    (interactive)
    (let ((session-name (string-trim (read-string "Enter the name for the session: "))))
      (vterm session-name)
      )))

(use-package vterm-toggle
  :ensure (:host github :repo "jixiuf/vterm-toggle")
  :config
  (global-set-key (kbd "C-c t") 'vterm-toggle)
  (global-set-key (kbd "C-`") 'vterm-toggle)
  (global-set-key [C-f2] 'vterm-toggle-cd)

  ;; you can cd to the directory where your previous buffer file exists
  ;; after you have toggle to the vterm buffer with `vterm-toggle'.
  (define-key vterm-mode-map [(control return)]   #'vterm-toggle-insert-cd)

                                        ;Switch to next vterm buffer
  (define-key vterm-mode-map (kbd "s-n")   'vterm-toggle-forward)
                                        ;Switch to previous vterm buffer
  (define-key vterm-mode-map (kbd "s-p")   'vterm-toggle-backward)
  )

(use-package shrink-path
  :ensure (:host github :repo "zbelial/shrink-path.el" :fetcher github))


(provide 'config-term)
