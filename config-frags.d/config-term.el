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
  :after (vterm)
  :defer t
  :bind (("C-c t" . vterm-toggle)
         ("C-`" . vterm-toggle)
         ("<C-f2>" . vterm-toggle-cd)
         (:map vterm-mode-map
               ("C-<return>" . vterm-toggle-insert-cd)
               ("s-n" . vterm-toggle-forward)
               ("s-p" . vterm-toggle-backward))))

(use-package shrink-path
  :ensure (:host github :repo "zbelial/shrink-path.el" :fetcher github)
  :defer t)


(provide 'config-term)
