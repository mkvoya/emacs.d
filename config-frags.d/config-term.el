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
  :vc (:url "https://github.com/jixiuf/vterm-toggle.git")
  :after (vterm)
  :defer t
  :bind (("C-`" . vterm-toggle)
         ("<C-f2>" . vterm-toggle-cd)
         (:map vterm-mode-map
               ("C-<return>" . vterm-toggle-insert-cd)
               ("s-n" . vterm-toggle-forward)
               ("s-p" . vterm-toggle-backward))))


;; (use-package ghostel
;;   :vc (:url "https://github.com/dakra/ghostel.git")


(provide 'config-term)
