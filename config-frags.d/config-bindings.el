;; -*- lexical-binding: t; -*-
;;; Emacs Config Fragement: Custom Key Bindings

;; (require 'consult)
(defun mk/live-search-config-frags ()
  "Live search in ~/.emacs.d/config-frags.d/ using consult-ripgrep.
If nothing is entered, open config-misc.el."
  (interactive)
  (let ((dir (expand-file-name "config-frags.d" user-emacs-directory))
        (default-file (expand-file-name "config-misc.el" (expand-file-name "config-frags.d" user-emacs-directory))))
    (if (fboundp 'consult-ripgrep)
        ;; 直接调用 consult-ripgrep，不要去声明 consult-ripgrep-args
        (consult-ripgrep dir)
      ;; fallback
      (message "consult-ripgrep not installed! Opening default file.")
      (find-file default-file))))


(global-set-key (kbd "C-h c") #'describe-char)
(global-set-key (kbd "C-c h") (lambda () (interactive) (find-file "~/Dropbox/Dreams/Org/Main.org")))
(global-set-key (kbd "C-c t") (lambda () (interactive) (find-file "~/Dropbox/Dreams/Org/Tasks.org")))
(global-set-key (kbd "C-c g") (lambda () (interactive) (find-file "~/Dropbox/Dreams/Org/Blog/all-posts.org")))
(global-set-key (kbd "C-c i") (lambda () (interactive) (find-file "~/Dropbox/Dreams/Org/Inbox.org")))
(global-set-key (kbd "C-c p") (lambda () (interactive) (find-file "~/Dropbox/Dreams/Org/Projects.org")))
;; (global-set-key (kbd "C-c r") (lambda () (interactive) (find-file "~/.emacs.d/config-frags.d/config-misc.el")))
(global-set-key (kbd "C-c r") 'mk/live-search-config-frags)
(global-set-key (kbd "M-s-<left>") 'tab-previous)
(global-set-key (kbd "M-s-<right>") 'tab-next)
(global-set-key (kbd "M-s-n") 'tab-new)
;; (global-set-key (kbd "C-c w") (lambda () (interactive) (find-file "~/Dropbox/Dreams/Org/Weights.org")))
;; Open ibuffer upon "C-c i"
(global-set-key (kbd "C-c b") 'ibuffer)
;; (global-set-key (kbd "C-c C-m e") (lambda () (interactive) (find-file "~/.emacs.d/emacs-config.org")))
;; (global-unset-key [mouse-3])
;; (global-set-key [down-mouse-3]
;;                 `(menu-item ,(purecopy "Menu Bar") ignore
;;                             :filter (lambda (_)
;;                                       (if (zerop (or (frame-parameter nil 'menu-bar-lines) 0))
;;                                           (mouse-menu-bar-map)
;;                                         (mouse-menu-major-mode-map)))))

(provide 'config-bindings)
