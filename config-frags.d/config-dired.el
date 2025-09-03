;; -*- lexical-binding: t; -*-
;;; Emacs Config Fragement: Dired

(use-package neotree :defer t)
;; (use-package perspective :config (persp-mode))


(defun mk/dired-open-pdf ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    (call-process "open" nil 0 nil "-a" "PDF Expert" file)
    (message "Opening %s done" file)))
;;(define-key dired-mode-map "E" 'mk/dired-open-pdf)

(defun mk/dired-curl ()
  "Get file from url to dired."
  (interactive)
  (when (equal major-mode 'dired-mode)
    (let* ((url (read-string "URL: "))
           (cwd (dired-current-directory))
           (target (concat (file-name-as-directory cwd) (file-name-nondirectory url))))
      (url-copy-file url target)
      )))

(use-package diredfl
  :defer t
  :config (diredfl-global-mode))
(use-package dired+ :ensure (:host github :repo "emacsmirror/dired-plus")
  :defer t)
(use-package dired-preview
  :config
  ;; Default values for demo purposes
  (setq dired-preview-delay 0.7)
  (setq dired-preview-max-size (expt 2 20))
  (setq dired-preview-ignored-extensions-regexp
        (concat "\\."
                "\\(mkv\\|webm\\|mp4\\|mp3\\|ogg\\|m4a"
                "\\|gz\\|zst\\|tar\\|xz\\|rar\\|zip"
                "\\|iso\\|epub\\|pdf\\)"))
  :defer t)

(use-package dirvish :defer t)

(provide 'config-dired)
