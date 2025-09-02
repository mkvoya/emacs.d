;; -*- lexical-binding: t; -*-
;;; Emacs Config Fragement: Others

(defun mk/insert-datetime()
  "Insert the current date and time."
  (interactive)
  (insert (format-time-string "%F %T")))

(use-package golden-ratio
  :ensure (:host github :repo "roman/golden-ratio.el" :files ("*.el")))

(use-package unkillable-scratch
  :config (progn (unkillable-scratch 1)))
(use-package scratch)

;; Speedy reading
(use-package spray
  :ensure (:host github :repo "emacsmirror/spray")
  :after (bind-key)
  :config (progn
            (setq spray-wpm 400
                  spray-margin-left 4
                  spray-margin-top 12)
            (bind-key "+" 'spray-faster spray-mode-map)
            (bind-key "-" 'spray-slower spray-mode-map)
            (add-to-list 'spray-unsupported-minor-modes 'beacon-mode)))

(use-package powerthesaurus
  ;; :bind ("C-c w p" . powerthesaurus-lookup-dwim)
  :config (add-to-list 'display-buffer-alist
                       `(,(rx bos "*Powerthesaurus - " (0+ any) "*" eos)
                         (display-buffer-in-side-window)
                         (side . right)
                         (slot . 0)
                         (window-width . 80))))

(use-package focus)


(defun mk/count-today-todos ()
  "Count the number of today's to-do tasks."
  (interactive)
  (let ((count 0)
        (today (format-time-string "%Y-%m-%d")))
    (org-compile-prefix-format 'todo)
    (org-map-entries
     (lambda ()
       (when (and (string= (org-get-todo-state) "TODO")
                  (or (string= (org-entry-get nil "SCHEDULED") today)
                      (and (org-entry-get nil "DEADLINE")
                           (string< today (org-entry-get nil "DEADLINE")))
                      ))
         (setq count (1+ count))))
     nil 'agenda)
    count)
  )

(use-package format-all)

(defun mk/--pick-project ()
  "Pick a project to do."
  (seq-random-elt
   (mapcar #'(lambda (h)
               (let* ((headline (plist-get h 'headline))
                      (title (substring-no-properties (car (plist-get headline :title)))))
                 title))
           (org-ql-query
             :select 'element
             :from "~/Dropbox/Dreams/Org/Projects.org"
             :where '(todo "PROJECT")
             :order-by 'deadline))))

(defun mk/pick-project ()
  "Pick a project to do."
  (interactive)
  (message (mk/--pick-project)))

(use-package chronos :defer t)
(use-package elquery :defer)
(use-package citeproc :defer)

(use-package xkcd :defer)

(use-package nyan-mode
  :ensure (:repo "TeMPOraL/nyan-mode")
  )


(provide 'config-misc)
