;;; This file is a black hole, containing all obsolete packages and configurations.


(use-package org-zettel-ref-mode
  :disabled t
  :ensure (:host github :repo "yibie/org-zettel-ref-mode" :files ("*"))
  :init
  (setq org-zettel-ref-overview-directory "~/Dropbox/Dreams/Org/orgzet/source-note/")
  :config
  (setq org-zettel-ref-mode-type 'denote)
  ;; (setq org-zettel-ref-mode-type 'org-roam)
  ;; (setq org-zettel-ref-mode-type 'normal)
  (setq org-zettel-ref-python-file "~/.emacs.d/ensure/repos/org-zettel-ref-mode/convert-to-org.py")
  (setq org-zettel-ref-temp-folder "/Volumes/ramfs/convert/")
  (setq org-zettel-ref-reference-folder "~/Dropbox/Dreams/Org/orgzet/ref/")
  (setq org-zettel-ref-archive-folder "~/Dropbox/Dreams/Org/orgzet/archives/")
  (setq org-zettel-ref-debug t)
  (setq org-zettel-ref-python-environment 'system)
  (defun mk/org-get-webpage ()
    "Retrive a web page from a given url and save it to the temp-folder. Then call org-zettel-run-python-script to convert it to org-mode format."
    (interactive)
    (let ((url (read-string "URL: ")))
      (call-process "curl" nil 0 nil "-o" (concat org-zettel-ref-temp-folder "webpage.html") url)
      (org-zettel-run-python-script)
      )
    )
  )

(use-package org-roam
  :disabled t
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Dropbox/Dreams/Org/"))
  ;; (org-roam-dailies-directory "Journals/")
  ;; (org-roam-dailies-capture-templates '(("d" "default" entry
  ;;                                        "* %<%H:%M>: %?"
  ;;                                        :target (file+datetree "LatestJournal.org" week))))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         )
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol)
  )

(use-package org-roam-ui
  :disabled t
  :ensure
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (elpaca-after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))


(use-package aidermacs
  :disabled t
  :ensure (:host github :repo "MatthewZMD/aidermacs" :files ("*.el"))
  :config
  (setq aidermacs-default-model "openai/gpt-4o")
  (setenv "OPENAI_API_BASE" "http://ipads.chat.gpt:3006/v1")
  (setenv "OPENAI_API_KEY" (gptel-api-key))
  (global-set-key (kbd "C-c k") 'aidermacs-transient-menu)
                                        ; See the Configuration section below

  (setq aidermacs-backend 'vterm)
  (setq aidermacs-comint-multiline-newline-key "C-<return>")

  (setq aidermacs-auto-commits t)
  (setq aidermacs-use-architect-mode nil))

(use-package eca
  :ensure (:host github :repo "editor-code-assistant/eca-emacs" :files ("*.el"))
  :config
  (setenv "OPENAI_API_BASE" "http://ipads.chat.gpt:3006/v1")
  (setenv "OPENAI_API_KEY" (gptel-api-key))
  )

;; * Super Alignment
(use-package valign :disabled t ; org-modern has this feature (and they conflict)
  :hook (org-mode . valign-mode)
  )
