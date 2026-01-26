;; -*- lexical-binding: t; -*-
;;; Emacs Configuration Fragment: Mail


(use-package mu4e
  :ensure nil
  :load-path "/opt/homebrew/opt/mu/share/emacs/site-lisp/mu/mu4e/"
  :init
  (setq mu4e-mu-binary (or (executable-find "mu") "/usr/local/bin/mu"))
  :config
  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)

  ;; Refresh mail using isync every 10 minutes
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -c ~/.emacs.d/site-rc/mbsyncrc -a")
  (setq mu4e-maildir "~/mails")

  (setq mu4e-contexts
        (list
         ;; Work account
         (make-mu4e-context
          :name "SJTU"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/sjtu" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "mingkaidong@sjtu.edu.cn")
                  (user-full-name    . "Mingkai Dong")
                  (smtpmail-smtp-server  . "mail.sjtu.edu.cn")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-stream-type  . ssl)
                  (mu4e-drafts-folder  . "/sjtu/Drafts")
                  (mu4e-sent-folder  . "/sjtu/Sent")
                  (mu4e-refile-folder  . "/sjtu/Archive")
                  (mu4e-trash-folder  . "/sjtu/Trash")))
         ;; Personal account
         (make-mu4e-context
          :name "Gmail"
          :match-func
            (lambda (msg)
              (when msg
                (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "mingkaidong@gmail.com")
                  (user-full-name    . "Mingkai Dong (Gmail)")
                  (smtpmail-smtp-server  . "smtp.gmail.com")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-stream-type  . ssl)
                  (mu4e-drafts-folder  . "/gmail/[Gmail]/Drafts")
                  (mu4e-sent-folder  . "/gmail/[Gmail]/Sent Mail")
                  (mu4e-refile-folder  . "/gmail/[Gmail]/All Mail")
                  (mu4e-trash-folder  . "/gmail/[Gmail]/Trash")))
         ;; Personal account 2
         (make-mu4e-context
          :name "mk"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/mk" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "mk@dong.mk")
                  (user-full-name    . "Mingkai Dong")
                  (smtpmail-smtp-server  . "postale.io")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-stream-type  . ssl)
                  (mu4e-drafts-folder  . "/mk/Drafts")
                  (mu4e-sent-folder  . "/mk/Sent")
                  (mu4e-refile-folder  . "/mk/Archive")
                  (mu4e-trash-folder  . "/mk/Trash")))))

  (setq message-send-mail-function 'smtpmail-send-it)
  ;; Only ask if a context hasn't been previously picked
  (setq mu4e-compose-context-policy 'ask-if-none)
  ;; Make sure plain text mails flow correctly for recipients
  (setq mu4e-compose-format-flowed t)

  (setq mu4e-maildir-shortcuts
      '(("/Inbox"             . ?i)
        ("/gmail/[Gmail]/Sent Mail" . ?s)
        ("/gmail/[Gmail]/Trash"     . ?t)
        ("/gmail/[Gmail]/Drafts"    . ?d)
        ("/gmail/[Gmail]/Archive"  . ?a))))

;; (use-package mu4easy
;;   :ensure (:host github :repo "danielfleischer/mu4easy" :files ("*.el"))
;;   :after (mu4e)
;;   :bind ("C-c u" . mu4e)
;;   :config
;;   (setq mu4easy-contexts
;;         '((mu4easy-context
;;            :c-name  "Gmail"
;;            :maildir "gmail"
;;            :mail    "mingkaidong@gmail.com"
;;            :smtp    "smtp.gmail.com"
;;            :sent-action delete) ; Gmail automatically saves sent messages in both "Sent Mail" and "All Mail," so setting mu4e-sent-messages-behavior to 'delete' prevents duplicate copies in mu4e's view.
;;           (mu4easy-context
;;            :c-name  "SJTU"
;;            :maildir "sjtu"
;;            :mail    "mingkaidong@sjtu.edu.cn"
;;            :smtp    "mail.sjtu.edu.cn")
;;           (mu4easy-context
;;            :c-name  "dong.mk"
;;            :maildir "mk"
;;            :mail    "mk@dong.mk"
;;            :smtp    "smtp.postale.io")))
;;   (setq mu4e-mu-binary (or (executable-find "mu") "/usr/local/bin/mu"))
;;   (mu4easy-mode))


(provide 'config-mail)
