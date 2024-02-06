  (use-package calendar
    :config
    (defun mk/notmuch-open-unread-view()
      "Open notmuch panel for unread mails."
      (interactive)
      (require 'notmuch)
      (notmuch-search "tag:unread"))
    (defun mk/notmuch-count-unread()
      "Get the notmuch unread mail count if notmuch is loaded."
      (if (fboundp 'notmuch-command-to-string)
          (replace-regexp-in-string "\n" ""
                                    (notmuch-command-to-string "count"
                                                               "tag:unread"))
        "?"))
    (setq display-time-string-forms
          '((format-time-string "%F %H:%M")
            ;; (propertize
            ;;  (format " [M:%s]" (mk/notmuch-count-unread))
            ;;  'help-echo  "Unread!"
            ;;  'keymap  (let ((map (make-sparse-keymap)))
            ;;             (define-key map [mouse-1]
            ;;                         #'mk/notmuch-open-unread-view)
            ;;             map))
            ))
    (display-time-mode 1))
