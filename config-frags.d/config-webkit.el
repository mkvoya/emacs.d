;; -*- lexical-binding: t; -*-
;;; Emacs Config Fragement: Webkit tools

  ;; From https://emacs.stackexchange.com/questions/47627/identify-buffer-by-part-of-its-name
  (defun switch-to-existing-buffer-other-window (part)
    "Switch to buffer with PART in its name."
    (interactive
     (list (read-buffer-to-switch "Switch to buffer in other window: ")))
    (let ((candidates
           (cl-remove
            nil
            (mapcar (lambda (buf)
                      (let ((pos (string-match part (buffer-name buf))))
                        (when pos
                          (cons pos buf))))
                    (buffer-list)))))
      (unless candidates
        (user-error "There is no buffers with %S in its name." part))
      (setq candidates (cl-sort candidates #'< :key 'car))
      (switch-to-buffer-other-window (cdr (car candidates)))))

  (defun mk/webkit-open-orgroam-ui()
    "Open the Org-roam client at [http://127.0.0.1:35901]."
    (interactive)
    ;; Ensure the server is running.
    (unless org-roam-ui-mode (org-roam-ui-mode))
    ;; Ensure the session is running.
    (xwidget-webkit-browse-url "http://127.0.0.1:35901" nil)
    ;; Switch to the buffer
    (switch-to-existing-buffer-other-window "Roam Server")
    )
  (defun mk/webkit-new-url (url &optional new-session)
    "Create a new session to browse the URL."
    (interactive (progn
                   (require 'browse-url)
                   (browse-url-interactive-arg "xwidget-webkit URL: ")))
    (xwidget-webkit-browse-url url t)
    )
  (defun mk/webkit-open-activitywatch()
    "Open the ActivityWatch client at [http://localhost:5600/]."
    (interactive)
    (xwidget-webkit-browse-url "http://127.0.0.1:5600" nil)
    )

(provide 'config-webkit)
