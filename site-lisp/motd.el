;;; motd.el --- Message of the day in Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'org-ql)

(defgroup motd nil "Message of the Day." :group 'emacs)

(defcustom motd-timer-interval 60
  "The time interval to detect the new day."
  :type 'number
  :group 'motd)

(defcustom motd-background-color "#FFFFFF"
  "The background color."
  :type 'string
  :group 'motd)

(defcustom motd-border-color "#000000"
  "The border color."
  :type 'string
  :group 'motd)

(defvar motd--today nil)
(defvar motd--buffer-name " *motd-message*")
(defvar motd--popup-frame nil)
(defvar motd--git-commit-dir nil)

(defun motd--git-commit ()
  "Generate git commit when motd message is shown."
  (when motd--git-commit-dir
    (shell-command (format "cd %s && git add -A && git commit -m \"%s\""
                           motd--git-commit-dir (format-time-string "%F")))
    (message (format "[motd-git] %s is committed." motd--git-commit-dir))
    ))

(defun motd--org-timestamp-day-diff (ts1 ts2)
  "Return the number of days from TS1 to TS2."
  (let ((start-day (org-time-string-to-absolute (org-timestamp-format ts1 "%F")))
        (end-day (org-time-string-to-absolute (org-timestamp-format ts2 "%F"))))
    (- end-day start-day)))

(defun motd--confddl--get-all ()
  "Get all pairs of conf ddls."
  (mapcar #'(lambda (h)
              (let* ((headline (plist-get h 'headline))
                     (deadline (plist-get headline :deadline))
                     (now (org-timestamp-from-time (current-time)))
                     (title (substring-no-properties (car (plist-get headline :title)))))
                (if (org-ql--org-timestamp-element< deadline now)
                    nil
                  (list deadline
                        (format "%s (%d days left): %s"
                                (org-timestamp-format deadline "%F")
                                (motd--org-timestamp-day-diff now deadline)
                                title)))))
          (org-ql-query
            :select 'element
            :from (org-agenda-files)
            :where '(todo "CONFDDL")
            :order-by 'deadline)))

(defun motd--confddl-message ()
  "Get first 3 confddl from org files."
  (mapconcat #'(lambda (pair)
                 (car (cdr pair)))
             (-take 3
                    (-sort #'(lambda (pair1 pair2)
                               (org-ql--org-timestamp-element< (car pair1) (car pair2)))
                           (motd--confddl--get-all)))
             "\n"))

(defun motd--report-content ()
  "Generate the report content of the motd box."
  (format "<<Emacs Daily Report>>
Recent Submission Deadlines:
%s\n" (motd--confddl-message)))


(defun motd--popup-ns (text)
  "Show TEXT in the NS native popup."
  (x-popup-dialog t `(,text
                      ("Love You 😘 .x..." . t)) t)
  )

(defun motd--create-frame (buffer)
  "Create a floating popup frame with BUFFER as content."
  (let*  ((parent (window-frame))
          (f (make-frame `((parent-frame . ,parent)
                           (undecorated . 1)
                           (name . "*Motd Frame*")
                           (border-width . 5)
                           (internal-border-width . 10)
                           (minibuffer . ,(minibuffer-window parent))
                           (left-fringe . 0)
                           (right-fringe . 0)
                           (menu-bar-lines . 0)
                           (tool-bar-lines . 0)
                           (tab-bar-lines . 0)
                           (top-bar-lines . 0)
                           (mode-line-format . nil)
                           (header-line-format . nil)
                           )))
          (win (frame-root-window f)))
    (set-frame-size f
                    (/ (* (frame-inner-width) 4) 5)
                    (/ (frame-inner-height) 3)
                    t)
    (set-frame-position f
                        (/ (frame-inner-width) 10)
                        (/ (frame-inner-height) 6))
    (set-frame-parameter f 'background-color motd-background-color)
    (set-frame-parameter f 'border-color motd-border-color)
    (set-window-buffer win buffer)
    (set-window-dedicated-p win t)
  f
  ))

(defun motd--popup (text)
  "Show TEXT in the popup frame."
  (with-current-buffer (get-buffer-create motd--buffer-name)
    (read-only-mode -1)
    (erase-buffer)
    (motd-message-mode t)
    (insert text)
    (insert "\n")
    (let ((padding (/ (- (frame-width) 40) 2)))
      (insert (format (format "%%%ds" padding) ""))
      (insert "[ OK ]")
      (insert (format (format "%%%ds" padding) ""))
      (insert "\n"))
    (insert "\n")
    (insert "\n")
    (setq mode-line-format nil)
    (when evil-mode
      (evil-change-state 'emacs))
    (read-only-mode t))
  (setq motd--popup-frame
        (motd--create-frame motd--buffer-name)))

(defun motd--timeout-handler ()
  "Remind you important things upon the first touch of Emacs in the day."
  (let ((day (format-time-string "%d")))
    (unless (or (equal day motd--today) (not (frame-focus-state)))
      (let ((text (motd--report-content)))
        (motd--popup text)
        ;; (motd--popup-ns text)
        )
      (motd--git-commit)
      (setq motd--today day))))

(defvar motd--timer nil)

(defun motd-start-timer ()
  "Start the timer for the motd daemon."
  (interactive)
  (setq motd--timer
        (run-with-idle-timer 0 'motd-timer-interval #'motd--timeout-handler)))

(defun motd-stop-timer ()
  "Stop the timer for the motd daemon."
  (interactive)
  (cancel-timer motd--timer)
  (setq motd--timer nil))

(defun motd--remove-popup ()
  "Delete the popup frame."
  (interactive)
  (when motd--popup-frame
    (delete-frame motd--popup-frame)
    (setq motd--popup-frame nil)
    )
  )

(define-minor-mode motd-message-mode
  "Minor mode to show motd message."
  :lighter ""
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "RET") 'motd--remove-popup)
            (define-key map (kbd "X") 'motd--remove-popup)
            (define-key map (kbd "q") #'(lambda () (interactive) (message "HI")))
            map))

(provide 'motd)
;;; motd.el ends here
