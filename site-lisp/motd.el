;;; motd.el --- Message of the day in Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'org-ql)
(require 'org)
(require 'async-cmd)

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
(defvar motd--buffer-name "*Motd Message*")
(defvar motd--git-commit-dir nil)

(defvar motd--timer nil)
(defvar motd--fade-timer nil)

(defvar motd--buffer (get-buffer-create "*Motd Message*"))

(defun show-notification (buffer-name)
  "Show a notification in a floating transparent frame with dark background for 30 seconds."
  (let* ((notification-buffer buffer-name)
         (current-frame (selected-frame))
         (frame-width (frame-pixel-width current-frame))
         (left (frame-parameter current-frame 'left))
         (frame-pos-x (- (+ 0 (if (listp left) (nth 1 left) left) frame-width)
                         (* (+ 40 4) (frame-char-width)))) ; Right align
         (frame-top (frame-parameter current-frame 'top))
         (frame-pos-y (+ (if (integerp frame-top) frame-top (car (cdr frame-top))) 20))
         (frame-params `((name . "notification")
                         (width . 80)
                         (minibuffer . nil)
                         (left . ,frame-pos-x)
                         (top . ,frame-pos-y)
                         (no-accept-focus . t)
                         (no-focus-on-map . t)
                         (internal-border-width . 10)
                         (drag-internal-border . t)
                         (undecorated . t)
                         (background-color . "#2d2d2d")
                         (foreground-color . "#ffffff")
                         (alpha . (95 . 90))
                         (menu-bar-lines . 0)
                         (tool-bar-lines . 0)
                         (tab-bar-lines . 0)
                         (header-line-format . nil)
                         (mode-line-format . nil)
                         (left-fringe . 0)
                         (right-fringe . 0)))
         (frame (make-frame frame-params)))
    (set-window-buffer (frame-selected-window frame) notification-buffer)
    (set-window-parameter (frame-selected-window frame) 'mode-line-format 'none)
    (set-window-parameter (frame-selected-window frame) 'header-line-format 'none)
    (run-with-timer 30 nil
                    (lambda ()
                      (when (frame-live-p frame)
                        (delete-frame frame))))))

(defun motd--git-commit ()
  "Generate git commit when motd message is shown."
  (let* ((arg-string (format "cd %s && git add -A && git commit -m \"%s\""
                             motd--git-commit-dir
                             (format-time-string "%F"))))
    (async-cmd-start
     "sh" `("-c" ,arg-string)
     (lambda (id status buf)
       (message "[motd-git] %s is committed." motd--git-commit-dir)))))

;;  (format "Auto Git Commit:\n%s\n"

(defun motd--gitlab-commit-summary ()
  "Call gitlab monitor script asynchronously and append output to motd buffer."
  (async-cmd-start "python3" `(,(expand-file-name "~/.emacs.d/site-tools/GitlabMonitor/gitlab_monitor.py"))))

(defun motd--ccfddl-summary ()
  "Call ccfddl script asynchronously and append output to motd buffer."
  (async-cmd-start "python3" `(,(expand-file-name "/Users/dongmk/.emacs.d/site-tools/CCFDDL/conference_deadlines.py"))))

(defun motd--report-content-to-buffer ()
  "Generate the report content of the motd box."
  (let ((ids (list
              (motd--git-commit)
              (motd--ccfddl-summary)
              (motd--gitlab-commit-summary))))
    (async-cmd-wait-and-cleanup
     ids
     (lambda (results) ;; (list id exist-status buffer name)
       (with-current-buffer motd--buffer
         (erase-buffer)
         (insert "<<Emacs Daily Report>>\n")
         (dolist (it results)
           (let ((buf (cdr it)))
             (when (buffer-live-p buf)
               (with-current-buffer motd--buffer
                 (insert (format "\n%s\n" (with-current-buffer buf (buffer-string))))
                 ))))
         (goto-char (point-min)))
         (show-notification motd--buffer)))))

(defun motd--timeout-handler ()
  "Remind you important things upon the first touch of Emacs in the day."
  (let ((day (format-time-string "%d")))
    (unless (or (equal day motd--today) (not (frame-focus-state)))
      (setq motd--today day)
      (make-thread (lambda () (motd--report-content-to-buffer))))))

(defun motd-start-timer ()
  "Start the timer for the motd daemon."
  (interactive)
  (if motd--timer
      (message "Timer is already up.")
    (setq motd--timer
          (run-with-idle-timer 0 'motd-timer-interval #'motd--timeout-handler))))

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
    (setq motd--popup-frame nil)))

(define-minor-mode motd-message-mode
  "Minor mode to show motd message."
  :lighter ""
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "RET") 'motd--remove-popup)
            (define-key map (kbd "X") 'motd--remove-popup)
            (define-key map (kbd "q") (lambda () (interactive) (message "HI")))
            map))

(provide 'motd)
;;; motd.el ends here
