;;; motd.el --- Message of the day in Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'org-ql)
(require 'org)

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
(defvar motd--git-commit-dir nil)


(defvar motd--timer nil)
(defvar motd--fade-timer nil)

(defvar motd--buffer (get-buffer-create "*Motd Message*"))

(defun show-notification (buffer-name)
  "Show a notification in a floating transparent frame with dark background for 3 seconds."
  (let* ((notification-buffer buffer-name)
         (current-frame (selected-frame))
         (frame-width (frame-pixel-width current-frame))
         (frame-pos-x (- (+ 0 (frame-parameter current-frame 'left) frame-width)
                         (* (+ 40 4) (frame-char-width)))) ; Right align with 1 char margin
         (frame-pos-y (+ (frame-parameter current-frame 'top)
                         20)) ; Add small offset from top
         (frame-params `((name . "notification")
                         (width . 80)
                         ;; (height . 3)
                         (minibuffer . nil)
                         ;; (minibuffer . ,(minibuffer-window parent))
                         ;; (parent-frame . ,parent)
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
                         (right-fringe . 0)))  ; Added parameters to remove fringes
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
  (format "Auto Git Commit:\n
%s\n"
          (when motd--git-commit-dir
            (let ((result (shell-command-to-string
                           (format "cd %s && git add -A && git commit -m \"%s\""
                                   motd--git-commit-dir (format-time-string "%F")))))
              (message (format "[motd-git] %s is committed." motd--git-commit-dir))
              result))))

(defun motd--gitlab-commit-summary ()
  "Call gitlab monitor script and return its output."
  (make-process
   :name "gitlab-monitor"
   :command '("python3" "/Users/dongmk/.emacs.d/site-tools/GitlabMonitor/gitlab_monitor.py")
   :buffer "*gitlab-monitor*"
   :sentinel (lambda (process event)
               (when (string= event "finished\n")
                 (let ((output (with-current-buffer "*gitlab-monitor*"
                                 (buffer-string))))
                   (kill-buffer "*gitlab-monitor*")
                   (with-current-buffer motd--buffer
                     (goto-char (point-max))
                     (insert (format "%s\n" output)))
                   (setq motd--data-ready (+ motd--data-ready 1)))))))

(defun motd--ccfddl-summary ()
  "Call ccfddl script and return its output."
  (make-process
   :name "ccfddl"
   :command '("python3" "/Users/dongmk/.emacs.d/site-tools/CCFDDL/conference_deadlines.py")
   :buffer "*ccfddl*"
   :sentinel (lambda (process event)
               (when (string= event "finished\n")
                 (let ((output (with-current-buffer "*ccfddl*"
                                 (buffer-string))))
                   (kill-buffer "*ccfddl*")
                   (with-current-buffer motd--buffer
                     (goto-char (point-max))
                     (insert (format "%s\n" output)))
                   (setq motd--data-ready (+ motd--data-ready 1)))))))

(defun motd--report-content-to-buffer ()
  "Generate the report content of the motd box."
  (with-current-buffer motd--buffer
    (erase-buffer)
    (insert "<<Emacs Daily Report>>\n")
    (motd--git-commit))
  (motd--ccfddl-summary)
  (motd--gitlab-commit-summary))

(defvar motd--data-ready nil)
(defvar motd--data-ready-timer nil)

(defun motd--timeout-handler ()
  "Remind you important things upon the first touch of Emacs in the day."
  (let ((day (format-time-string "%d")))
    (unless (or (equal day motd--today) (not (frame-focus-state)))
      (setq motd--today day)
      (setq motd--data-ready 0)
      (make-thread (lambda () (motd--report-content-to-buffer)))
      (setq motd--data-ready-timer
            (run-with-timer 1 3
                            (lambda ()
                              (message (format "waiting %d/2" motd--data-ready))
                              (when (eq motd--data-ready 2)
                                (with-current-buffer motd--buffer
                                             (goto-char (point-min)))
                                (show-notification motd--buffer)
                                (cancel-timer motd--data-ready-timer)
                                (setq motd--data-ready-timer nil))))))))

(defun motd-start-timer ()
  "Start the timer for the motd daemon."
  (interactive)
  (if motd--timer
      (message "Timer is already up.")
    (setq motd--timer
          (run-with-idle-timer 0 'motd-timer-interval #'motd--timeout-handler)))
  )

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
