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
(defvar motd--git-commit-dir nil)


(defvar motd--timer nil)
(defvar motd--fade-timer nil)

(defun show-notification (message)
  "Show a notification in a floating transparent frame with dark background for 3 seconds."
  (let* ((notification-buffer (get-buffer-create "*notification*"))
         (current-frame (selected-frame))
         (frame-width (frame-pixel-width current-frame))
         (frame-pos-x (- (+ 0 (frame-parameter current-frame 'left) frame-width)
                         (* (+ 40 4) (frame-char-width)))) ; Right align with 1 char margin
         (frame-pos-y (+ (frame-parameter current-frame 'top)
                         20)) ; Add small offset from top
         (frame-params `((name . "notification")
                         (width . 40)
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
    (with-current-buffer notification-buffer
      (erase-buffer)
      (insert message))
    (set-window-buffer (frame-selected-window frame) notification-buffer)
    (set-window-parameter (frame-selected-window frame) 'mode-line-format 'none)
    (set-window-parameter (frame-selected-window frame) 'header-line-format 'none)
    (run-with-timer 30 nil
                    (lambda ()
                      (when (frame-live-p frame)
                        (delete-frame frame))
                      (kill-buffer notification-buffer)))))

;; (show-notification "HIHI")

(defun motd--git-commit ()
  "Generate git commit when motd message is shown."
  (when motd--git-commit-dir
    (shell-command (format "cd %s && git add -A && git commit -m \"%s\""
                           motd--git-commit-dir (format-time-string "%F")) "MOTD-Git Log")
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



(defun motd--timeout-handler ()
  "Remind you important things upon the first touch of Emacs in the day."
  (let ((day (format-time-string "%d")))
    (unless (or (equal day motd--today) (not (frame-focus-state)))
      (progn
        (let ((text (motd--report-content)))
          (show-notification text)
          )
        (motd--git-commit))
      (setq motd--today day))))

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
