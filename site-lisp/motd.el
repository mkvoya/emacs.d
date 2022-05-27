;;; motd.el --- Message of the day in Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defgroup motd nil "Message of the Day." :group 'emacs)

(defcustom motd-timer-interval 60
  "The time interval to detect the new day."
  :type 'number
  :group 'motd)

(defvar motd--today nil)

(defun motd--report-content ()
  "Generate the report content of the motd box."
  "Emacs Daily Report
    Recent Submission Deadlines:\n")

(defun motd--timeout-handler ()
  "Remind you important things upon the first touch of Emacs in the day."
  (let ((day (format-time-string "%d")))
    (unless (or (equal day motd--today) (not (frame-focus-state)))
      (x-popup-dialog t `(,(motd--report-content)
                          ("Love You 😘 .x..." . t)))
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

(provide 'motd)
;;; motd.el ends here
