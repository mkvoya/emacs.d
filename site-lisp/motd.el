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

(defvar motd--today nil)
(defvar motd--posframe-buffer " *motd-message*")

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

(defun motd--popup-posframe (text)
  "Show TEXT in the posframe popup."
  (with-current-buffer (get-buffer-create motd--posframe-buffer)
    (read-only-mode -1)
    (erase-buffer)
    (motd-message-mode t)
    (insert text)
    (insert "\n")
    (insert "         [ OK ]      \n")
    (insert "\n")
    (insert "\n")
    (read-only-mode t))
  (posframe-show motd--posframe-buffer
                 :position '(1, 1)
                 :background-color "lightgreen"
                 :border-color "grey"
                 ;; :initialize #'motd--posframe-setup-click-callback
                 :accept-focus t
                 ;; :timeout 2
                 :border-width 5
                 :internal-border-width 10
                 :poshandler 'posframe-poshandler-frame-center))

(defun motd--timeout-handler ()
  "Remind you important things upon the first touch of Emacs in the day."
  (let ((day (format-time-string "%d")))
    (unless (or (equal day motd--today) (not (frame-focus-state)))
      (let ((text (motd--report-content)))
        (if (posframe-workable-p)
            (motd--popup-posframe text)
          (motd--popup-ns text)))
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

(defun motd--remove-posframe ()
  "Delete the posframe popup."
  (interactive)
  (posframe-delete motd--posframe-buffer)
  )

(define-minor-mode motd-message-mode
  "Minor mode to show motd message."
  :lighter ""
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "RET") 'motd--remove-posframe)
            (define-key map (kbd "X") 'motd--remove-posframe)
            (define-key map (kbd "q") '(lambda () (interactive) (message "HI")))
            map))

(provide 'motd)
;;; motd.el ends here
