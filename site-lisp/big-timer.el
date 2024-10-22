;;; big-timer.el --- A big timer display for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Mingkai Dong

;; Author: Mingkai Dong <mk@dong.mk>
;; Version: 0.1
;; Package-Requires: ((emacs "24.4") (svg-lib "0.2"))
;; Keywords: timer, tools
;; URL: n/a

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a big timer display for Emacs.
;; It creates a separate frame to show the elapsed or remaining time in large digits.
;; The timer supports two modes: countdown and countup.
;; It also includes buttons for resetting and pausing/resuming the timer.

;;; Code:

(require 'svg-lib)

(defvar big-timer-timer nil
  "Timer for updating the big timer.")

(defvar big-timer-frame nil
  "Frame for displaying the big timer.")

(defvar big-timer-start-time nil
  "Start time of the timer.")

(defvar big-timer-mode 'countup
  "Current mode of the timer. Can be 'countup or 'countdown.")

(defvar big-timer-duration nil
  "Duration for countdown mode in seconds.")

(defvar big-timer-paused nil
  "Flag indicating whether the timer is paused.")

(defvar big-timer-pause-time nil
  "Time when the timer was paused.")

(defun big-timer-start ()
  "Start the big timer."
  (unless big-timer-frame
    (setq big-timer-frame (make-frame '((name . "Big Timer")
                                        (minibuffer . nil)
                                        (left-fringe . 0)
                                        (right-fringe . 0)
                                        (right-margin . 0)
                                        (left-margin . 0)
                                        (internal-border-width . 20)))))
  (with-selected-frame big-timer-frame
    (switch-to-buffer (get-buffer-create "*Big Timer*"))
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (propertize "00:00\n\n" 'face '(:height 1.5)))
    ;; (insert-image (svg-lib-icon "refresh" '(:scale 0.8)))
    (insert-text-button " "
                        'display (svg-lib-icon "refresh" '(:scale 0.8))
                        'action (lambda (_) (big-timer-reset))
                        'follow-link t)
    (insert " ")
    ;; (insert-image (svg-lib-icon "play" '(:scale 0.8)))
    (insert-text-button " "
                        'display (svg-lib-icon "play" '(:scale 0.8))
                        'action (lambda (_) (big-timer-toggle-pause))
                        'follow-link t)
    (text-scale-set 8)
    (setq-local cursor-type nil)
    (setq-local mode-line-format nil)
    (setq-local header-line-format nil)
    (hl-line-mode -1)
    (set-frame-position big-timer-frame 100 100)
    (let* ((char-width (window-font-width))
           (char-height (window-font-height))
           (button-width 16)  ; Adjusted width for scaled buttons
           (button-height 16)  ; Adjusted height for scaled buttons
           (frame-width (+ (max (* char-width 5) (* button-width 2)) 120))  ; 5 chars for time, 2 buttons, extra padding
           (frame-height (+ (* char-height 2) button-height 20)))  ; 2 lines of text, buttons, extra padding
      (set-frame-size big-timer-frame frame-width frame-height t))
    (setq-local cursor-type nil)
    (setq buffer-read-only t)
    (local-set-key (kbd "q") 'global-big-timer-mode))
  (setq big-timer-start-time (current-time))
  (setq big-timer-paused nil)
  (setq big-timer-pause-time nil)
  (setq big-timer-timer (run-with-timer 0 1 'update-big-timer)))

(defun big-timer-stop ()
  "Stop the big timer."
  (when big-timer-timer
    (cancel-timer big-timer-timer)
    (setq big-timer-timer nil))
  (when (and big-timer-frame (frame-live-p big-timer-frame))
    (delete-frame big-timer-frame)
    (setq big-timer-frame nil))
  (setq big-timer-start-time nil)
  (setq big-timer-duration nil)
  (setq big-timer-mode 'countup)
  (setq big-timer-paused nil)
  (setq big-timer-pause-time nil))

(defun update-big-timer ()
  "Update the big timer display."
  (when (and big-timer-frame (frame-live-p big-timer-frame))
    (with-selected-frame big-timer-frame
      (with-current-buffer (get-buffer "*Big Timer*")
        (let* ((elapsed-time (if big-timer-paused
                                 (time-subtract big-timer-pause-time big-timer-start-time)
                               (time-subtract (current-time) big-timer-start-time)))
               (seconds (floor (time-to-seconds elapsed-time)))
               (display-seconds (if (eq big-timer-mode 'countdown)
                                    (max 0 (- big-timer-duration seconds))
                                  seconds))
               (minutes (/ display-seconds 60))
               (remaining-seconds (mod display-seconds 60)))
          (setq buffer-read-only nil)

          (erase-buffer)
          (insert (propertize (format "%02d:%02d\n" minutes remaining-seconds) 'face '(:height 1.5)))
          (insert (propertize " " 'face '(:height 1)))
          ;; (insert-image (svg-lib-icon "refresh" '(:scale 0.8)))
          (insert-text-button " "
                              'display (svg-lib-icon "refresh" '(:scale 0.8))
                              'action (lambda (_) (big-timer-reset))
                              'follow-link t)
          (insert " ")
          ;; (insert-image (svg-lib-icon (if big-timer-paused "play" "pause") '(:scale 0.8)))
          (insert-text-button " "
                              'display (svg-lib-icon (if big-timer-paused "play" "pause") '(:scale 0.8))
                              'action (lambda (_) (big-timer-toggle-pause))
                              'follow-link t)
          (setq buffer-read-only t))))))

(defun big-timer-set-mode (mode &optional duration)
  "Set the timer mode to MODE and optionally set DURATION for countdown.
MODE can be 'countup or 'countdown. DURATION is in seconds."
  (setq big-timer-mode mode)
  (when (eq mode 'countdown)
    (setq big-timer-duration (or duration 0)))
  (setq big-timer-start-time (current-time))
  (setq big-timer-paused nil)
  (setq big-timer-pause-time nil)
  (when big-timer-timer
    (update-big-timer)))

(defun big-timer-start-countdown (duration)
  "Start a countdown timer with the specified DURATION in minutes."
  (interactive "nEnter countdown duration in minutes: ")
  (let ((duration-seconds (* duration 60)))
    (big-timer-set-mode 'countdown duration-seconds)
    (unless global-big-timer-mode
      (global-big-timer-mode 1))))

(defun big-timer-reset (&optional duration)
  "Reset the timer and optionally set a new DURATION for countdown mode."
  (interactive "P")
  (when duration
    (setq big-timer-duration (if (numberp duration) duration 0)))
  (setq big-timer-start-time (current-time))
  (setq big-timer-paused nil)
  (setq big-timer-pause-time nil)
  (when big-timer-timer
    (update-big-timer)))

(defun big-timer-toggle-pause ()
  "Toggle pause/resume of the big timer."
  (interactive)
  (if big-timer-paused
      (progn
        (setq big-timer-start-time (time-add big-timer-start-time
                                             (time-subtract (current-time) big-timer-pause-time)))
        (setq big-timer-paused nil)
        (setq big-timer-pause-time nil))
    (setq big-timer-paused t)
    (setq big-timer-pause-time (current-time)))
  (update-big-timer))

(define-minor-mode global-big-timer-mode
  "Toggle Big Timer mode globally.
When enabled, displays a large digital timer in a separate frame."
  :global t
  :init-value nil
  :lighter " BigTimer"
  (if global-big-timer-mode
      (progn
        (setq big-timer-mode 'countup)
        (big-timer-start))
    (big-timer-stop)))

(provide 'big-timer)

;;; big-timer.el ends here
