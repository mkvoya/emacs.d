;; -*- lexical-binding: t; -*-
(require 'org)
(require 'svg)

(defvar timeliner-start-hour 4 "The hour of the day to start the timeline.")

(defun timeliner--start-time ()
  (let* ((now (current-time))
         (decoded-time (decode-time now))
         (year (nth 5 decoded-time))
         (month (nth 4 decoded-time))
         (day (nth 3 decoded-time))
         (point-of-today (encode-time 0 0 timeliner-start-hour day month year)))
    (if (time-less-p now point-of-today)
        (time-subtract point-of-today (seconds-to-time 86400))
      point-of-today)))

(defun timeliner--get-clock-entries ()
  (let* ((timeline-start (timeliner--start-time)))
    ;; (princ (format-time-string "%Y-%m-%d %H:%M:%S" timeline-start))
    (apply 'append
           (org-ql-query
             :from (org-agenda-files)
             :where '(clocked :from -2 :to 0)
             :select (lambda ()  ;; the select function should not have side-effect due to the org-ql's caching
                       (let* ((element (org-element-at-point))
                              (title (org-element-property :title element))
                              (begin (org-element-property :begin element))
                              (end (org-element-property :end element))
                              (collector nil))
                         ;; (prin1 title)
                         (save-excursion
                           (goto-char begin)
                           (while (re-search-forward org-clock-line-re end t)
                             (let* ((clock-element (org-element-at-point))
                                    (clock-duration (org-element-property :duration clock-element))
                                    (clock-status (org-element-property :status clock-element))
                                    (clock-timerange (org-element-property :value clock-element))
                                    (clock-start (org-timestamp-to-time (org-timestamp-split-range clock-timerange)))
                                    (clock-end (org-timestamp-to-time (org-timestamp-split-range clock-timerange t))))
                               (when (or (time-less-p timeline-start clock-start)
                                         (time-less-p timeline-start clock-end))
                                 (push (cons clock-start clock-end) collector)))))
                         collector))))))

(defvar timeliner-svg-height 5 "The height of the timeline SVG.")

(defun timeliner--add-line-to-svg (svg entry timeline-start color &optional opacity new-y new-height line-width)
  (let* ((start-time (car entry))
         (end-time (cdr entry))
         (start-x (- (float-time start-time) timeline-start))
         (end-x (- (float-time end-time) timeline-start)))
    (if (minusp start-x)
        (setq start-x 0)
      (setq start-x (/ (* 400 start-x) 86400)))
    (setq end-x (/ (* 400 end-x) 86400))
    (svg-rectangle svg
                   start-x (or new-y 0)
                   (- end-x start-x) (or new-height timeliner-svg-height)
                   :fill color
                   :fill-opacity (or opacity 0.5)
                   :stroke "black"
                   :stroke-width (or line-width 0)
                   )))

(defun timeliner--generate-svg-image (clock-entries)
  "Generate an SVG image showing the CLOCK-ENTRIES events."
  (let* ((svg (svg-create 400 timeliner-svg-height))
         (timeline-start (float-time (timeliner--start-time))))
    (svg-rectangle svg 0 0 400 timeliner-svg-height
                   :fill "#dedede"
                   :fill-opacity 1)
    (dolist (entry clock-entries)
      ;; (princ (format-time-string "%Y-%m-%d %H:%M:%S" (car entry)))
      (timeliner--add-line-to-svg svg entry timeline-start "blue" 1 nil nil 1))
    (when (org-clocking-p)
      (timeliner--add-line-to-svg svg (cons org-clock-start-time (current-time)) timeline-start "#32cd32" 1))
    (timeliner--add-line-to-svg svg (cons timeline-start (current-time)) timeline-start "#ff0000" 1 (* timeliner-svg-height 0.66) (/ timeliner-svg-height 2))
    (svg-image svg :ascent 'center)))

(defvar timeliner-result-string "" "The string to display in the header line.")

(defun timeliner-update-string ()
  (setq timeliner-result-string (propertize " "
                                'display (timeliner--generate-svg-image (timeliner--get-clock-entries))
                                'help-echo "This is a timeline")))
(defun timeliner-get-string ()
  timeliner-result-string)

(defvar timeliner-timer nil "The main timer for the timeline update.")

;; Set up the timer to run 'my-timer-function every 10 seconds
(setq timeliner-timer (run-at-time 0 10 'timeliner-update-string))

