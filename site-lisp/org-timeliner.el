
(defvar timeliner--start-hour 4 "The hour of the day to start the timeline.")

(defun timeliner--start-time ()
  (let* ((now (current-time))
         (decoded-time (decode-time now))
         (year (nth 5 decoded-time))
         (month (nth 4 decoded-time))
         (day (nth 3 decoded-time))
         (point-of-today (encode-time 0 0 timeliner--start-hour day month year)))
    (if (time-less-p now point-of-today)
        (time-subtract point-of-today (seconds-to-time 86400))
      point-of-today)))

(defun testfunc ()
    (org-ql-query
      :from (org-agenda-files)
      :where '(clocked :from -2 :to 0)
      :select (lambda ()
                (let* ((element (org-element-at-point))
                       (title (org-element-property :title element))
                       (begin (org-element-property :begin element))
                       (end (org-element-property :end element)))
                  (prin1 title))))
  )

(testfunc)

(defun timeliner--get-clock-entries ()
  (let* ((timeline-start (timeliner--start-time))
         (clock-entries nil))
    ;; (princ (format-time-string "%Y-%m-%d %H:%M:%S" timeline-start))
    (org-ql-query
      :from (org-agenda-files)
      :where '(clocked :from -2 :to 0)
      :select (lambda ()
                (let* ((element (org-element-at-point))
                       (title (org-element-property :title element))
                       (begin (org-element-property :begin element))
                       (end (org-element-property :end element)))
                  (prin1 title)
                  (save-excursion
                    (goto-char begin)
                    (while (re-search-forward org-clock-line-re end t)
                      (let* ((clock-element (org-element-at-point))
                             (clock-duration (org-element-property :duration clock-element))
                             (clock-status (org-element-property :status clock-element))
                             (clock-timerange (org-element-property :value clock-element))
                             (clock-start (org-timestamp-to-time (org-timestamp-split-range clock-timerange)))
                             (clock-end (org-timestamp-to-time (org-timestamp-split-range clock-timerange t)))
                             )
                        (when (or (time-less-p timeline-start clock-start)
                                  (time-less-p timeline-start clock-end))
                          (push (cons clock-start clock-end) clock-entries))))))))
    (prin1 clock-entries)
    clock-entries))
(timeliner--get-clock-entries)

(defun timeliner--generate-svg-image (clock-entries)
  "Generate an SVG image showing the CLOCK-ENTRIES events."
  (let* ((svg (svg-create 400 5))
         (timeline-start (float-time (timeliner--start-time))))
    (svg-rectangle svg 0 0 400 5
                   :fill "gray"
                   :fill-opacity 0.5)
    (dolist (entry clock-entries)
      (princ (format-time-string "%Y-%m-%d %H:%M:%S" (car entry)))
      (let* ((start-time (car entry))
             (end-time (cdr entry))
             (start-x (- (float-time start-time) timeline-start))
             (end-x (- (float-time end-time) timeline-start)))
        (if (minusp start-x)
            (setq start-x 0)
          (setq start-x (/ (* 400 start-x) 86400)))
        (setq end-x (/ (* 400 end-x) 86400))
        (svg-rectangle svg start-x 10 (- end-x start-x) 5
                       :fill "blue"
                       :fill-opacity 0.5)))
    (svg-image svg)))

(defvar timeliner-result-string "" "The string to display in the header line.")

(defun timeliner-update-string ()
  (setq timeliner-result-string (propertize " "
                                'display (timeliner--generate-svg-image (timeliner--get-clock-entries))
                                'help-echo "This is a timeline")))
(defun timeliner-get-string ()
  head-timeline-string)

(setq-default
   header-line-format
   '("GC: " (:eval (number-to-string gcs-done))
     " - " (:eval (number-to-string gc-elapsed))
     "s" " " (:eval (headliner-get-string))))
