(defun aw-ensure-activitywatch-running ()
  "Ensure ActivityWatch is running, start it if not."
  (interactive)
  (unless (get-process "ActivityWatch")
    (start-process "ActivityWatch" "*ActivityWatch*" "open" "-a" "ActivityWatch")
    (message "Started ActivityWatch")))


(defun aw-get-latest-window-data ()
  "Get the latest window data from ActivityWatch."
  (interactive)
  (let ((url "http://localhost:5600/api/0/buckets/aw-watcher-window_localhost/events?limit=1"))
    (with-current-buffer 
        (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "\n\n")
      (let ((json-object-type 'hash-table)
            (json-array-type 'list))
        (json-read)))))
