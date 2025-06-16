(defun swiftbar--get-agenda-swiftbar-items ()
  "Get all headings with swiftbar tag in all agenda files."
  (let ((items nil)
        (seen-headings (make-hash-table :test 'equal)))
    (dolist (file (org-agenda-files))
      (with-current-buffer (find-file-noselect file)
        (org-map-entries
         (lambda ()
           (when (member "swiftbar" (org-get-tags))
             (let ((heading (org-get-heading t t t t)))
               (unless (gethash heading seen-headings)
                 (puthash heading t seen-headings)
                 (push (list :heading heading
                            :level (org-current-level)
                            :pos (point)
                            :file file)
                       items)))))
         nil
         'agenda)))
    items))

(defun swiftbar-list-agenda-swiftbar-items ()
  "Return all headings with swiftbar tag from agenda files as a string."
  (interactive)
  (let ((items (swiftbar--get-agenda-swiftbar-items)))
    (mapconcat (lambda (item) 
                 (substring-no-properties (plist-get item :heading)))
               items
               ",")))

(defun swiftbar-clock-in (heading)
  "Clock in to the heading with the given HEADING."
  (interactive "sHeading: ")
  (let ((items (swiftbar--get-agenda-swiftbar-items))
        (found nil))
    (dolist (item items)
      (when (string= (plist-get item :heading) heading)
        (setq found item)
        (find-file (plist-get item :file))
        (goto-char (plist-get item :pos))
        (org-clock-in)
        (message "Clocked in to: %s" heading)))
    (unless found
      (message "No heading found with title: %s" heading))))
