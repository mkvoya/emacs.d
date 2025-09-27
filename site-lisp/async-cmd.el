;;; async-cmd.el --- Simple async command manager (direct file + callback) -*- lexical-binding: t -*-

(require 'cl-lib)

(defvar async-cmd--table (make-hash-table :test 'equal)
  "Hash table mapping id -> plist with keys:
:process (the process object)
:tempfile (path to temp file)
:finished (t/nil)
:exit-status (integer or nil)
:callback (function or nil)")

(defvar async-cmd--counter 0
  "Counter for generating unique ids.")

(defun async-cmd--gen-id ()
  "Generate a new unique identifier string."
  (format "async-cmd-%d-%d" (float-time) (cl-incf async-cmd--counter)))

(defun async-cmd-start (program args &optional callback)
  "Run PROGRAM with ARGS asynchronously.
Outputs are redirected to a temporary file.
Returns an identifier string.

If CALLBACK is non-nil, it will be called when the process finishes as
  (funcall CALLBACK id exit-status buffer),
where BUFFER is the buffer visiting the output file (if available)."
  (interactive
   (list (read-file-name "Program: ")
         (split-string (read-string "Args: ") " " t)))
  (let* ((id (async-cmd--gen-id))
         (tmp (make-temp-file "async-cmd-output-"))
         (proc
          (make-process
           :name (format "%s-proc" id)
           :command (cons program args)
           :file tmp
           :noquery t
           :sentinel
           (lambda (p event)
             ;; event = "finished\n" or "exited abnormally..."
             (let* ((info (gethash id async-cmd--table))
                    (exit (process-exit-status p)))
               (when info
                 (plist-put info :finished t)
                 (plist-put info :exit-status exit)
                 (puthash id info async-cmd--table)
                 (when-let ((cb (plist-get info :callback)))
                   (let ((buf (when (file-exists-p tmp)
                                (find-file-noselect tmp))))
                     (funcall cb id exit buf)))))))))
    (puthash id (list :process proc
                      :tempfile tmp
                      :finished nil
                      :exit-status nil
                      :callback callback)
             async-cmd--table)
    id))

(defun async-cmd-finished-p (id)
  "Return non-nil if process for ID finished."
  (let ((info (gethash id async-cmd--table)))
    (unless info (error "Unknown id: %s" id))
    (plist-get info :finished)))

(defun async-cmd-exit-status (id)
  "Return exit status for ID, or nil if not finished."
  (let ((info (gethash id async-cmd--table)))
    (unless info (error "Unknown id: %s" id))
    (plist-get info :exit-status)))

(defun async-cmd-output-buffer (id &optional buffer-name)
  "Return a buffer with contents of output file for ID.
If BUFFER-NAME is given, insert contents into that buffer.
Otherwise, create a new buffer named *async-cmd-ID*."
  (let* ((info (gethash id async-cmd--table))
         (tmp (plist-get info :tempfile)))
    (unless info (error "Unknown id: %s" id))
    (unless (file-exists-p tmp)
      (error "Temp file for %s not found" id))
    (let ((buf (if buffer-name
                   (get-buffer-create buffer-name)
                 (generate-new-buffer (format "*async-cmd-%s*" id)))))
      (with-current-buffer buf
        (erase-buffer)
        (insert-file-contents tmp))
      buf)))

(defun async-cmd-kill-and-cleanup (id)
  "Kill process and remove resources for ID."
  (let ((info (gethash id async-cmd--table)))
    (unless info (error "Unknown id: %s" id))
    (let ((proc (plist-get info :process))
          (tmp (plist-get info :tempfile)))
      (when (process-live-p proc)
        (ignore-errors (kill-process proc)))
      (when (and tmp (file-exists-p tmp))
        (ignore-errors (delete-file tmp)))
      (remhash id async-cmd--table)
      t)))

(provide 'async-cmd)

;;; async-cmd.el ends here
