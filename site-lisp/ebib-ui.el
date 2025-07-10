;;; ebib-ui.el --- A TUI for ebib -*- lexical-binding: t; -*-

;;; Commentary:
;; This elisp file provides a TUI for ebib, the bib manager in Emacs.
;; The layout is a three-pane view similar to Zotero.

;;; Code:

(require 'ebib)
(require 'tabulated-list)

;;;
;;; UI constants and state
;;;

(defvar ebib-ui--collections-buffer-name "*ebib-ui-collections*"
  "Buffer name for collections view.")
(defvar ebib-ui--entries-buffer-name "*ebib-ui-entries*"
  "Buffer name for entries view.")
(defvar ebib-ui--details-buffer-name "*ebib-ui-entry-details*"
  "Buffer name for entry details view.")

(defvar ebib-ui--collections-window nil
  "Window object for collections view.")
(defvar ebib-ui--entries-window nil
  "Window object for entries view.")
(defvar ebib-ui--details-window nil
  "Window object for entry details view.")


;;;
;;; Collections List (Left Pane)
;;;

(defun ebib-ui--get-all-keywords ()
  "Get a list of all keywords from the ebib database."
  (let ((keywords (make-hash-table :test 'equal)))
    (dolist (entry (ebib-db-as-list))
      (let ((kw-string (ebib-ui--get-field entry 'keywords)))
        (when (and kw-string (> (length kw-string) 0))
          (dolist (kw (split-string kw-string "[ \t]*,[ \t]*"))
            (puthash kw t keywords)))))
    (let (result)
      (maphash (lambda (k _v) (push k result)) keywords)
      (sort result #'string<))))

(defun ebib-ui--select-keyword (keyword)
  "Filter entries by KEYWORD."
  (ebib-ui--populate-entries keyword)
  (with-current-buffer (get-buffer ebib-ui--entries-buffer-name)
    (tabulated-list-print)))

(defun ebib-ui--populate-collections ()
  "Populate the collections view with keywords from ebib."
  (with-current-buffer (get-buffer-create ebib-ui--collections-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      ;; Add an "All Entries" button
      (insert-button "All Entries"
                     'action (lambda (_) (ebib-ui--populate-entries)
                                     (with-current-buffer (get-buffer ebib-ui--entries-buffer-name)
                                       (tabulated-list-print)))
                     'face 'link)
      (insert "\n")
      ;; Add keyword buttons
      (dolist (kw (ebib-ui--get-all-keywords))
        (insert-button kw
                       'action (lambda (button)
                                 (ebib-ui--select-keyword (button-label button)))
                       'face 'link)
        (insert "\n")))))

;;;
;;; Window Layout Setup
;;;

(defun ebib-ui--setup-windows ()
  "Set up the three-pane window layout for ebib-ui."
  (let* ((collections-win (selected-window))
         (entries-win (split-window-right (round (* 0.75 (frame-width)))))
         (details-win (progn (select-window entries-win)
                             (split-window-below (round (* 0.6 (frame-height)))))))

    (setq ebib-ui--collections-window collections-win
          ebib-ui--entries-window entries-win
          ebib-ui--details-window details-win)

    (set-window-buffer collections-win (get-buffer-create ebib-ui--collections-buffer-name))
    (set-window-buffer entries-win (get-buffer-create ebib-ui--entries-buffer-name))
    (set-window-buffer details-win (get-buffer-create ebib-ui--details-buffer-name))

    (with-current-buffer (get-buffer ebib-ui--entries-buffer-name)
      (ebib-ui-entries-mode))

    (ebib-ui--populate-collections)
    (ebib-ui--populate-entries)
    (ebib-read-bib-files)))

;;;
;;; Main Entry Point
;;;

(defun ebib-ui ()
  "Open the ebib TUI."
  (interactive)
  (let ((buffer (get-buffer-create "*ebib-ui*")))
    (switch-to-buffer buffer)
    (kill-all-local-variables))
  (ebib-ui--setup-windows))


(provide 'ebib-ui)

;;; ebib-ui.el ends here
