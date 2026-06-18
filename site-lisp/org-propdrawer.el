;; -*- lexical-binding: t; -*-
;; Hide property drawers in org mode

;;; Commentary:

;; `org-propdrawer-mode' is a buffer-local minor mode that hides the
;; `:PROPERTIES:' ... `:END:' drawers attached to headings (and the
;; top-level file drawer) by covering them with invisible overlays.
;; `:LOGBOOK:' drawers are hidden as well by default.
;;
;; A small indicator is shown in place of each hidden drawer so it is
;; clear that something is folded away.  Moving point onto the heading
;; reveals its drawer; leaving the heading hides it again, so editing a
;; property is still convenient.

;;; Code:

(require 'org)
(require 'org-element)
(require 'svg)

(defgroup org-propdrawer nil
  "Hide property drawers in org mode."
  :group 'org)

(defcustom org-propdrawer-indicator 'ellipsis
  "Indicator shown in place of a hidden drawer.
It may be one of the predefined symbols, a literal string, or nil:

  `ellipsis'  the text \" [...]\";
  `svg-line'  a short, low-height SVG dash (---);
  a string    shown verbatim;
  nil         no indicator at all."
  :type '(choice (const :tag "Ellipsis  [...]" ellipsis)
                 (const :tag "Short SVG dash  ---" svg-line)
                 (const :tag "No indicator" nil)
                 (string :tag "Custom string"))
  :group 'org-propdrawer)

(defcustom org-propdrawer-reveal-at-point t
  "When non-nil, reveal the drawer of the heading at point."
  :type 'boolean
  :group 'org-propdrawer)

(defcustom org-propdrawer-hide-properties t
  "When non-nil, hide `:PROPERTIES:' drawers."
  :type 'boolean
  :group 'org-propdrawer)

(defcustom org-propdrawer-hide-logbook t
  "When non-nil, hide `:LOGBOOK:' drawers."
  :type 'boolean
  :group 'org-propdrawer)

(defvar-local org-propdrawer--overlays nil
  "List of overlays covering property drawers in the current buffer.")

(defvar-local org-propdrawer--idle-timer nil
  "Idle timer used to refresh hidden drawers after edits.")

(defface org-propdrawer-indicator-face
  '((t :inherit org-drawer :height 0.8))
  "Face for the indicator shown in place of a hidden property drawer."
  :group 'org-propdrawer)

(defun org-propdrawer--svg-line ()
  "Return a display string showing a short, low-height SVG dash.
Falls back to the text \"---\" when SVG images are unavailable."
  (if (image-type-available-p 'svg)
      (let* ((w 28) (h 10) (y (/ h 2))
             (color (or (face-foreground 'org-propdrawer-indicator-face nil t)
                        "gray"))
             (svg (svg-create w h)))
        (svg-line svg 3 y (- w 3) y :stroke-color color :stroke-width 2)
        (propertize " " 'display (svg-image svg :ascent 'center :scale 1)))
    (propertize " ---" 'face 'org-propdrawer-indicator-face)))

(defun org-propdrawer--indicator-string ()
  "Return the before-string to display for a hidden drawer, or nil."
  (pcase org-propdrawer-indicator
    ('nil nil)
    ('ellipsis (propertize " [...]" 'face 'org-propdrawer-indicator-face))
    ('svg-line (org-propdrawer--svg-line))
    ((pred stringp)
     (propertize org-propdrawer-indicator 'face 'org-propdrawer-indicator-face))
    (_ nil)))

(defun org-propdrawer--make-overlay (beg end)
  "Hide the drawer spanning BEG to END with an overlay.
END is the position right after `:END:', before its trailing newline.
When an indicator is shown that newline is left visible so the
indicator occupies its own line yet still corresponds to a real buffer
position (otherwise cursor motion would skip across the drawer).  When
there is no indicator the newline is hidden too, fully collapsing the
drawer."
  (let* ((indicator (org-propdrawer--indicator-string))
         (oend (if indicator end (min (point-max) (1+ end))))
         (ov (make-overlay beg oend nil t nil)))
    (overlay-put ov 'org-propdrawer t)
    (overlay-put ov 'invisible 'org-propdrawer)
    (overlay-put ov 'evaporate t)
    (when indicator
      (overlay-put ov 'before-string indicator))
    (push ov org-propdrawer--overlays)
    ov))

(defun org-propdrawer--clear ()
  "Remove all property-drawer overlays in the current buffer."
  (mapc #'delete-overlay org-propdrawer--overlays)
  (setq org-propdrawer--overlays nil))

(defun org-propdrawer--logbook-re ()
  "Return a regexp matching a `:LOGBOOK:' drawer.
Uses the drawer name configured via `org-log-into-drawer', falling
back to \"LOGBOOK\"."
  (let ((name (or (and (stringp org-log-into-drawer) org-log-into-drawer)
                  "LOGBOOK")))
    (concat "^[ \t]*:" (regexp-quote name) ":[ \t]*\n"
            "\\(?:.*\n\\)*?"
            "[ \t]*:END:[ \t]*$")))

(defun org-propdrawer--drawer-bounds ()
  "Return a list of (BEG . END) conses for every drawer to hide.
BEG is the beginning of the drawer's opening line and END is the
position right after `:END:' (before its trailing newline).
Which drawer types are collected depends on
`org-propdrawer-hide-properties' and `org-propdrawer-hide-logbook'."
  (let ((case-fold-search t)
        (regexps (delq nil
                       (list (and org-propdrawer-hide-properties
                                  org-property-drawer-re)
                             (and org-propdrawer-hide-logbook
                                  (org-propdrawer--logbook-re)))))
        (bounds nil))
    (save-excursion
      (dolist (re regexps)
        (goto-char (point-min))
        (while (re-search-forward re nil t)
          (let ((beg (match-beginning 0))
                (end (match-end 0)))
            (push (cons beg end) bounds)))))
    (sort bounds #'car-less-than-car)))

(defun org-propdrawer--point-in-p (beg end)
  "Return non-nil when point is inside or on the heading owning BEG..END.
This is the region that should stay revealed for convenient editing."
  (and org-propdrawer-reveal-at-point
       (save-excursion
         (let ((heading-beg
                (save-excursion
                  (goto-char beg)
                  (forward-line -1)
                  (line-beginning-position))))
           (and (>= (point) heading-beg) (<= (point) end))))))

(defun org-propdrawer-refresh (&rest _)
  "Recompute and apply the property-drawer overlays in this buffer."
  (when (derived-mode-p 'org-mode)
    (org-propdrawer--clear)
    (dolist (b (org-propdrawer--drawer-bounds))
      (unless (org-propdrawer--point-in-p (car b) (cdr b))
        (org-propdrawer--make-overlay (car b) (cdr b))))))

(defun org-propdrawer--schedule-refresh (&rest _)
  "Schedule a refresh on the next idle moment, coalescing rapid edits."
  (when org-propdrawer--idle-timer
    (cancel-timer org-propdrawer--idle-timer))
  (setq org-propdrawer--idle-timer
        (run-with-idle-timer
         0.2 nil
         (lambda (buf)
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (setq org-propdrawer--idle-timer nil)
               (org-propdrawer-refresh))))
         (current-buffer))))

;;;###autoload
(define-minor-mode org-propdrawer-mode
  "Toggle hiding of org property drawers in the current buffer."
  :lighter " PropHide"
  :group 'org-propdrawer
  (if org-propdrawer-mode
      (progn
        (add-to-invisibility-spec 'org-propdrawer)
        (add-hook 'post-command-hook #'org-propdrawer-refresh nil t)
        (add-hook 'after-change-functions #'org-propdrawer--schedule-refresh nil t)
        (org-propdrawer-refresh))
    (remove-hook 'post-command-hook #'org-propdrawer-refresh t)
    (remove-hook 'after-change-functions #'org-propdrawer--schedule-refresh t)
    (when org-propdrawer--idle-timer
      (cancel-timer org-propdrawer--idle-timer)
      (setq org-propdrawer--idle-timer nil))
    (org-propdrawer--clear)
    (remove-from-invisibility-spec 'org-propdrawer)))

(provide 'org-propdrawer)
;;; org-propdrawer.el ends here
