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

(defcustom org-propdrawer-svg-line-height-scale 0.5
  "Height of the `svg-line' dash as a fraction of the text line height.
The SVG canvas height is this fraction of `default-line-height'.  Only
the `svg-line' indicator style uses this."
  :type 'number
  :group 'org-propdrawer)

(defcustom org-propdrawer-compact-indicator-line nil
  "When non-nil, shrink the indicator line to the height of its content.
This puts a `line-height' of t on the newline terminating the indicator
line, so the line is only as tall as the indicator instead of a full
text line.  The `svg-line' indicator style is always compacted,
regardless of this option."
  :type 'boolean
  :group 'org-propdrawer)

(defvar-local org-propdrawer--overlays nil
  "List of overlays covering property drawers in the current buffer.")

(defvar-local org-propdrawer--lh-markers nil
  "Markers at newlines carrying a `line-height' text property we set.
Tracked so the property can be removed precisely on refresh, even after
the buffer has been edited.")

(defvar-local org-propdrawer--idle-timer nil
  "Idle timer used to refresh hidden drawers after edits.")

(defface org-propdrawer-indicator-face
  '((t :inherit org-drawer :height 0.8))
  "Face for the indicator shown in place of a hidden property drawer."
  :group 'org-propdrawer)

(defun org-propdrawer--svg-line-px-height ()
  "Return the pixel height of the `svg-line' dash.
Scaled from the buffer's `default-line-height' by
`org-propdrawer-svg-line-height-scale'."
  (max 1 (round (* org-propdrawer-svg-line-height-scale
                   (default-line-height)))))

(defun org-propdrawer--svg-line ()
  "Return a display string showing a short, low-height SVG dash.
Falls back to the text \"---\" when SVG images are unavailable."
  (if (image-type-available-p 'svg)
      (let* ((w 28)
             (h (org-propdrawer--svg-line-px-height))
             (y (/ h 2.0))
             (color (or (face-foreground 'org-propdrawer-indicator-face nil t)
                        "gray"))
             (svg (svg-create w h)))
        (svg-line svg 3 y (- w 3) y :stroke-color color
                  :stroke-width (min 2 h))
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

(defun org-propdrawer--make-overlay (group)
  "Hide the drawer GROUP with a single overlay.
GROUP is a non-empty list of (BEG . END) conses for one or more
drawers sitting on consecutive lines (they belong to the same
heading); the whole region is collapsed together so their indicators
share one line.

END of the last drawer is the position right after `:END:', before its
trailing newline.  When an indicator is shown that newline is left
visible so the indicators occupy their own line yet still correspond to
a real buffer position (otherwise cursor motion would skip across the
drawers).  When there is no indicator the newline is hidden too, fully
collapsing the drawers."
  (let* ((beg (caar group))
         (end (cdar (last group)))
         (indicator (org-propdrawer--indicator-string))
         (before (and indicator
                      (apply #'concat (make-list (length group) indicator))))
         (oend (if before end (min (point-max) (1+ end))))
         (ov (make-overlay beg oend nil t nil)))
    (overlay-put ov 'org-propdrawer t)
    (overlay-put ov 'invisible 'org-propdrawer)
    (overlay-put ov 'evaporate t)
    (when before
      (overlay-put ov 'before-string before)
      ;; Tighten the indicator line by setting `line-height' on the
      ;; (visible) newline that terminates it.  `line-height' is honored
      ;; only as a text property (not as an overlay property), so set it
      ;; directly, silently, and remember the spot for cleanup.  For
      ;; `svg-line' an explicit pixel height (the dash height) is used so
      ;; the line can shrink below the font height; for other styles `t'
      ;; merely drops the extra leading.
      (let ((lh (cond ((eq org-propdrawer-indicator 'svg-line)
                       (org-propdrawer--svg-line-px-height))
                      (org-propdrawer-compact-indicator-line t))))
        (when (and lh (< end (point-max)))
          (with-silent-modifications
            (put-text-property end (1+ end) 'line-height lh))
          (push (copy-marker end) org-propdrawer--lh-markers))))
    (push ov org-propdrawer--overlays)
    ov))

(defun org-propdrawer--clear ()
  "Remove all property-drawer overlays and indicator text properties."
  (mapc #'delete-overlay org-propdrawer--overlays)
  (setq org-propdrawer--overlays nil)
  (when org-propdrawer--lh-markers
    (with-silent-modifications
      (dolist (m org-propdrawer--lh-markers)
        (let ((pos (marker-position m)))
          (when (and pos (< pos (point-max)))
            (remove-text-properties pos (1+ pos) '(line-height nil)))
          (set-marker m nil))))
    (setq org-propdrawer--lh-markers nil)))

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

(defun org-propdrawer--group-bounds (bounds)
  "Group BOUNDS that sit on consecutive lines into sublists.
BOUNDS is the ascending list returned by `org-propdrawer--drawer-bounds'.
Two drawers are grouped when the next one begins on the line
immediately following the previous drawer's `:END:'."
  (let ((groups nil) (cur nil) (prev-end nil))
    (dolist (b bounds)
      (if (and prev-end (= (car b) (1+ prev-end)))
          (setq cur (cons b cur))
        (when cur (push (nreverse cur) groups))
        (setq cur (list b)))
      (setq prev-end (cdr b)))
    (when cur (push (nreverse cur) groups))
    (nreverse groups)))

(defun org-propdrawer-refresh (&rest _)
  "Recompute and apply the property-drawer overlays in this buffer."
  (when (derived-mode-p 'org-mode)
    (org-propdrawer--clear)
    (dolist (group (org-propdrawer--group-bounds (org-propdrawer--drawer-bounds)))
      (let ((beg (caar group))
            (end (cdar (last group))))
        (unless (org-propdrawer--point-in-p beg end)
          (org-propdrawer--make-overlay group))))))

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
