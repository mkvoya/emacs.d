(defvar super-prop-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-p r") 'super-prop-render-banner)
    (define-key map (kbd "C-c C-p d") 'super-prop-remove-banner)
    map)
  "Keymap for super-prop-mode.")

(defun super-prop-render-banner ()
  "Replace property drawer with a SVG banner showing ID and CREATED properties."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (forward-line)
    (when (looking-at org-property-drawer-re)
      (let* ((drawer-start (point))
             (drawer-end (save-excursion
                          (re-search-forward ":END:" nil t)
                          (line-end-position)))
             (id-prop (org-entry-get nil "ID"))
             (created-prop (org-entry-get nil "CREATED"))
             (banner-svg
              (svg-create 400 30
                         :stroke "gray"
                         :stroke-width 1))
             (overlay (make-overlay drawer-start drawer-end)))

        ;; Add background rectangle
        (svg-rectangle banner-svg 0 0 400 20
                      :fill "#f0f0f0"
                      :rx 5)

        ;; Add text for ID and CREATED
        (svg-text banner-svg
                  (format "ID: %s" (or id-prop "N/A"))
                  :x 10 :y 10
                  :font-family "Helvetica"
                  :font-size 10
                  :fill "black")
        
        (when created-prop
          (svg-text banner-svg
                    (format "Created: %s" created-prop)
                    :x 40 :y 10
                    :font-family "Helvetica"
                    :font-size 12
                    :fill "black"))
        
        ;; Set overlay properties
        (overlay-put overlay 'display
                    (svg-image banner-svg))
        (overlay-put overlay 'super-prop t)))))

(defun super-prop-remove-banner ()
  "Remove the SVG banner overlay from the property drawer."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (forward-line)
    (when (looking-at org-property-drawer-re)
      (let ((overlays (overlays-in (point)
                                  (save-excursion
                                    (re-search-forward ":END:" nil t)
                                    (line-end-position)))))
        (dolist (ov overlays)
          (when (overlay-get ov 'super-prop)
            (delete-overlay ov)))))))

(defun super-prop-render-all-banners ()
  "Render SVG banners for all property drawers in the buffer."
  (org-map-entries #'super-prop-render-banner))

(defun super-prop-remove-all-banners ()
  "Remove all SVG banner overlays from the buffer."
  (org-map-entries #'super-prop-remove-banner))

;;;###autoload
(define-minor-mode super-prop-mode
  "Minor mode for displaying property drawers as SVG banners."
  :lighter " SuperProp"
  :keymap super-prop-mode-map
  (if super-prop-mode
      (super-prop-render-all-banners)
    (super-prop-remove-all-banners)))

(provide 'super-prop-mode)

