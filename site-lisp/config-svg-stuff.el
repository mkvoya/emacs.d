;;; svg-stuff.el --- SVG related packages -*- lexical-binding: t -*-

;;; Commentary:
;;; This file contains svg related packages for configuration.
;;;
;;; Add svg checkbox that can be clicked.
;;;
;;; An example:
;;;
;;;     [X] item1
;;;     [ ] item2
;;;       [X] item 3
;;;     [X] item3
;;;
;;; Code:

(use-package svg-lib :demand t)


(use-package svg-tag-mode
  :disabled t
  :commands svg-tag-mode
  :hook (org-mode . svg-tag-mode)
  :demand t
  :after (svg-lib org)
  :straight (:host github :repo "rougier/svg-tag-mode" :files ("svg-tag-mode.el"))
  :config

  (defun mk/svg-prop-drawer(props)
    (let* ((svg (svg-create 14 14)))
      (svg-rectangle svg 0 0 14 14 :fill 'white :rx 2 :stroke-width 2.5 :stroke-color 'black)
      (svg-image svg :ascent 'center)
      ))

  (defun mk/svg-checkbox-empty()
    (let* ((svg (svg-create 14 14)))
      (svg-rectangle svg 0 0 14 14 :fill 'white :rx 2 :stroke-width 2.5 :stroke-color 'black)
      (svg-image svg :ascent 'center)
      ))

  (defun mk/svg-checkbox-filled()
    (let* ((svg (svg-create 14 14)))
      (svg-rectangle svg 0 0 14 14 :fill "#FFFFFF" :rx 2)
      (svg-polygon svg '((5.5 . 11) (12 . 3.5) (11 . 2) (5.5 . 9) (1.5 . 5) (1 . 6.5))
                   :stroke-color 'black :stroke-width 1 :fill 'black)
      (svg-image svg :ascent 'center)
      ))
  ;; (insert-image (svg-checkbox-empty))
  ;; (insert-image (svg-checkbox-filled))
  (defun mk/svg-checkbox-toggle()
    (interactive)
    (save-excursion
      (let* ((start-pos (line-beginning-position))
             (end-pos (line-end-position))
             (text (buffer-substring-no-properties start-pos end-pos))
             (case-fold-search t)  ; Let X and x be the same in search
             )
        (beginning-of-line)
        (cond ((string-match-p "\\[X\\]" text)
               (progn
                 (re-search-forward "\\[X\\]" end-pos)
                 (replace-match "[ ]")))
              ((string-match-p "\\[ \\]" text)
               (progn
                 (search-forward "[ ]" end-pos)
                 (replace-match "[X]")))
              ))))

  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")
  (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

  (setq svg-tag-tags
        `(
          ;; Properties
          ;; ("^:PROPERTIES:\n\\(:ID:.*\n\\)*?:END:$" . ((lambda (tag)
          ;;                                               (string-match ":ID:[[:space:]]*\\(.*\\)$" tag)
          ;;                                               (svg-tag-make (concat "⚙ ID: " (match-string 1 tag))))))
          ;; Org tags
          ;; (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
          ;; (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))

          ;; Task priority
          ("\\[#[A-Z]\\]" . ( (lambda (tag)
                                (svg-tag-make tag :face 'org-priority
                                              :beg 2 :end -1 :margin 0))))

          ;; TODO / DONE
          ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0))))
          ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))

          ;; Active date (with or without day name, with or without time)
          (,(format "\\(<%s>\\)" date-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :end -1 :margin 0))))
          (,(format "\\(<%s \\)%s>" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
          (,(format "<%s \\(%s>\\)" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

          ;; Inactive date  (with or without day name, with or without time)
          (,(format "\\(\\[%s\\]\\)" date-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
          (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
          (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))

          ;; TODO-list
          ("\\[ \\]" . ((lambda (_tag) (mk/svg-checkbox-empty))
                        (lambda () (interactive) (mk/svg-checkbox-toggle))
                        "Click to toggle."
                        ))
          ("\\(\\[[Xx]\\]\\)" . ((lambda (_tag) (mk/svg-checkbox-filled))
                                 (lambda () (interactive) (mk/svg-checkbox-toggle))
                                 "Click to toggle."))
          ))
  )
