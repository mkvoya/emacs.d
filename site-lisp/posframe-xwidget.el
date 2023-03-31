;;; posframe-xwidget.el --- Show webpage in posframe  -*- lexical-binding: t -*-
;;; Commentary:
;; This package provides floating web pages using posframe and xwidget.
;;; Code:

(require 'posframe)

(defvar posframe-xwidget-buffer-name " *posframe-xwidget*"
  "The buffer name used to show xwidget in posframe.")
(defvar posframe-xwidget-buffer nil
  "The buffer used to show xwidget.")

(defun posframe-xwidget-init ()
  (setq posframe-xwidget-buffer
        (posframe-show posframe-xwidget-buffer-name))
  )

(defun posframe-xwidget-update (html)
  (unless posframe-xwidget-buffer
    (posframe-xwidget-init)
    )
  (setq posframe)


  )

(provide 'posframe-xwidget)
;;; posframe-xwidget.el ends here
