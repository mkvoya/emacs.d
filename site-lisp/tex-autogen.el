;;;

(defun tex-autogen ()
  "Compile the current LaTeX file using tex-compile."
  (when (and (derived-mode-p 'tex-mode)
             (buffer-file-name))
    (let ((saved-buffer (current-buffer))) ; we need to save the buffer as Make will change it
      (TeX-command "Make" 'TeX-master-file)
      (with-current-buffer saved-buffer
        (TeX-view)))))

;;;###autoload
(define-minor-mode tex-autogen-mode
  "Minor mode to automatically compile LaTeX files upon saving."
  :lighter " AutoCompile"
  :global nil
  (if tex-autogen-mode
      (add-hook 'after-save-hook 'tex-autogen nil t)
    (remove-hook 'after-save-hook 'tex-autogen t)))

(provide 'tex-autogen)
