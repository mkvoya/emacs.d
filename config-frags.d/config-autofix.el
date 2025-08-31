;; -*- lexical-binding: t; -*-
;;; Emacs Config Fragement: Auto Correction & Fix


(use-package elnode
  :defer
  :ensure (:type git :host github :repo "jcaw/elnode"))

(use-package flymake
  :ensure nil  ; built-in
  :config
  (setq flymake-no-changes-timeout 2)
  )
(use-package jinx
  :disabled t
  :hook (emacs-startup . global-jinx-mode)
  :bind (("C-;" . jinx-correct)
         ("C-M-;" . jinx-languages))
  :config

  (add-to-list 'jinx-exclude-regexps '(t "\\cc"))
  (defun jinx--add-to-abbrev (overlay word)
    "Add abbreviation to `global-abbrev-table'.
          The misspelled word is taken from OVERLAY.  WORD is the corrected word."
    (let ((abbrev (buffer-substring-no-properties
                   (overlay-start overlay)
                   (overlay-end overlay))))
      (message "Abbrev: %s -> %s" abbrev word)
      (define-abbrev global-abbrev-table abbrev word)))

  (advice-add 'jinx--correct-replace :before #'jinx--add-to-abbrev)

  (defun my/jinx-ispell-localwords ()
    "Return a string of ispell's local words.

      Those are the words following `ispell-words-keyword' (usually
      \"LocalWords\") in the current buffer."
    (require 'ispell)
    (save-excursion
      (goto-char (point-min))
      (cl-loop while (search-forward ispell-words-keyword nil t)
               collect (string-trim (buffer-substring-no-properties (point) (line-end-position))) into result
               finally return (mapconcat #'identity result " "))))

  (defun my/jinx-add-ispell-localwords ()
    "Add ispell's local words to `jinx-local-words'."
    (let ((ispell-localwords (my/jinx-ispell-localwords)))
      (setq jinx-local-words (concat jinx-local-words ispell-localwords))
      (setq jinx--session-words (append jinx--session-words (split-string ispell-localwords)))))

  (add-hook 'jinx-mode-hook #'my/jinx-add-ispell-localwords)

  (defun my/jinx-save-as-ispell-localword (save key word)
    "Save WORD using ispell's `ispell-words-keyword'.
    If SAVE is non-nil save, otherwise format candidate given action KEY."
    (if save
        (progn
          (require 'ispell)
          (ispell-add-per-file-word-list word)
          (add-to-list 'jinx--session-words word)
          (setq jinx-local-words
                (string-join
                 (sort (delete-dups
                        (cons word (split-string jinx-local-words)))
                       #'string<)
                 " "))))
    (list key word "File"))

  (setf (alist-get ?* jinx--save-keys) #'my/jinx-save-as-ispell-localword)
  )

;; (use-package flyspell-correct
;;   :after flyspell
;;   :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package grammarly
  :ensure (:host github :repo "emacs-grammarly/grammarly")
  :config
  (grammarly-load-from-authinfo)
  )
(use-package flymake-grammarly
  :ensure (:host github :repo "emacs-grammarly/flymake-grammarly")
  :after grammarly
  :hook (LaTeX-mode . flymake-grammarly-load)
  :config
  (setq flymake-grammarly-check-time 0.8)
  )

(provide 'config-autofix)
