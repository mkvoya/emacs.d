;; -*- lexical-binding: t; -*-
;;; Emacs Config Fragement: AucTeX (LaTeX)

(use-package tex
  :ensure (auctex :repo "https://git.savannah.gnu.org/git/auctex.git" :branch "main"
                  :pre-build (("make" "elpa"))
                  :build (:not elpaca--compile-info) ;; Make will take care of this step
                  :files ("*.el" "doc/*.info*" "etc" "images" "latex" "style")
                  :version (lambda (_) (require 'auctex) AUCTeX-version))
  :defer t
  :init
  (setq TeX-source-correlate-method 'synctex)
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-start-server t)
  (provide 'tex-buf)  ; We don't have tex-buf anymore, just add this to make some packages happy.

  :config
  ;; make latexmk available via C-c C-c
  ;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
  ;; (add-to-list 'TeX-command-list '("latexmk" "latexmk -pdf -escape-shell %s" TeX-run-TeX nil t :help "Run latexmk on file"))
  (add-to-list 'TeX-command-list '("Make" "make" TeX-run-command nil t))
  (add-to-list 'TeX-command-list '("Make Clean" "make clean" TeX-run-command nil t))
  (add-to-list 'TeX-command-list '("Make Clean & Make" "make clean && make" TeX-run-command nil t))
  ;; "texcount article.tex -inc -incbib -sum"
  (add-to-list 'TeX-command-list '("Make Count" "make count" TeX-run-command nil t))
  ;; (setq-default TeX-command-default "Make")
  ;; from https://gist.github.com/stefano-meschiari/9217695
  (setq TeX-auto-save t)
  (setq Tex-parse-self t)
  ;; Guess/Ask for the master file.
  (setq-default TeX-master nil)


  ;; Thank https://tex.stackexchange.com/a/167097/122482
  (defun mk/shadow-iffalse-block ()
    (font-lock-add-keywords
     'latex-mode
     '(("\\\\iffalse\\(\\(.\\|\n\\)*\\)\\\\fi" 0 'font-lock-comment-face append))
     t))
  (add-hook 'latex-mode-hook #'mk/shadow-iffalse-block)

  (add-hook 'TeX-mode-hook (lambda () (setq TeX-command-default "Make")))
  (add-hook 'LaTeX-mode-hook (lambda()
                               (visual-line-mode t)
                               ;; (flyspell-mode)
                               ;; (add-hook 'after-save-hook #'flyspell-buffer)
                               (LaTeX-math-mode)
                               (mk/eglot-ensure)
                               ;; (darkroom-mode)
                               ;; (setq buffer-face-mode-face '(:family "iA Writer Quattro V"))
                               ;; (buffer-face-mode)
                               ;; (visual-line-mode)
                               ;; (visual-line-mode)
                               ;; (darkroom-decrease-margins 0.8)
                               ))
  ;; (add-hook 'TeX-output-mode (lambda () (goto-char (point-max))))

  (setq reftex-refstyle "\\ref")
  (setq reftex-plug-into-AUCTeX t)
  (setq TeX-PDF-mode t)

  (setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
  (setq TeX-view-program-list
        '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

  (setq TeX-error-overview-open-after-TeX-run t)
  ;; (setq mkvoya/tex-auto-compile nil)
  ;; (defun mkvoya/tex-try-auto-compile ()
  ;;   (when (and (eq major-mode 'TeX-mode)
  ;;              (mkvoya/tex-auto-compile))
  ;;     (TeX-command-run))
  ;;   )
  ;; (add-hook 'after-save-hook #'mkvoya/tex-try-auto-compile)

  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

  (use-package reftex
    :ensure nil
    :defer t
    :config
    (add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
    (add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode
    ;; (setq reftex-default-bibliography '("./references.bib"))
    )
  ;; (use-package auctex-latexmk :config (auctex-latexmk-setup))
  ;; https://emacs.stackexchange.com/a/63195/30542
  (defun my-LaTeX-mode-setup ()
    (font-latex-add-keywords '(("autoref" "*{") ("Autoref" "{") ("nameref" "*{"))
                             'reference))

  (add-hook 'LaTeX-mode-hook #'my-LaTeX-mode-setup)
  )

;; Show build progress in modeline
(use-package procress
  :ensure (:host github :repo "haji-ali/procress")
  :commands procress-auctex-mode
  :init
  (add-hook 'LaTeX-mode-hook #'procress-auctex-mode)
  :config
  (procress-load-default-svg-images))

;; https://emacs.stackexchange.com/questions/45546/per-mode-value-for-fill-column
(defun mkvoya/tex-mode-hook ()
  (setq fill-column 1024))
(add-hook 'TeX-mode-hook #'mkvoya/tex-mode-hook)

(provide 'config-auctex)
