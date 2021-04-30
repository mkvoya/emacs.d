;;; package -- summary
;;; Commentary:
;;;
;;; Theme

;;; Code:

;;; ========= section font ===========

;; Check using what-cursor-position. https://stackoverflow.com/questions/1242352/get-font-face-under-cursor-in-emacs

(when (member "Sarasa Mono SC" (font-family-list))
  ;; (set-fontset-font t 'han (font-spec :family "Sarasa Mono SC"))
  (set-fontset-font t nil (font-spec :family "Sarasa Mono SC"))
  ;; Specifying the range seems necessary to avoid line bumps.
  ;; According to https://emacs.stackexchange.com/questions/5689/force-a-single-font-for-all-unicode-glyphs
  ;; The following config is overwritten by someone (unicode-fonts).
  ;; (set-fontset-font t '(#x000000 . #x3fffff) (font-spec :family "Sarasa Mono SC"))
  (set-frame-font "Sarasa Mono SC" t t)
  (add-to-list 'default-frame-alist '(font . "Sarasa Mono SC-16")))

;;;; from https://emacs-china.org/t/inconsolata/7997
;; (when (member "Inconsolata" (font-family-list))
;;   (set-frame-font "Inconsolata-14" t t)
;;   (add-to-list 'default-frame-alist
;;                '(font . "Inconsolata-14")))
;; (when (member "Sarasa Mono SC" (font-family-list))
;;   (set-fontset-font t 'han (font-spec :family "Sarasa Mono SC")))
;; (when (member "Monaco" (font-family-list))
;;   (set-frame-font "Monaco-11" t t))
;; from http://baohaojun.github.io/perfect-emacs-chinese-font.html
;; (when (member "Noto Sans Mono" (font-family-list))
;;   (set-fontset-font t 'han "Noto Sans Mono"))
;;
;;;; (when (member "Microsoft Yahei" (font-family-list))
;;;;   (set-fontset-font t 'han "Microsoft Yahei"))
;;;; (setq face-font-rescale-alist '(("Microsoft Yahei" . 1.2) ("WenQuanYi Zen Hei" . 1.2)))
;; (when (member "Hiragino Sans GB" (font-family-list))
;;   (set-fontset-font t 'han (font-spec :family "Hiragino Sans GB")))
;; (setq face-font-rescale-alist '(("Hiragino Sans GB" . 1)))
;;
;;;; 测试对比对
;;;; Ok good it.



;; (set-frame-font "InconsolataGo QiHei NF-16" t t)
;; (add-to-list 'default-frame-alist
;;             '(font . "InconsolataGo QiHei NF-16"))

;; (set-default-font "InconsolataGo QiHei NF 16")
;; (setq default-frame-alist '((font . "InconsolataGo QiHei NF-16")))


;; Disable since it seems not work.
;; ;;; Overlays for line height? from https://stackoverflow.com/questions/26437034/emacs-line-height. Does not work well for fonts
;; ;; Set the padding between lines
;; (defvar line-padding 3)
;; (defun add-line-padding ()
;;   "Add extra padding between lines."
;;   ;; remove padding overlays if they already exist
;;   (interactive)
;;   (let ((overlays (overlays-at (point-min))))
;;     (while overlays
;;       (let ((overlay (car overlays)))
;;         (if (overlay-get overlay 'is-padding-overlay)
;;             (delete-overlay overlay)))
;;       (setq overlays (cdr overlays))))
;;   ;; add a new padding overlay
;;   (let ((padding-overlay (make-overlay (point-min) (point-max))))
;;     (overlay-put padding-overlay 'is-padding-overlay t)
;;     ;; (overlay-put padding-overlay 'line-spacing (* .1 line-padding))
;;     (overlay-put padding-overlay 'line-height (+ 1 2)))
;;   (setq mark-active nil))
;; (add-hook 'buffer-list-update-hook 'add-line-padding)
;;; ========= section general ===========

;; ;; Dark and Light theme
;; (use-package heaven-and-hell
;;   :ensure t
;;   :init
;;   (setq heaven-and-hell-theme-type 'dark) ;; Omit to use light by default
;;   (setq heaven-and-hell-themes
;;         '((light . tsdh-light)
;;           (dark . tsdh-dark))) ;; Themes can be the list: (dark . (tsdh-dark wombat))
;;   ;; Optionall, load themes without asking for confirmation.
;;   (setq heaven-and-hell-load-theme-no-confirm t)
;;   :hook (after-init . heaven-and-hell-init-hook)
;;   :bind (("C-c <f6>" . heaven-and-hell-load-default-theme)
;;          ("<f6>" . heaven-and-hell-toggle-theme)))

(global-hl-line-mode 1)
(set-face-background 'hl-line "#d0fed0")
(set-face-foreground 'hl-line nil)
(set-face-foreground 'highlight nil)
;; (set-face-attribute 'default nil :height 160)

(add-hook 'prog-mode-hook #'hs-minor-mode)

(provide 'config-appearances)
;;; config-appearances.el ends here
