;;; package -- summary
;;; Commentary:
;;;
;;; Theme

;;; Code:

;; from https://emacs-china.org/t/inconsolata/7997
(when (member "Inconsolata" (font-family-list))
  (set-frame-font "Inconsolata-16" t t)
  (add-to-list 'default-frame-alist
               '(font . "Inconsolata-16")))
;; (when (member "Monaco" (font-family-list))
;;   (set-frame-font "Monaco-11" t t))
;; from http://baohaojun.github.io/perfect-emacs-chinese-font.html
;; (when (member "Noto Sans Mono" (font-family-list))
;;   (set-fontset-font t 'han "Noto Sans Mono"))

(when (member "Microsoft Yahei" (font-family-list))
  (set-fontset-font t 'han "Microsoft Yahei"))
(setq face-font-rescale-alist '(("Microsoft Yahei" . 1.2) ("WenQuanYi Zen Hei" . 1.2)))



(provide 'config-fonts)
;;; config-fonts.el ends here
