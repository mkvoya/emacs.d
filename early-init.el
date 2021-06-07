;;; early-init.el -- Early Emacs configuration cloned from https://github.com/angrybacon/dotemacs/blob/master/early-init.el
;;; Also many hints from <https://github.com/sachac/.emacs.d>.

;;; Commentary:

;;; Code:

(setq-default
 load-prefer-newer t ; Load newer packages when available.
 package-enable-at-startup nil
 package-native-compile t)

(setq-default
 default-frame-alist
 '((horizontal-scroll-bars . nil)       ;; No horizontal scroll-bars
   (left-fringe . 8)                    ;; Thin left fringe
   (menu-bar-lines . 0)                 ;; No menu bar
   (right-divider-width . 1)            ;; Thin vertical window divider
   (right-fringe . 3)                   ;; Thin right fringe
   (tool-bar-lines . 0)                 ;; No tool bar
   (font . "Sarasa Mono SC-13")         ;; Set font
   (vertical-scroll-bars . nil)))       ;; No vertical scroll-bars

;; It seems that we don't need the following anymore.
;; ;;; Window
;; (tool-bar-mode -1) ; close tool bar (-1 is switch)
;; (menu-bar-mode -1) ; close menu bar
;; (scroll-bar-mode -1) ; close the scroll bar

;; refs: https://emacs-china.org/t/inconsolata/7997
(when (member "Sarasa Mono SC" (font-family-list))
  (set-fontset-font t nil (font-spec :family "Sarasa Mono SC"))
  (set-frame-font "Sarasa Mono SC" t t)
  (add-to-list 'default-frame-alist '(font . "Sarasa Mono SC-13")))

;;; early-init.el ends here
