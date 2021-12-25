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
   ;; (undecorated . 1)                 ; this will completely remove the titlebar
   (ns-titlebar-height-adjust . -10)    ; this is actually not used
   (ns-title-hidden . 1)                ; hide the title text in the titlebar
   (ns-fullsize-content . 1)            ; make the content full size
   (ns-transparent-titlebar . 1)        ; make the titlebar transparent
   (vertical-scroll-bars . nil)))       ;; No vertical scroll-bars

;; It seems that we don't need the following anymore.
;; ;;; Window
;; (tool-bar-mode -1) ; close tool bar (-1 is switch)
;; (menu-bar-mode -1) ; close menu bar
;; (scroll-bar-mode -1) ; close the scroll bar

;; refs: https://emacs-china.org/t/inconsolata/7997
;; (when (member "Sarasa Mono SC" (font-family-list))
;;   (set-fontset-font t nil (font-spec :family "Sarasa Mono SC"))
;;   (set-frame-font "Sarasa Mono SC" t t)
;;   (add-to-list 'default-frame-alist '(font . "Sarasa Mono SC-14")))

;;; early-init.el ends here
