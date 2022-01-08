;;; early-init.el -- Early Emacs configuration cloned from https://github.com/angrybacon/dotemacs/blob/master/early-init.el
;;; Also many hints from <https://github.com/sachac/.emacs.d>.

;;; Commentary:

;;; Code:

(setq-default
 load-prefer-newer t ; Load newer packages when available.
 package-enable-at-startup nil
 package-native-compile t)

(when (display-graphic-p)
  (tool-bar-mode nil) ; t for enable, nil for disable, -1 for toggle
  (scroll-bar-mode nil)
  (menu-bar-mode nil))

(setq source-directory (expand-file-name "~/Library/Caches/Homebrew/emacs-plus@29--git"))

(setq-default
 default-frame-alist
 '(
   (left-fringe . 8)                    ;; Thin left fringe
   (menu-bar-lines . 0)                 ; No menu bar
   (right-divider-width . 1)            ;; Thin vertical window divider
   (right-fringe . 3)                   ;; Thin right fringe
   (tool-bar-lines . 0)                 ; No tool bar
   ;; (undecorated . 1)                 ; this will completely remove the titlebar
   (ns-titlebar-height-adjust . -10)    ; this is actually not used
   (ns-title-hidden . 1)                ; hide the title text in the titlebar
   (ns-fullsize-content . 1)            ; make the content full size
   (ns-transparent-titlebar . 1)        ; make the titlebar transparent
   (vertical-scroll-bars . nil)         ; No vertical scroll-bars
   (horizontal-scroll-bars . nil)       ; No horizontal scroll-bars
   ))

;;; early-init.el ends here
