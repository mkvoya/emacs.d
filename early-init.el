;;; early-init.el -- Early Emacs configuration cloned from https://github.com/angrybacon/dotemacs/blob/master/early-init.el

;;; Commentary:

;;; Code:

(setq-default
 load-prefer-newer t
 package-enable-at-startup nil
 package-native-compile t)

(setq-default
 default-frame-alist
 '((horizontal-scroll-bars . nil)       ;; No horizontal scroll-bars
   (left-fringe . 3)                    ;; Thin left fringe
   (menu-bar-lines . 0)                 ;; No menu bar
   (right-divider-width . 1)            ;; Thin vertical window divider
   (right-fringe . 3)                   ;; Thin right fringe
   (tool-bar-lines . 0)                 ;; No tool bar
   (vertical-scroll-bars . nil)))       ;; No vertical scroll-bars

;;; early-init.el ends here
