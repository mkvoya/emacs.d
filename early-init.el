;;; early-init.el -- Early Emacs configuration cloned from https://github.com/angrybacon/dotemacs/blob/master/early-init.el
;;; Also many hints from <https://github.com/sachac/.emacs.d>.

;;; Commentary:

;;; Code:

;; (setq debug-on-error t)
;; (setq debug-on-signal t)
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
   (width . 90)
   (height . 100)
   (font . "Menlo-14")
   (minibuffer . t)
   ))

;; (when window-system (set-frame-size (selected-frame) 100 80))


;;; Thank https://github.com/Eason0210/emacs.d/blob/master/early-init.el
(setq load-prefer-newer t)
(setq package-enable-at-startup nil)
;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; UTF-8
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPUND_TEXT TEXT STRING)))
;; Unicode
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; lazy answer
(fset 'yes-or-no-p 'y-or-n-p)

(setq confirm-kill-emacs 'y-or-n-p)

;; Sentence
(setq sentence-end-double-space nil) ; Use only one space to end a sentence

;;; early-init.el ends here
