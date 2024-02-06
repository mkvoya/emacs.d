;;; early-init.el --- Early Emacs configuration  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(defvar mk-android (string= system-type "android") "Whether on Anroid.")

;; (setq debug-on-error t)
;; (setq debug-on-signal t)
(setq-default
 ;; Package
 load-prefer-newer t ; Load newer packages when available.
 package-enable-at-startup nil  ; do not load packege.el
 package-quickstart nil
 package-native-compile nil
 ;; GC
 gc-cons-threshold most-positive-fixnum
 gc-cons-percentage 0.6
 ;; Resizing the Emacs frame can be a terribly expensive part of changing the
 ;; font. By inhibiting this, we easily halve startup times with fonts that are
 ;; larger than the system default.
 frame-inhibit-implied-resize t
 )

(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'y-or-n-p)
(setq comp-deferred-compilation nil)


(when mk-android
  (setenv "PATH" (format "%s:%s" "/data/data/com.termux/files/usr/bin"
                         (getenv "PATH")))
  (setenv "LD_LIBRARY_PATH" (format "%s:%s"
                                    "/data/data/com.termux/files/usr/lib"
                                    (getenv "LD_LIBRARY_PATH")))
  (push "/data/data/com.termux/files/usr/bin" exec-path)
  )

(unless mk-android
  (when (display-graphic-p)
    (tool-bar-mode nil) ; t for enable, nil for disable, -1 for toggle
    (scroll-bar-mode nil)
    (menu-bar-mode nil))

  (setq source-directory (expand-file-name "~/Library/Caches/Homebrew/emacs-plus@30--git"))

  (defun simple-top-bar-render (left right)
    "Return a string of `frame-width' length. Containing LEFT, and RIGHT aligned respectively."
    (let ((available-width (- (frame-width top-bar-current-frame)
                              (+ (length (format-mode-line left))
                                 (length (format-mode-line right))
                                 )
                              15)))
      (append left
              (list (format (format "%%%ds" available-width) ""))
              right)))
  (defun mk/set-frame-top-bar-format (format)
    (dolist (frame (frame-list))
      (set-frame-parameter frame 'top-bar-format format)
      )
    )

  (defvar mk/default-frame-top-bar-format
    '((:eval
       (simple-top-bar-render
        ;; Left.
        `("           "
          "MK's EMACS "
          ,(format "[%s]"
                   (buffer-name
                    (window-buffer
                     (get-mru-window top-bar-current-frame)))))
        ;; Right.
        '(mode-line-misc-info
          )))))
  ;; (mk/set-frame-top-bar-format mk/default-frame-top-bar-format)

  (setq-default
   default-frame-alist
   `(
     (left-fringe . 8)                    ;; Thin left fringe
     (menu-bar-lines . 0)                 ; No menu bar
     (right-divider-width . 1)            ;; Thin vertical window divider
     (right-fringe . 3)                   ;; Thin right fringe
     (tool-bar-lines . 0)                 ; No tool bar
     (tab-bar-lines . 0)                  ; No tab bar
     (top-bar-lines . 1)                  ; Enable top bar
     (top-bar-format . ,mk/default-frame-top-bar-format)   ; Enable top bar
     ;; (undecorated . 1)                 ; this will completely remove the titlebar
     (ns-titlebar-height-adjust . -10)    ; this is actually not used
     (ns-title-hidden . 1)                ; hide the title text in the titlebar
     (ns-fullsize-content . 1)            ; make the content full size
     (ns-transparent-titlebar . 1)        ; make the titlebar transparent
     (vertical-scroll-bars . nil)         ; No vertical scroll-bars
     (horizontal-scroll-bars . nil)       ; No horizontal scroll-bars
     (undecorated . t)
     (width . 150)
     (height . 100)
     (font . "Monaco-12")
     (minibuffer . t)
     ))
  ;; (set-frame-font "Monaco-12" nil t)
  (set-face-attribute 'fixed-pitch nil :family "Monaco")

  ) ; End of unless mk-android

;; (when window-system (set-frame-size (selected-frame) 100 80))

;; UTF-8
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPUND_TEXT TEXT STRING)))
;; Unicode
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

(setq current-language-environment "UTF-8")
;; (setq default-input-method "rfc1345")

;; Sentence
(setq sentence-end-double-space nil) ; Use only one space to end a sentence

(provide 'early-init)
    ;;; early-init.el ends here
