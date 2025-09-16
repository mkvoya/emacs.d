;;; early-init.el --- Early Emacs configuration  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(defvar is-android (string= system-type "android") "Whether on Anroid.")

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


(setenv "LIBRARY_PATH"
        (format "%s:%s"
                (string-join
                 '("/opt/homebrew/opt/gcc/lib/gcc/current"
                   "/opt/homebrew/opt/libgccjit/lib/gcc/current"
                   "/opt/homebrew/opt/gcc/lib/gcc/current/gcc/aarch64-apple-darwin24/14/"
                   ) ":")
                (or (getenv "LD_LIBRARY_PATH") "")))

;; (when (display-graphic-p)
;;   (tool-bar-mode -1) ; t for enable, nil for disable, -1 for toggle
;;   (scroll-bar-mode -1)
;;   (menu-bar-mode -1))

(setq source-directory (expand-file-name "~/Library/Caches/Homebrew/emacs-plus@31--git"))

(setq-default
 default-frame-alist
 `(
   (left-fringe . 8)                    ;; Thin left fringe
   ;; (menu-bar-lines . 0)                 ; No menu bar
   (right-divider-width . 1)            ;; Thin vertical window divider
   (right-fringe . 3)                   ;; Thin right fringe
   ;; (tool-bar-lines . 0)                 ; No tool bar
   ;; (tab-bar-lines . 0)                  ; No tab bar
   ;; (undecorated . 1)                 ; this will completely remove the titlebar
   ;; (ns-titlebar-height-adjust . -10)    ; this is actually not used
   (ns-title-hidden . 1)                ; hide the title text in the titlebar
   (ns-fullsize-content . 1)            ; make the content full size
   (ns-transparent-titlebar . 1)        ; make the titlebar transparent
   (vertical-scroll-bars . nil)         ; No vertical scroll-bars
   (horizontal-scroll-bars . nil)       ; No horizontal scroll-bars
   (undecorated . t)
   (width . 120)
   (height . 50)
   (font . "Monaco-12")
   (minibuffer . t)
   (drag-with-header-line . t)
   (drag-with-mode-line . t)
   (drag-with-internal-border . t)
   ))
;; (set-frame-font "Monaco-12" nil t)
(set-face-attribute 'fixed-pitch nil :family "Monaco")

;; (when window-system (set-frame-size (selected-frame) 120 50))

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
