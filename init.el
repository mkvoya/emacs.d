;;; init.el --- Bootstrap for the org configuration files cloned from https://github.com/angrybacon/dotemacs/blob/master/init.el
;;; Also many hints from <https://github.com/sachac/.emacs.d>.

;;; Commentary:

;;; Code:

 (let* ((file-name-handler-alist nil)
        (gc-cons-percentage .6)
        (gc-cons-threshold most-positive-fixnum)
        (read-process-output-max (* 1024 1024)))

    ;; Disable that pesky echo message
    ;; (setq inhibit-startup-echo-area-message user-login-name)

    ;; Mark safe variables early so that tangling won't break
    ;; MK: What's for?
    ;; (put 'after-save-hook 'safe-local-variable
    ;;      (lambda (value) (equal value '(org-babel-tangle t))))
    ;; (put 'display-line-numbers-width 'safe-local-variable 'integerp)

    ;;  ;; Tangle and compile if necessary only, then load the configuration
    ;;  (let* ((modification-time
    ;;          (file-attribute-modification-time (file-attributes .org))))
    ;;    (require 'org-macs)
    ;;    (unless (org-file-newer-than-p .el modification-time)
    ;;      (require 'ob-tangle)
    ;;      (org-babel-tangle-file .org .el "emacs-lisp")
    ;;      (byte-compile-file .el))
    ;;    (load-file .el))
    (load-file "~/.emacs.d/emacs-config.el")

    ;; Collect garbage when all else is done
    (garbage-collect))




;;; init.el ends here
