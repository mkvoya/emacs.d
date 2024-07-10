;;; init.el --- -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:


(setq gc-cons-threshold (* 4 (expt 2 20))
      gc-cons-percentage 0.6)

(let* (;; (file-name-handler-alist nil)  ; This causes loading issues. Check: https://lists.gnu.org/archive/html/emacs-devel/2022-08/msg00218.html
       (read-process-output-max (expt 2 22)))

  ;; 将lisp目录放到加载路径的前面以加快启动速度
  (let ((dir (locate-user-emacs-file "init-lisp")))
    (add-to-list 'load-path (file-name-as-directory dir)))
  (let ((dir (locate-user-emacs-file "lisp")))
    (add-to-list 'load-path (file-name-as-directory dir)))

  (load "~/.emacs.d/emacs-config.el")

  ;; Collect garbage when all else is done
  ;; (garbage-collect)
  )

(setq gc-cons-threshold (expt 2 23)
      gc-cons-percentage 0.1)
(setq gc-cons-threshold (expt 2 28)
      gc-cons-percentage 0.3)

(message "Everything is up. Wish you a nice day. :)")
(put 'narrow-to-region 'disabled nil)

(provide 'init)
;;; init.el ends here
