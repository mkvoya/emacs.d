;;; init.el --- Bootstrap for the org configuration files cloned from https://github.com/angrybacon/dotemacs/blob/master/init.el
;;; Also many hints from <https://github.com/sachac/.emacs.d>.

;;; Commentary:

;;; Code:


(setq gc-cons-threshold (* 400 (expt 2 20))
      gc-cons-percentage 0.6)

 (let* ((file-name-handler-alist nil)
        (read-process-output-max (expt 2 22)))

    (load "~/.emacs.d/emacs-config.el")

    ;; Collect garbage when all else is done
    ;; (garbage-collect)
    )

(setq gc-cons-threshold (expt 2 23)
      gc-cons-percentage 0.1)

(message "Everything is up. Wish you a nice day. :)")

;;; init.el ends here
