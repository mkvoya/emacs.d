;;; init.el --- Bootstrap for the org configuration files cloned from https://github.com/angrybacon/dotemacs/blob/master/init.el
;;; Also many hints from <https://github.com/sachac/.emacs.d>.

;;; Commentary:

;;; Code:

 (let* ((file-name-handler-alist nil)
        (gc-cons-percentage .6)
        (gc-cons-threshold most-positive-fixnum)
        (read-process-output-max (* 1024 1024)))

    (load-file "~/.emacs.d/emacs-config.el")

    ;; Collect garbage when all else is done
    (garbage-collect))




;;; init.el ends here
