;; -*- lexical-binding: t; -*-
;;; Emacs Config Fragement: Bootstrap (should be loaded first)

;; Setup Elpaca
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca-wait)
(setq use-package-always-ensure t)
(elpaca bind-key) ; elpaca-use-package will trigger bind-key, so we have to specify it before
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))
(elpaca-wait)
(use-package cua-base :ensure nil)
(elpaca-wait)


(setq custom-file "~/.emacs.d/customs.el")
(add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))

(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'elpaca-after-init-hook 'benchmark-init/deactivate))

;; (use-package bind-key :ensure (:wait t))
(use-package compat :ensure (:wait t))
(use-package use-package-ensure-system-package :ensure nil)
(use-package delight :ensure (:wait t))

;; Get shell env from user shell.
;; https://apple.stackexchange.com/questions/51677/how-to-set-path-for-finder-launched-applications
;; $ sudo launchctl config user path /usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin
;; We need to at least make the /usr/local/bin in the path so that imagemagick can use rsgv rather than its built-in svg renderer.
;; The above command works.
(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))


(provide 'config-bootstrap)
