;;; package -- mk/org-mode-config
;;; Commentary:
;;;
;;; mk's org mode config

;;; Code:

;; Enable Org mode
(require 'org)

;; Shortcuts
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; Org mode TODO states
(setq org-todo-keywords
      '((sequence
         "TODO(t)"
         "HAND(h)"
         "WAIT(w)"
         "|"
         "DONE(d!)"
         "CANCELED(c@)"
         )))
;; Keyword colors
(setf org-todo-keyword-faces
      '(
        ;; Many styles from https://github.com/GTrunSec/my-profile/blob/master/dotfiles/doom-emacs/config.org
        ("TODO" . (:foreground "#ff39a3" :weight bold))
        ("HAND"  . "orangered")
        ;; ("WAIT" . (:foreground "orange" :weight bold))
        ("WAIT" . "pink")
        ("CANCELED" . (:foreground "white" :background "#4d4d4d" :weight bold))
        ("DONE" . "#008080")
        ;; ("FIXME" . "IndianRed")
        ;; ("☟ NEXT" . (:foreground "DeepSkyBlue"
        ;;                         ;; :background "#7A586A"
        ;;                         :weight bold))
        ;; ("☕ BREAK" . "gray")
        ;; ("❤ LOVE" . (:foreground "VioletRed4"
        ;;                       ;; :background "#7A586A"
        ;;                       :weight bold))
        ))


;;; Org Style
;; from https://www.lijigang.com/blog/2018/08/08/神器-org-mode/#org4288876
;; 打开 org-indent mode
(setq org-startup-indented t)
(require 'org-superstar)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

;; 设置 bullet list
(with-eval-after-load 'org-superstar
  (set-face-attribute 'org-superstar-item nil :height 1.2)
  (set-face-attribute 'org-superstar-header-bullet nil :height 1.2)
  (set-face-attribute 'org-superstar-leading nil :height 1.3))
;; Set different bullets, with one getting a terminal fallback.
(setq org-superstar-headline-bullets-list
      '("◉" "◈" "○" "▷"))
;; Stop cycling bullets to emphasize hierarchy of headlines.
(setq org-superstar-cycle-headline-bullets nil)
;; Hide away leading stars on terminal.
(setq org-superstar-leading-fallback ?\s)

;; agenda 里面时间块彩色显示
;; From: https://emacs-china.org/t/org-agenda/8679/3
(defun ljg/org-agenda-time-grid-spacing ()
  "Set different line spacing w.r.t. time duration."
  (save-excursion
    (let* ((background (alist-get 'background-mode (frame-parameters)))
           (background-dark-p (string= background "dark"))
           (colors (list "#1ABC9C" "#2ECC71" "#3498DB" "#9966ff"))
           pos
           duration)
      (nconc colors colors)
      (goto-char (point-min))
      (while (setq pos (next-single-property-change (point) 'duration))
        (goto-char pos)
        (when (and (not (equal pos (point-at-eol)))
                   (setq duration (org-get-at-bol 'duration)))
          (let ((line-height (if (< duration 30) 1.0 (+ 0.5 (/ duration 60))))
                (ov (make-overlay (point-at-bol) (1+ (point-at-eol)))))
            (overlay-put ov 'face `(:background ,(car colors)
                                                :foreground
                                                ,(if background-dark-p "black" "white")))
            (setq colors (cdr colors))
            (overlay-put ov 'line-height line-height)
            (overlay-put ov 'line-spacing (1- line-height))))))))
(add-hook 'org-agenda-finalize-hook #'ljg/org-agenda-time-grid-spacing)


;; Captures
(setq org-capture-templates nil)
(add-to-list 'org-capture-templates
             '("j" "Journals" entry
               (file+datetree "~/Dropbox/Dreams/Org/Journals/Index.org" "Journals")
               "* %U - %^{heading}\n  %?"))
;; (setq org-default-notes-file "~/Dropbox/Dreams/Org/Inbox.org")
(add-to-list 'org-capture-templates
             '("t" "Tasks" entry
               (file+headline "~/Dropbox/Dreams/Org/Inbox.org" "Tasks")
               "* TODO %?\nADDED: %u\n%a"))
(add-to-list 'org-capture-templates
             '("b" "Bookmarks" entry
               (file+datetree "~/Dropbox/Dreams/Org/Collections/Bookmarks.org" "Bookmarks")
               "* %U - %^{title}\nADDED: %U\n%?"))

;;; Paste Image From https://emacs-china.org/t/topic/6601/4
(defun org-insert-image ()
  "Insert a image from clipboard."
  (interactive)
  (let* ((path (concat default-directory "img/"))
         (image-file (concat
                      path
                      (buffer-name)
                      (format-time-string "_%Y%m%d_%H%M%S.png"))))
    (if (not (file-exists-p path))
        (mkdir path))
    (do-applescript (concat
                     "set the_path to \"" image-file "\" \n"
                     "set png_data to the clipboard as «class PNGf» \n"
                     "set the_file to open for access (POSIX file the_path as string) with write permission \n"
                     "write png_data to the_file \n"
                     "close access the_file"))
    ;; (shell-command (concat "pngpaste " image-file))
    (org-insert-link nil
                     (concat "file:" image-file)
                     "")
    (message image-file))
  (org-display-inline-images)
  )

(setq org-latex-to-pdf-process
      '("xelatex -interaction nonstopmode %f"
        "xelatex -interaction nonstopmode %f")) ;; for multiple passes

;;; from https://christiantietze.de/posts/2019/12/emacs-notifications/
(require 'appt)

(setq appt-time-msg-list nil) ;; clear existing appt list
(setq appt-display-interval '5) ;; warn every 5 minutes from t - appt-message-warning-time
(setq
 appt-message-warning-time '15 ;; send first warning 15 minutes before appointment
 appt-display-mode-line nil ;; don't show in the modeline
 appt-display-format 'window) ;; pass warnings to the designated window function
(setq appt-disp-window-function (function t/appt-display-native))

(appt-activate 1) ;; activate appointment notification
; (display-time) ;; Clock in modeline

(setq exec-path (append '("~/.emacs.d/3rd-parties/alerter/bin") exec-path))
(defvar alerter-command (executable-find "alerter") "The path to alerter.")

(defun ct/send-notification (title msg)
  (let ((notifier-path (executable-find "alerter")))
    (start-process
     "Appointment Alert"
     "*Appointment Alert*" ; use `nil` to not capture output; this captures output in background
     notifier-path
     "-message" msg
     "-title" title
     "-sender" "org.gnu.Emacs"
     "-activate" "org.gnu.Emacs")))
(defun ct/appt-display-native (min-to-app new-time msg)
  (ct/send-notification
   (format "Appointment in %s minutes" min-to-app) ; Title
   (format "%s" msg))) ; Message/detail text

;; Agenda-to-appointent hooks
(org-agenda-to-appt) ;; generate the appt list from org agenda files on emacs launch
(run-at-time "24:01" 3600 'org-agenda-to-appt) ;; update appt list hourly
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt) ;; update appt list on agenda view

;; Auto add DONE TIME, from https://orgmode.org/guide/Progress-Logging.html
(setq org-log-done 'time)

;; Org export code style
(setq org-html-htmlize-output-type 'css)

(provide 'config-org)
;;; config-org.el ends here
