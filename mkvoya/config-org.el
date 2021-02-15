;;; package -- mk/org-mode-config
;;; Commentary:
;;;
;;; mk's org mode config

;;; Code:

;; Enable Org mode
(use-package org
  :defer t
  :config

  ;; Shortcuts
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)

  ;; Auto add DONE TIME, from https://orgmode.org/guide/Progress-Logging.html
  (setq org-log-done 'time)

  ;; Org mode TODO states
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"
           "HAND(h)"
           "WAIT(w)"
           "LONG-TERM(l)"
           "DELEGATE(e)"
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
          ("DELEGATE"  . "DeepSkyBlue")
          ;; ("FIXME" . "IndianRed")
          ;; ("☟ NEXT" . (:foreground "DeepSkyBlue"
          ;;                         ;; :background "#7A586A"
          ;;                         :weight bold))
          ;; ("☕ BREAK" . "gray")
          ;; ("❤ LOVE" . (:foreground "VioletRed4"
          ;;                       ;; :background "#7A586A"
          ;;                       :weight bold))
          ))

  ;; from https://github.com/psamim/dotfiles/blob/master/doom/config.el#L73
  ;; (setq org-ellipsis "…")
  ;; ➡, ⚡, ▼, ↴, , ∞, ⬎, ⤷, ⤵
  ;; (setq org-ellipsis "↴▾▽▼↩↘↸")
  (setq org-ellipsis "▾")

  ;; https://stackoverflow.com/questions/17590784/how-to-let-org-mode-open-a-link-like-file-file-org-in-current-window-inste
  (defun org-force-open-current-window ()
    "Open at current window."
    (interactive)
    (let ((org-link-frame-setup (quote
                                 ((vm . vm-visit-folder)
                                  (vm-imap . vm-visit-imap-folder)
                                  (gnus . gnus)
                                  (file . find-file)
                                  (wl . wl)))
                                ))
      (org-open-at-point)))

  ;; Depending on universal argument try opening link
  (defun org-open-maybe (&optional arg)
    "Open maybe ARG."
    (interactive "P")
    (if arg (org-open-at-point)
      (org-force-open-current-window)))
  ;; Redefine file opening without clobbering universal argument
  (define-key org-mode-map "\C-c\C-o" 'org-open-maybe)
  ;; org inline image width from https://www.reddit.com/r/emacs/comments/55zk2d/adjust_the_size_of_pictures_to_be_shown_inside/
  (setq org-image-actual-width (/ (display-pixel-width) 3))

  (add-hook 'org-mode-hook (lambda () (electric-indent-local-mode -1)))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)
     (C . t)))

  ;; https://emacs.stackexchange.com/questions/3302/live-refresh-of-inline-images-with-org-display-inline-images
  ;; Always redisplay inline images after executing SRC block
  (eval-after-load 'org
    (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images))
  )

;;; Org Style
;; from https://www.lijigang.com/blog/2018/08/08/神器-org-mode/#org4288876
;; ;; 打开 org-indent mode
;; (setq org-startup-indented t)
(use-package org-superstar
  :after (org)
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  ;; 设置 bullet list
  ;; (with-eval-after-load 'org-superstar
  ;;   (set-face-attribute 'org-superstar-item nil :height 1.2)
  ;;   (set-face-attribute 'org-superstar-header-bullet nil :height 1.2)
  ;;   (set-face-attribute 'org-superstar-leading nil :height 1.3))
  ;; Set different bullets, with one getting a terminal fallback.
  ;; (setq org-superstar-headline-bullets-list
  ;;       '("◉" "◈" "○" "▷"))
  (setq org-superstar-headline-bullets-list
        '("①" "②" "③" "④" "⑤" "⑥" "⑦" "⑧" "⑨"))
  ;; ⎋〄
  ;; Stop cycling bullets to emphasize hierarchy of headlines.
  (setq org-superstar-cycle-headline-bullets nil)
  ;; Hide away leading stars on terminal.
  (setq org-superstar-leading-fallback ?\s)
  )


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
(use-package org-capture
  :config
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
  (add-to-list 'org-capture-templates
               '("p" "Plans" entry
                 (file+olp+datetree "~/Dropbox/Dreams/Org/Plans.org" "Plans")
                 "* TODO %T %^{Heading}\n  %^{EFFORT}p %?" :time-prompt t :tree-type week :empty-lines 1))

  )
;;; Paste Image From https://emacs-china.org/t/topic/6601/4
(defun org-insert-image ()
  "Insert a image from clipboard."
  (interactive)
  (let* ((path (concat default-directory
                       (buffer-name)
                       ".assets/"))
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

;;; from https://christiantietze.de/posts/2019/12/emacs-notifications/
(use-package appt
  :defer 10
  :config

  (setq
   appt-time-msg-list nil ;; clear existing appt list
   appt-display-interval '5 ;; warn every 5 minutes from t - appt-message-warning-time
   appt-message-warning-time '15 ;; send first warning 15 minutes before appointment
   appt-display-mode-line nil ;; don't show in the modeline
   appt-display-format 'window) ;; pass warnings to the designated window function
  (setq appt-disp-window-function (function ct/appt-display-native))

  (appt-activate 1) ;; activate appointment notification
                                        ; (display-time) ;; Clock in modeline

  (setq exec-path (append '("~/.emacs.d/3rd-parties/alerter/bin") exec-path))
  (defvar alerter-command (executable-find "alerter") "The path to alerter.")

  (defun ct/send-notification (title msg)
    "Send notification (TITLE MSG)."
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
    "Appt display native (MIN-TO-APP NEW-TIME MSG)."
    (ct/send-notification
     (format "Appointment in %s minutes" min-to-app) ; Title
     (format "%s" msg))) ; Message/detail text

  ;; Agenda-to-appointent hooks
  (org-agenda-to-appt) ;; generate the appt list from org agenda files on emacs launch
  (run-at-time "24:01" 3600 'org-agenda-to-appt) ;; update appt list hourly
  (add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt) ;; update appt list on agenda view
  )

(use-package ox-html
  :config
  ;; Org export code style
  (setq org-html-htmlize-output-type 'css)
  (setq-default org-html-doctype "html5")
  (setq-default org-html-html5-fancy t)
  )

(add-hook (quote org-mode-hook)
          (lambda ()
            (org-shifttab 5)))

;;; According to https://orgmode.org/manual/Hard-indentation.html#Hard-indentation
;;; But I don't need the odd levels only
(setq org-adapt-indentation t
      org-hide-leading-stars t)
      ;;org-odd-levels-only t)

;;; =========== OrgMode and Calendar ============
;;; https://www.pengmeiyu.com/blog/sync-org-mode-agenda-to-calendar-apps/
(use-package ox-icalendar
  :config
  (setq org-icalendar-alarm-time 5)
  (setq org-icalendar-combined-agenda-file "~/Dropbox/Dreams/Org/org.ics"
        org-icalendar-include-todo 'all
        org-icalendar-store-UID t
        org-icalendar-timezone ""
        org-icalendar-use-deadline
        '(event-if-not-todo event-if-todo event-if-todo-not-done todo-due)
        org-icalendar-use-scheduled
        '(event-if-not-todo event-if-todo event-if-todo-not-done todo-start))
  )
(use-package org-caldav
  :ensure t
  :after (async)
  :config
  (setq org-caldav-url "https://dong.mk/radicale/mkvoya/")
  (setq org-caldav-calendar-id "f846603c-c54c-c73f-f009-e7331ef16216")
  (setq org-caldav-inbox "~/Dropbox/Dreams/Org/Caldav.inbox.org")
  (setq org-caldav-files '("~/Dropbox/Dreams/Org/IPADS.sched.org"
                           "~/Dropbox/Dreams/Org/Main.org"
                           "~/Dropbox/Dreams/Org/Inbox.org"
                           ))
  ;; (setq org-icalendar-timezone "America/Los_Angeles")
  (setq org-icalendar-timezone "Asia/Shanghai")
  )
;;; ========= End of OrgMode and Calendar =============


;; ;; Two more extensions could be relavant.
;; ;; org-super-links
;; ;; org-wild-notifier.el
;; (use-package org-wild-notifier
;;   :ensure t
;;   :config
;;   (org-wild-notifier-mode t)
;;   ;;; Overwrite
;;   (defun org-wild-notifier--notify (event-msg)
;;     "Notify about an event using `alert' library.
;; EVENT-MSG is a string representation of the event."
;;     ;;(message "Here is the events %S" event-msg)
;;     ;;(message "%d" "ddd")
;;     ;;(if event-msg
;;      ;;   (progn
;;           (ct/send-notification org-wild-notifier-notification-title event-msg)
;;           ;; (alert event-msg :title org-wild-notifier-notification-title :severity org-wild-notifier--alert-severity))
;;           ;;)
;;           (message "No new events"))
;;   ;;)
;;   )


;;; Org Publish
(use-package ox-publish
  :config
  (setq org-publish-project-alist
        '(
          ("org-notes"
           :base-directory "~/Dropbox/Dreams/Org/Public"
           :base-extension "org"
           :publishing-directory "/Volumes/ramfs/public_html/"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4             ; Just the default for this project.
           :auto-preamble t
           )
          ("org-static"
           :base-directory "~/Dropbox/Dreams/Org/Public"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
           :publishing-directory "/Volumes/ramfs/public_html/"
           :recursive t
           :publishing-function org-publish-attachment
           )
          ("org" :components ("org-notes" "org-static"))
          )))


(use-package org-ref
  :ensure t
  :defer t
  :config
  (let* ((note-dir "~/Dropbox/Dreams/Research/Papers/Notes/")
         (note-file (concat note-dir "Notes.org"))
         (bib-file "~/Dropbox/Dreams/Research/Papers/Main.bib")
         (pdf-dir "~/Dropbox/Dreams/Research/Papers"))
    (setq reftex-default-bibliography bib-file)
    ))

(use-package org-roam
      :ensure t
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory "~/Dropbox/Dreams/Org/")
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))

;; ;; Allows you to edit entries directly from org-brain-visualize
;; (use-package polymode
;;   :config
;;   (add-hook 'org-brain-visualize-mode-hook #'org-brain-polymode))
;;
(use-package org-brain :ensure t
  :init
  (setq org-brain-path "~/Dropbox/Dreams/Org/Brain")
  ;; For Evil users
  (with-eval-after-load 'evil
    (evil-set-initial-state 'org-brain-visualize-mode 'emacs))
  :config
  (bind-key "C-c b" 'org-brain-prefix-map org-mode-map)
  (setq org-id-track-globally t)
  (setq org-id-locations-file "~/Dropbox/Dreams/Org/Brain/org-id-locations")
  (add-hook 'before-save-hook #'org-brain-ensure-ids-in-buffer)
  (push '("b" "Brain" plain (function org-brain-goto-end)
          "* %i%?" :empty-lines 1)
        org-capture-templates)
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 12)
  (setq org-brain-include-file-entries nil
        org-brain-file-entries-use-title nil))


(provide 'config-org)
;;; config-org.el ends here
