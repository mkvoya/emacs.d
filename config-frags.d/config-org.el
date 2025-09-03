;; -*- lexical-binding: t; -*-
;;; Emacs Config Fragement: Org Mode

;; | appt | MELPA, Appointment package |

;; * Org mode
(use-package org
  :after (bind-key)
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c a" . #'org-agenda)
         ("C-c c" . #'org-capture)
         ("C-c C-o" . #'org-open-maybe) ; Redefine file opening without clobbering universal argument
         )
  :hook ((org-insert-heading . mk/org-add-created-property) ; Add CREATED property when creating an entry
         (org-after-todo-state-change . mk/org-set-scheduled-today)
         (org-after-todo-state-change . mk/org-set-next-activation)
         (org-after-todo-state-change . mk/org-clocking-on-state-change)
         (org-mode . mk/org-syntax-table-modify) ; Modify syntax table
         (org-mode . mk/org-show-link-when-idle)
         (org-babel-after-execute . org-redisplay-inline-images) ; Always redisplay inline images after executing SRC block
  )
  :init
  ;; (setq org-modules-loaded t)

  ;; Hook functions

  (defun mk/org-add-created-property ()
    "Record creation time."
    (org-id-get-create)
    (org-set-property "CREATED" (format-time-string (org-time-stamp-format t t))))

  (defun mk/org-set-scheduled-today ()
    "Add today as the default scheduled date when turning into TODO."
    (when (and (string= org-state "TODO")
               (not (org-entry-get nil "SCHEDULED")))
      (org-schedule nil (format-time-string "%Y-%m-%d"))))

  (defun mk/org-set-next-activation ()
    (when (and (string= (org-get-todo-state) "NEXT")
               (not (org-entry-get nil "ACTIVATED")))
      (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))

  (defun mk/org-syntax-table-modify ()
    "Modify `org-mode-syntax-table' for the current org buffer."
    (modify-syntax-entry ?< "." org-mode-syntax-table)
    (modify-syntax-entry ?> "." org-mode-syntax-table))

  (defun mk/org-show-link-when-idle()
    "在echo area中显示链接详情"
    (require 'help-at-pt)
    (setq help-at-pt-display-when-idle t) ;; 不会立即生效
    (setq help-at-pt-timer-delay 0.5)
    (help-at-pt-set-timer)) ;; 调用才会生效

  ;; https://stackoverflow.com/questions/17590784/how-to-let-org-mode-open-a-link-like-file-file-org-in-current-window-inste
  (defun org-force-open-current-window ()
    "Open at current window."
    (interactive)
    (let ((org-link-frame-setup (quote
                                 ((vm . vm-visit-folder)
                                  (vm-imap . vm-visit-imap-folder)
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

  (defun mk/org-clocking-on-state-change ()
    "Auto clock-in/-out when state changes."
    (unless (string= org-last-state org-state)
      (cond
       ((string= org-state "ONGOING")
        (org-clock-in))
       ((string= org-last-state "ONGOING")
        (org-clock-out)
        ))))

  :config

  ;; simple use-packages
  ;; don't remember these packages, re-enable if necessary.
  ;; (use-package org-contrib :ensure (:host github :repo "emacsmirror/org-contrib"))
  ;; (use-package org-inline-pdf :defer t)

  (add-to-list 'org-modules 'org-protocol)
  (require 'org-protocol)


  ;; (setq org-latex-create-formula-image-program 'dvisvgm)
  ;; According to https://orgmode.org/manual/Hard-indentation.html#Hard-indentation
  ;; But I don't need the odd levels only
  (setq org-adapt-indentation t)
  (setq org-hide-leading-stars t)
  (setq org-startup-indented t) ; disable org-indent-mode for org-margin
  (setq org-latex-create-formula-image-program 'dvisvgm)

  ;; (setq org-latex-create-formula-image-program 'dvipng)
  (setq org-support-shift-select t)  ; Use shift to select region when possible.
  (setq org-clock-idle-time 10)  ; Clock will prompt to stop after 10 min of idle.
  (setq org-element-use-cache nil)  ; cache sometimes causes problems


   ;; Edit settings
  (setq org-auto-align-tags nil)
  (setq org-tags-column 0)
  (setq org-catch-invisible-edits 'show-and-error)
  (setq org-special-ctrl-a/e t)
  (setq org-insert-heading-respect-content t)
   ;; Org styling, hide markup etc.
  (setq org-hide-emphasis-markers t)
  (setq org-pretty-entities t)

  ;; configs
  (setq org-directory "~/Dropbox/Dreams/Org/")
  ;; (setq org-display-remote-inline-images 'download)

  ;; Org mode TODO states
  (setq org-todo-keywords
        '((sequence
           "TODO(t!)" "NEXT(n!)" "TRACK(o!)" "ROUTINE(r)" "HOLD(h!/!)" "IDEA(I)"
           "PROJECT(p)" "CONFDDL(C)"
           "|"
           "ACCEPTED(a@)" "DONE(d!/!)" "CANCELED(c@)"
           )))
  ;; Keyword colors
  (setf org-todo-keyword-faces
        '(("TODO" . (:foreground "#dfffff" :background "#ff19a3" :weight bold))
          ("NEXT"  . "orangered")
          ("ACCEPTED"  . "darkgreen")
          ("HOLD" . "pink")
          ("CANCELED" . (:foreground "white" :background "#4d4d4d"))
          ("DONE" . (:foreground "#008080"))
          ))
  (setq org-log-into-drawer t)
  (setq org-log-done 'time) ;; Auto add DONE TIME
  (setq org-ellipsis "↴")
  (set-face-attribute 'org-ellipsis nil :foreground "grey86")


  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)
     (C . t)
     (python . t)
     (shell . t)))

  ;; * Org Babel
  (defun org-babel-C-execute/filter-args (args)
    (when-let* ((params (cadr args))
                (stdin (cdr (assoc :stdin params)))
                (res (org-babel-ref-resolve stdin))
                (stdin (org-babel-temp-file "c-stdin-")))
      (with-temp-file stdin (insert res))
      (let* ((cmdline (assoc :cmdline params))
             (cmdline-val (or (cdr cmdline) "")))
        (when cmdline (setq params (delq cmdline params)))
        (setq params
              (cons (cons :cmdline (concat cmdline-val " <" stdin))
                    params))
        (setf (cadr args) params)))
    args)

  (with-eval-after-load 'ob-C
    (advice-add 'org-babel-C-execute :filter-args
                #'org-babel-C-execute/filter-args))


  (require 'color)

  (set-face-attribute 'org-block nil :background (color-darken-name (face-attribute 'default :background) 3))
  (set-face-attribute 'org-code nil :background (color-darken-name (face-attribute 'default :background) 3))
  (set-face-attribute 'org-quote nil :background (color-darken-name (face-attribute 'default :background) 3))
  (set-face-attribute 'org-block-begin-line nil :background "#F1E6F8")
  (set-face-attribute 'org-block-end-line nil :background (color-darken-name (face-attribute 'default :background) 4))
  (set-face-attribute 'outline-1 nil :foreground "firebrick")
  (set-face-attribute 'org-level-1 nil :height 1.1)
  (set-face-attribute 'outline-2 nil :foreground "purple2")
  (set-face-attribute 'outline-3 nil :foreground "violetRed2")
  (set-face-attribute 'outline-4 nil :foreground "cyan4")
  ;; (set-face-attribute 'outline-4 nil :foreground "springgreen4")

  (setq org-fontify-quote-and-verse-blocks t)

  (add-hook 'org-mode-hook
            (lambda ()
              (electric-indent-local-mode -1)
              (mkvoya/better-wrap)
              (prettify-symbols-mode)
              ;; (org-hide-properties)
              ))


  (setq org-hide-emphasis-markers nil)      ; don’t hide markers for like *foo*
  ;; (setq org-hide-emphasis-markers t)
  (setq org-emphasis-alist
        '(("*" bold)
          ("/" italic)
          ("_" underline)
          ("=" org-verbatim verbatim)
          ;; ("@" (:foreground "red" :background "black"))
          ("&" (:foreground "red"))
          ("~" org-code verbatim)
          ("+"
           (:strike-through t))))
  (use-package ov)
  (load-file "~/.emacs.d/site-lisp/org-colored-text.el")
  )

(use-package org-super-agenda
  :after (org)
  :init (org-super-agenda-mode)
  :config
  (setq org-super-agenda-groups
        '((:name "Next Items"
                 :time-grid t
                 :tag ("NEXT" "outbox"))
          (:name "Important"
                 :priority "A")
          (:name "Quick Picks"
                 :effort< "0:30")
          (:priority<= "B"
                       :scheduled future
                       :order 1)))
  (setq org-agenda-custom-commands
        '(("z" "Super Agenda"
           ((agenda "" ((org-agenda-span 'day)
                        (org-super-agenda-groups
                         '((:name "Today" :time-grid t :date today :scheduled today :order 1)))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:log t)
                            (:name "What's Next" :todo "NEXT" :time-grid t :order 1)
                            (:name "Important" :priority "A" :order 6)
                            (:name "Due Today" :deadline today :order 2)
                            (:name "Scheduled Soon" :scheduled future :order 8)
                            (:name "Due Soon" :deadline future :order 9)
                            (:name "Overdue" :deadline past :order 7)
                            (:name "Projects" :tag "Project" :order 14)
                            ))))))))
  )

(use-package org-modern :ensure (:type git :host github :repo "minad/org-modern")
  :after (org)
  :demand
  :config
  ;; (set-face-attribute 'org-modern-symbol nil :family "Iosevka")
  (set-face-attribute 'org-modern-todo nil :height 1)
  (set-face-attribute 'org-modern-todo nil :weight 'light)
  ;; org-modern-tag
  (custom-set-faces '(org-modern-tag
                      ((((background light)) :foreground "black" :background "#f4f4f4")
                       (((background dark))  :foreground "white" :background "#222222"))))
  (with-eval-after-load 'org (global-org-modern-mode)))

(use-package org-sticky-header :ensure (:host github :repo "alphapapa/org-sticky-header")
  :after (org)
  )
;; Org Cite
(use-package oc
  :ensure nil
  :after org)


(use-package websocket :defer t)
(use-package simple-httpd :defer t)

(use-package org-bars :ensure (:host github :repo "tonyaldon/org-bars")
  :after (org)
  :config (setq org-bars-with-dynamic-stars-p nil))


(use-package org-tag-beautify
  :disabled t
  :custom (org-tag-beautify-data-dir "~/.emacs.d/ensure/repos/org-tag-beautify/data/")
  :init (org-tag-beautify-mode 1))

(use-package org-rainbow-tags
  :custom
  (org-rainbow-tags-hash-start-index 0)
  ;; (org-rainbow-tags-extra-face-attributes '(:inverse-video t :box t :weight 'bold))
  (org-rainbow-tags-extra-face-attributes '(:weight 'bold))
  :hook (org-mode . org-rainbow-tags-mode))

;; agenda 里面时间块彩色显示
;; From: https://emacs-china.org/t/org-agenda/8679/3
(defun my:org-agenda-time-grid-spacing ()
  "Set different line spacing w.r.t. time duration."
  (save-excursion
    (let* ((background (alist-get 'background-mode (frame-parameters)))
           (background-dark-p (string= background "dark"))
           (colors (if background-dark-p
                       (list "#aa557f" "DarkGreen" "DarkSlateGray" "DarkSlateBlue")
                     (list "#F6B1C3" "#FFCF9D" "#BEEB9F" "#ADD5F7")))
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

(add-hook 'org-agenda-finalize-hook #'my:org-agenda-time-grid-spacing)
(setq org-agenda-start-with-log-mode t)

(font-lock-add-keywords
 'org-mode
 '(("^[[:space:]]*\\(-\\) "
    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))


(use-package org-transclusion
  :ensure (:host github :repo "nobiot/org-transclusion" :files ("*.el"))
  :after (org org-modern)
  :hook (org-mode . org-transclusion-mode)
  :config
  ;; (setq org-transclusion-fringe-bitmap 'empty-line)
  (setq org-transclusion-fringe-bitmap 'right-triangle)
  (set-face-attribute
   'org-transclusion-fringe nil
   :foreground "blue"
   :background "orange")
  (set-face-attribute
   'org-transclusion-source-fringe nil
   :foreground "lightblue"
   :background "blue")
  (require 'org-transclusion-indent-mode)
  )
;; :bind (("<f12>" . #'org-transclusion-add))
;; ("C-n t" . #'org-transclusion-mode)


(use-package org-capture
  :ensure nil ; org built-in
  :after (org)
  :config
  (defun mk/org-capture-people ()
    (interactive)
    (format "* %s\n%%?" (read-string "姓名: " nil))
    )
  (setq org-capture-templates
        '(("p" "New People" entry (file+headline "~/Dropbox/Dreams/Org/People/General.org" "People")
           #'mk/org-capture-people
           )
          ("r" "New Research Snippet" entry (file+headline "~/Dropbox/Dreams/Research/Snippets.org" "Research Snippets")
           "* %?\n%i %a"
           )))
  )

(use-package org-journal
  :ensure t
  :after (org)
  :bind (("C-c j" . org-journal-open-current-journal-file)
         ("C-c J" . org-journal-new-entry)
         ("C-c s"  . org-journal-search-forever)
         ("C-c C-s"  . nil) ; unbind
         )
  :config
  (setq org-journal-file-format "%Y-%m-%d-w%V.org")
  (setq org-journal-enable-agenda-integration t)
  (setq org-journal-file-type 'weekly)
  (setq org-journal-dir "~/Dropbox/Dreams/Org/Journals/"
        org-journal-date-format "%A, %d %B %Y")

  ;; org capture integration
  (defun org-journal-find-location ()
    ;; Open today's journal, but specify a non-nil prefix argument in order to
    ;; inhibit inserting the heading; org-capture will insert the heading.
    (org-journal-new-entry t)
    (unless (eq org-journal-file-type 'daily)
      (org-narrow-to-subtree))
    (goto-char (point-max)))
  (defvar org-journal--date-location-scheduled-time nil)

  (defun org-journal-date-location (&optional scheduled-time)
    (let ((scheduled-time (or scheduled-time (org-read-date nil nil nil "Date:"))))
      (setq org-journal--date-location-scheduled-time scheduled-time)
      (org-journal-new-entry t (org-time-string-to-time scheduled-time))
      (unless (eq org-journal-file-type 'daily)
        (org-narrow-to-subtree))
      (goto-char (point-max))))

  (defun transform-square-brackets-to-round-ones(string-to-transform)
    "Transforms [ into ( and ] into ), other chars left unchanged."
    (concat
     (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform))
    )
  (setq org-capture-templates
        `(("i" "Inbox" entry  (file "inbox.org")
           ,(concat "* TODO %?\n"
                    "/Entered on/ %U"))
          ("j" "Journal entry" plain (function org-journal-date-location)
           "** TODO %?\n <%(princ org-journal--date-location-scheduled-time)>\n"
           :jump-to-captured t)
          ("J" "Journal entry" plain (function org-journal-find-location)
           "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?"
           :jump-to-captured t :immediate-finish t)
          ("c" "Captured" entry (file "capture.org")
           "* %t %:description\nlink: %l \n\n%i\n" :prepend t :empty-lines-after 1)
          ("n" "Captured Now!" entry (file "capture.org")
           "* %t %:description\nlink: %l \n\n%i\n" :prepend t :emptry-lines-after 1 :immediate-finish t)
          ("p" "Protocol" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
           "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
          ("L" "Protocol Link" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
           "* %? [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n")
          ("w" "Web site" entry
           (file "webpages.org")
           "* %a :website:\n\n%U %?\n\n%:initial")
          )
        )
  )


(use-package consult-notes
  :ensure (:type git :host github :repo "mclear-tools/consult-notes")
  :commands (consult-notes
             consult-notes-search-in-all-notes)
  :bind ("C-c d f" . consult-notes)
  :config
  (setq consult-notes-sources
        '(("denote"          ?d "~/Dropbox/Dreams/Org")
          ("People"          ?d "~/Dropbox/Dreams/Org/People")
          ))
  )



(use-package ox-html
  :ensure nil ; org built-in
  :after (org)
  :defer t
  :config
  ;; Org export code style
  (setq org-html-htmlize-output-type 'css)
  (setq org-src-preserve-indentation nil)
  (setq-default org-html-doctype "html5")
  (setq-default org-html-html5-fancy t)
  (setq org-html-validation-link nil)

  ;; https://emacs.stackexchange.com/a/3512/30542
  (defun my-org-inline-css-hook (exporter)
    "Insert custom inline css"
    (when (eq exporter 'html)
      (let* ((dir (ignore-errors (file-name-directory (buffer-file-name))))
             (path (concat dir "style.css"))
             (homestyle (or (null dir) (null (file-exists-p path))))
             (final (if homestyle "~/.emacs.d/misc/ox-html-code-style.css" path)))
        (setq org-html-head-include-default-style t)
        (setq org-html-head (concat
                             "<style type=\"text/css\">\n"
                             "<!--/*--><![CDATA[/*><!--*/\n"
                             (with-temp-buffer
                               (insert-file-contents final)
                               (buffer-string))
                             "/*]]>*/-->\n"
                             "</style>\n")))))

  (add-hook 'org-export-before-processing-hook 'my-org-inline-css-hook)

    ;;; Add summary support, from Sachachua
  (setq org-babel-exp-code-template "#+begin_src %lang%switches%flags :summary %summary\n%body\n#+end_src")
  (defun my-org-html-src-block (src-block _contents info)
    (let* ((result (org-html-src-block src-block _contents info))
           (block-info
            (org-with-point-at (org-element-property :begin src-block)
              (org-babel-get-src-block-info)))
           (summary (assoc-default :summary (elt block-info 2))))
      (if (member summary '("%summary" ""))
          result
        (format "<details><summary>%s</summary>%s</details>"
                summary
                result))))
  (with-eval-after-load 'ox-html
    (map-put!
     (org-export-backend-transcoders (org-export-get-backend 'html))
     'src-block 'my-org-html-src-block))
  )



(use-package ox-twbs
  :after ox-html
  :defer t
  :config
  (defun my-org-html-src-block2 (src-block _contents info)
    (let* ((result (org-twbs-src-block src-block _contents info))
           (block-info
            (org-with-point-at (org-element-property :begin src-block)
              (org-babel-get-src-block-info)))
           (summary (assoc-default :summary (elt block-info 2))))
      (if (member summary '("%summary" ""))
          result
        (format "<details><summary>%s</summary>%s</details>"
                summary
                result))))
  (with-eval-after-load 'ox-twbs
    (map-put!
     (org-export-backend-transcoders (org-export-get-backend 'twbs))
     'src-block 'my-org-html-src-block2))
  )

;; Paste Image From https://emacs-china.org/t/topic/6601/4
(defun org-insert-image ()
  "Insert a image from clipboard."
  (interactive)
  (let* ((buf-name (if (and (fboundp 'denote-file-is-note-p)
                            (fboundp 'denote-retrieve-filename-identifier)
                            (denote-file-is-note-p (buffer-file-name)))
                       (denote-retrieve-filename-identifier (buffer-name))
                     (buffer-name)))
         (insert-image-pos (second (assoc "INSERT_IMAGE_POS" (org-collect-keywords '("insert_image_pos")))))
         (path (cond ((equal insert-image-pos "default-directory") ; default-directory
                      default-directory)
                     ((and insert-image-pos (not (equal insert-image-pos "nil"))) ; user-specified
                      insert-image-pos)
                     (t (concat default-directory ; default
                                buf-name
                                ".assets/"))))
         (image-file (concat
                      path
                      buf-name
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

(use-package ox-pandoc
  :defer t)

(use-package org-timeline
  :after org
  :hook (org-agenda-finalize . org-timeline-insert-timeline))


(defun mk/org-archive-to-specified-file ()
  "Archive the current org entry to a user-specified file."
  (interactive)
  (let ((file (read-file-name "Archive to file: ")))
    (let ((org-archive-location (concat file "::")))
      (org-archive-subtree))
    (message "Archived to %s" file)))


(use-package ox-hugo
  :defer t
  :after ox
  :init
  (defun mk/sync-to-server (&optional all-subtrees async visible-only noerror)
    (async-shell-command "cd ~/Dropbox/Public/Blog2025 && hugo -D && rsync -rvP ~/Dropbox/Public/Blog2025/public/ dong.mk:/srv/http/blog"))
  (advice-add 'org-hugo-export-wim-to-md :after #'mk/sync-to-server)
  :config

  ;; Populates only the EXPORT_FILE_NAME property in the inserted heading.
  (with-eval-after-load 'org-capture
    (defun org-hugo-new-subtree-post-capture-template ()
      "Returns `org-capture' template string for new Hugo post.
          See `org-capture-templates' for more information."
      (let* ((date (format-time-string (org-time-stamp-format :long :inactive) (org-current-time)))
             (title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
             (fname (org-hugo-slug title)))
        (mapconcat #'identity
                   `(
                     ,(concat "* " title)
                     ":PROPERTIES:"
                     ,(concat ":ID:       " (org-id-new))
                     ,(concat ":CREATED:  " (format-time-string (org-time-stamp-format t t)))
                     ,(concat ":EXPORT_FILE_NAME: " fname)
                     ,(concat ":EXPORT_DATE: " date)
                     ":END:"
                     "%?\n")          ;Place the cursor here finally
                   "\n")))

    (add-to-list 'org-capture-templates
                 '("h"                ;`org-capture' binding + h
                   "Hugo post"
                   entry
                   (file "~/Dropbox/Dreams/Org/Blog/all-posts.org")
                   (function org-hugo-new-subtree-post-capture-template))))
  (defun mk/org-new-post ()
    (interactive)
    (find-file "~/Dropbox/Dreams/Org/Blog/all-posts.org")
    (let ((content (org-hugo-new-subtree-post-capture-template)))
      (goto-char (point-max))
      (insert "\n")
      (insert content)
      )
    )
  )


(setq org-modern-fold-stars
      '(("◉" . "○") ("◈" . "◇") ("⯈" . "⯆") ("◉" . "○") ("◈" . "◇")))

(use-package emacs
  :ensure nil
  ;; :disabled t
  :after (org org-ql)
  :init
  (load-file "~/.emacs.d/site-lisp/org-timeliner.el")
  ;; (setq-default header-line-format
  ;;               '("GC: " (:eval (number-to-string gcs-done)) " - " (:eval (format "%.3f" gc-elapsed)) "s"
  ;;                 " " (:eval (timeliner-get-string))))
  (setq-default header-line-format
                '("" (:eval (timeliner-get-string))))

  (defun my-set-header-line-height-px (pixels)
    "Set the `header-line` face height to exactly PIXELS in current frame.
         PIXELS is the desired height in physical pixels."
    (let* ((char-px (frame-char-height)) ; 当前字体一个字符的像素高度
           ;; Emacs face :height 100 = 默认字体大小
           ;; 计算比例（百分比）
           (percent (truncate (* 100 (/ (float pixels) char-px)))))
      (set-face-attribute 'header-line nil
                          :height percent
                          :foreground nil
                          :background nil
                          :box nil)
      (set-face-attribute 'header-line-inactive nil
                          :foreground nil
                          :background nil
                          :height percent
                          :box nil
                          :underline nil)))

  ;; 用法示例：把 header-line 高度设为 20 像素
  ;; (add-hook 'elpaca-after-init-hook (lambda ()
  ;;                                     (my-set-header-line-height-px 3)))
  )

(use-package org-timeblock
  :ensure (:type git :host github :repo "ichernyshovvv/org-timeblock")
  :after (org)
  :defer t)

(use-package org-analyzer
  :after (org)
  :defer t)

(use-package nov :defer t)
(use-package djvu :defer t)
(use-package org-noter
  :ensure (:host github :repo "org-noter/org-noter" :files ("*.el" "modules/*.el"))
  :after (org nov djvu)
  :defer t)

(defun download-webpage-and-rename (url)
  "Download webpage from URL and rename the file to the webpage title."
  (interactive "sEnter URL: ")
  (url-retrieve
   url
   (lambda (status)
     (if (plist-get status :error)
         (message "Failed to retrieve URL: %s" (plist-get status :error))
       (goto-char (point-min))
       (re-search-forward "<title>\\(.*?\\)</title>" nil t)
       (let ((title (match-string 1))
             (filename (concat (make-temp-file "webpage-" nil ".html"))))
         (write-region (point-min) (point-max) filename)
         (rename-file filename (concat title ".html") t)
         (message "Downloaded and saved as %s.html" title))))))

(use-package org-web-tools
  :defer t)


(defun how-many-str (regexp str)
  (cl-loop with start = 0
           for count from 0
           while (string-match regexp str start)
           do (setq start (match-end 0))
           finally return count))
(defun how-many-checked (str)
  (+ (how-many-str "\\[[^[:blank:]]\\]" str)
     (how-many-str "([^[:blank:]])" str)))
(defun how-many-unchecked (str)
  (+ (how-many-str "\\[ \\]" str)
     (how-many-str "( )" str)))
(defun mk/org-columns--summary-pomodoro-count (check-boxes _)
  "Summarize pomodoro CHECK-BOXES with a check-box cookie."
  (let ((checked (cl-reduce '+ (cl-mapcar #'how-many-checked check-boxes)))
        (unchecked (cl-reduce '+ (cl-mapcar #'how-many-unchecked check-boxes))))
    (format "[%d/%d]" checked (+ checked unchecked))))
(setq org-columns-summary-types
      '(("P/" . mk/org-columns--summary-pomodoro-count))
      )

(use-package define-word :disabled t)

(use-package org-download :defer t)
(use-package org-sidebar :defer t)

(provide 'config-org)
