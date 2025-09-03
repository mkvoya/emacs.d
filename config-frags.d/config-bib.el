;; -*- lexical-binding: t; -*-
;;; Emacs Config Fragement: Bibliography

;; * Common Stuff
(setq mk/bib-main-file "~/Dropbox/Dreams/Research/Papers/Papers.bib")
(setq mk/bib-pdf-dir "~/Dropbox/Dreams/Research/Papers/")
(setq mk/ebib-dir-root "~/Dropbox/Dreams/Research/Papers")

(setq reftex-default-bibliography `("paper.bib" "references.bib" ,mk/bib-main-file))
;; Automatically choose the file to link with according to the selected text.
(defvar autolink-directory "~/Dropbox/Dreams")

;; My source code for bib
(setq paper-root-dir (expand-file-name "~/Dropbox/Dreams/Research/Papers"))
(defun mk/normalize-paper-title (title)
  "Remove bad chars in the paper TITLE."
  (replace-regexp-in-string
   "[\s\n]+" " " (replace-regexp-in-string
                  "/" "" (replace-regexp-in-string
                          ":" "," title))))

(defun paper-root()
  "Open the paper root."
  (interactive)
  (find-file paper-root-dir))

(defun paper-find (&optional initial)
  "Search a paper in your Dreamland, by title, with INITIAL input."
  (interactive "P")
  (let ((consult-find-args (concat
                            (expand-file-name "~/.emacs.d/bin/paperfind.sh")
                            " "
                            paper-root-dir)))
    (find-file (concat (file-name-as-directory paper-root-dir)
                           (consult--find "Dreamland's Paper Find: "
                                          #'consult--find-builder initial)))))


(defun paper-open ()
  "Open the file in PDF Expert. Code borrowed from the crux package."
  (interactive)
  (let ((current-file-name
         (if (eq major-mode 'dired-mode)
             (dired-get-file-for-visit)
           buffer-file-name)))
    (call-process "open" nil 0 nil "-a" "/Applications/PDF Expert.app" current-file-name))
  )

(defun autolink--get-candidates (text)
  "Search for the file name with TEXT."
  (let* ((cmd (concat "find " autolink-directory " -iname \"*" (string-replace ":" "?" text) "*\""))
         (candidates (mapcar 'abbreviate-file-name (delete "" (split-string (shell-command-to-string cmd) "\n")))))
    (completing-read "Choose the one to link: " candidates)))

(defun paper-link (start end)
  "Try to guess the file to link according to the region between START and END."
  (interactive "r") ; The "r" here will fill the start and end automatically.
  (let* ((text (buffer-substring start end))
         (file (autolink--get-candidates text)))
    (goto-char end)
    (insert "]]")
    (goto-char start)
    (insert (concat "[[" file "]["))))

(defun mk/ebib--format-full-dir (dir title)
  "Get the full dir name from DIR and TITLE."
  (let ((clean-title
         (string-replace "}" "" (string-replace "{" "" title))))
    (format "%s/%s/%s" mk/ebib-dir-root dir clean-title)))

(defun mk/ebib-get-series-dirname-candidate (title series journal year publisher)
  "Form the name of given TITLE, SERIES, JOURNAL, YEAR, PUBLISHER"
  (cond
   ;; Use series if set
   ((not (string= series "no-series")) (string-replace " " "." (string-replace " '" "" series)))
   ;; Hard coded
   ((string= journal "scientific reports") (format "nat.sci.rep.%s" year))
   ((string= journal "science") (format "science.%s" year))
   ((string= journal "commun. acm") (format "commun.acm.%s" year))
   ((string= journal "nature communications") (format "nat.comm.%s" year))
   ((string= journal "nature") (format "nature.%s" year))
   ((string= journal "advanced science") (format "advs.%s" year))
   ((string= journal "acs nano") (format "acs.nano.%s" year))
   ((string= journal "nano select") (format "nano.%s" year))
   ((string= journal "bmc bioinformatics") (format "bioinfo.%s" year))
   ((string= journal "ieee transactions on computer-aided design of integrated circuits and systems") (format "tcad.%s" year))
   ((string= journal "ieee transactions on parallel and distributed systems") (format "tpds.%s" year))
   ((string= journal "ieee transactions on computers") (format "tc.%s" year))
   ((string= journal "ieee transactions on information theory") (format "tit.%s" year))
   ((string= journal "briefings in bioinformatics") (format "bib.%s" year))
   ((string= journal "acs synthetic biology") (format "acs.syn.bio.%s" year))
   ((string= journal "angewandte chemie international edition") (format "anie.%s" year))
   ((string= journal "computational and structural biotechnology journal") (format "csbj.%s" year))
   ((string= journal "the australian universities' review") (format "aur.%s" year))
   ((string= journal "nature computational science") (format "nat.comput.sci.%s" year))
   ((string= journal "nature reviews genetics") (format "nat.rev.genet.%s" year))
   ;; arXiv
   ((string= publisher "arxiv") (format "arxiv%s" year))
   (t "XXXX")))

(defun mk/ebib-get-dir (title series journal year publisher)
  "Calculate the directory for TITLE and SERIES and JOURNAL and PUBLISHER."
  (let* ((series-dir (mk/ebib-get-series-dirname-candidate title series journal year publisher))
         (fulldir (mk/ebib--format-full-dir series-dir title)))
    (if (file-exists-p fulldir) fulldir nil)
    ))

(defun mk/ebib-open-dir (key)
  "Open the directory for KEY."
  (interactive (list (ebib--get-key-at-point)))
  (ebib--execute-when
   (entries
    (let* ((title (mk/normalize-paper-title (ebib-get-field-value "title" key ebib--cur-db nil t)))
           (series (downcase (ebib-get-field-value "series" key ebib--cur-db "no-series" t)))
           (publisher (downcase (ebib-get-field-value "publisher" key ebib--cur-db "no-publisher" t)))
           (journal (downcase (ebib-get-field-value "journal" key ebib--cur-db "no-journal" t)))
           (year (ebib-get-field-value "year" key ebib--cur-db "0000" t))
           (series-dirname (mk/ebib-get-series-dirname-candidate title series journal year publisher))
           (cand (mk/ebib-get-dir title series journal year publisher)))
      (if cand (find-file cand)
        (print "No such dir, creating with prompt...")
        (let* ((conf (string-trim (read-string "The conf abbr: " series-dirname)))
               (target (mk/ebib--format-full-dir conf title)))
          (make-directory target t)
          (find-file target)
          ))))
   (default
    (beep))))

(defun mk/paper-get-dirs ()
  "Get all conf dirs."
  (let* ((cmd (format "find %s -type d -maxdepth 1 -exec realpath --relative-to %s {} \\;" mk/ebib-dir-root mk/ebib-dir-root))
         (candidates (mapcar 'abbreviate-file-name (delete "" (split-string (shell-command-to-string cmd) "\n"))))
         (choice (completing-read "Choose the one to link: " candidates)))
    (print choice)
    ))

(defun mk/ebib-open-file (key)
  "Open files for KEY."
  (interactive (list (ebib--get-key-at-point)))
  (ebib--execute-when
   (entries
    (let* ((title (mk/normalize-paper-title (ebib-get-field-value "title" key ebib--cur-db nil t)))
           (cmd (format "find %s -type d -iname \"%s\"" mk/ebib-dir-root title))
           (candidates (mapcar 'abbreviate-file-name (delete "" (split-string (shell-command-to-string cmd) "\n"))))
           (cand (cond
                  ((= 0 (length candidates)) nil)
                  ((= 1 (length candidates)) (car candidates))
                  (t (completing-read "Choose the one to link: " candidates)))))
      (if cand (find-file cand)
        ())))
   (default
    (beep))))

;; * Bibtex

(use-package bibtex
  :ensure nil  ; built in
  :defer
  :init
  (setq bibtex-dialect 'biblatex)
  ;; Bibtex autokey is used by Ebib.
  (setq bibtex-autokey-year-length 4  ; Full year format
        bibtex-autokey-name-year-separator "-"
        bibtex-autokey-year-title-separator "-"
        bibtex-autokey-titleword-separator "-"
        bibtex-autokey-titlewords 2  ; Use two words from the title
        bibtex-autokey-titlewords-stretch 0
        bibtex-autokey-titleword-length nil  ; Use whole word
        )
  (setq bibtex-autokey-titleword-ignore
        '("A" "An" "On" "The" "Eine?" "Der" "Die" "Das"
          "The" "on"
          "a" "an"
          "and" "the" "of" ".*[^[:upper:][:lower:]0-9].*"))

  (setq bibtex-completion-bibliography `(,mk/bib-main-file)
        bibtex-completion-library-path nil  ; TODO
        bibtex-completion-notes-path nil)  ; TODO
  (setq bibtex-completion-notes-template-multiple-files
        "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"
        bibtex-completion-additional-search-fields '(keywords)
        bibtex-completion-display-formats
        '((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
          (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
          (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
          (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
          (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}")))
  (setq bibtex-completion-pdf-open-function
        (lambda (fpath) (call-process "open" nil 0 nil fpath)))
  :config
  )
(use-package bibtex-completion
  :defer
  :after (bibtex)
  :config
  (bibtex-completion-init)  ; This will set the XXX-format-internal variable
  )


;; * citar
(use-package citar
  :ensure (:host github :repo "bdarcus/citar")
  :defer t
  :bind (;; ("C-c B" . citar-insert-citation)
         ;; :map minibuffer-local-map
         ;; ("M-b" . citar-insert-preset)
         )
  :init
  (setq citar-notes-paths '("~/Dropbox/Dreams/Org/PaperNotes"))
  (setq org-cite-global-bibliography `(,(expand-file-name mk/bib-main-file)))
  (setq org-cite-insert-processor 'citar)
  (setq org-cite-follow-processor 'citar)
  (setq org-cite-activate-processor 'citar)
  (setq citar-bibliography org-cite-global-bibliography)
  (setq citar-symbol-separator "  ")
  ;; (require 'embark)
  ;; (setq citar-at-point-function 'embark-act)
  ;; (use-package citar-embark
  ;;   :after citar embark
  ;;   :no-require
  ;;   :config (citar-embark-mode))
  :config
  ;; (use-package org-roam-bibtex
  ;;   :defer
  ;;   :after org-roam
  ;;   :config
  ;;   (setq orb-roam-ref-format 'org-cite)
  ;;   (setq orb-use-bibdesk-attachments 't)
  ;;   )
  ;; (require 'org-roam-bibtex)
  (setq citar-open-note-function #'(lambda (key entry) (orb-edit-note key)))
  ;; (citar-filenotify-setup '(LaTeX-mode-hook org-mode-hook))
  )


;; * biblio: |Lookup & import bib |
(use-package biblio
  :init
  (setq biblio-arxiv-bibtex-header "misc")
  (setq biblio-bibtex-use-autokey nil)  ; Don't use autokey of biblio
  (setq biblio-synchronous t) ; Some backends fail upon async queries.
  :defer t
  :config
  ;; Override
  (defun biblio--completing-read-function ()
    "Override to always return the defualt one"
    completing-read-function)
  ;; Override to add url
  (defun biblio-arxiv--build-bibtex-1 (metadata)
    "Create an unformated BibTeX record for METADATA."
    (let-alist metadata
      (format "@%s{NO_KEY,
  author = {%s},
  title = {%s},
  year = {%s},
  url = {%s},
  series = {arXiv %s},
  archivePrefix = {arXiv},
  eprint = {%s},
  primaryClass = {%s}}"
              biblio-arxiv-bibtex-header
              (biblio-join-1 " AND " .authors)
              .title .year .url .year .identifier .category)))
  )

;; * ebib: |bib manager
(use-package ebib
  :after (biblio bibtex citar)
  :defer t
  :bind (:map ebib-index-mode-map ("O" . #'mk/ebib-open-dir))
  :init
  (defun mk/ebib-create-org-schedule (_key _db)
    (format "SCHEDULED: <%s>" (org-read-date nil nil "+1d")))
  (setq ebib-reading-list-template-specifiers '((?K . ebib-reading-list-create-org-identifier)
                                                (?T . ebib-create-org-title)
                                                (?M . ebib-reading-list-todo-marker)
                                                (?L . ebib-create-org-link)
                                                (?F . ebib-create-org-file-link)
                                                (?D . ebib-create-org-doi-link)
                                                (?U . ebib-create-org-url-link)
                                                (?S . mk/ebib-create-org-schedule)))
  (setq ebib-reading-list-template "* %M %T\n%S\n:PROPERTIES:\n%K\n:END:\n%F\n")
  (setq ebib-autogenerate-keys t)  ; Use bibtex autokey.
  (setq ebib-uniquify-keys t)
  (setq ebib-file-associations '(("pdf" . "open -a \"PDF Expert\" %s")))
  (setq ebib-bibtex-dialect 'biblatex)  ; biblatex is better than xxx.
  (setq ebib-index-window-size 10)
  (setq ebib-preload-bib-files `(,mk/bib-main-file))
  (setq ebib-file-search-dirs `(,mk/bib-pdf-dir))
  (setq ebib-notes-storage 'one-file-per-note)
  (setq ebib-reading-list-file "~/Dropbox/Dreams/Org/Ebib-ReadingList.org")
  (setq ebib-notes-directory "~/Dropbox/Dreams/Org/PaperNotes/")
  (setq ebib-notes-locations `(,ebib-notes-directory))
  ;; ebib-keywords-file "~/Dropbox/Bibliography/ebib-keywords.txt"
  (setq ebib-keywords-field-keep-sorted t)
  (setq ebib-keywords-file-save-on-exit 'always)
  ;; ebib-file-associations '(("pdf")) "using Emacs to open pdf"
  (setq ebib-use-timestamp t)  ; recording the time that entries are added
  (setq ebib-notes-symbol "📘")
  (setq ebib-index-columns '(("Year" 4 t)
                             ("Entry Key" 30 t)
                             ("Note" 2 nil)
                             ("Title" 50 t)
                             ("Series/Journal" 20 t)
                             ("Author/Editor" 40 nil)))
  (setq ebib-index-default-sort '("timestamp" . descend))

  (defun mk/ebib-display-series-or-journal (field key db)
    "Return series/journal FIELD content from KEY and DB."
    (or (ebib-get-field-value "Series" key db 'noerror 'unbraced 'xref)
        (ebib-get-field-value "Journal" key db "(No Series/Journal)" 'unbraced 'xref))
    )
  (setq ebib-field-transformation-functions
        '(("Title" . ebib-clean-TeX-markup-from-entry)
          ("Doi" . ebib-display-www-link)
          ("Url" . ebib-display-www-link)
          ("Note" . ebib-notes-display-note-symbol)
          ("Series/Journal" . mk/ebib-display-series-or-journal)
          ))
  :config
  (setq ebib-index-mode-line '("%e"
                               mode-line-front-space
                               ebib--mode-line-modified
                               mode-line-buffer-identification
                               (:eval (if (and ebib--cur-db (ebib-db-dependent-p ebib--cur-db))
                                          (format " [%s]" (ebib-db-get-filename (ebib-db-get-main ebib--cur-db) t))))
                               (:eval (format "  (%s)" (ebib--get-dialect ebib--cur-db)))
                               (:eval (if (and ebib--cur-db (ebib--get-key-at-point))
                                          (format "     Entry %d/%d" (line-number-at-pos) (count-lines (point-min) (point-max)))
                                        "     No Entries"))
                               (:eval (if (and ebib--cur-db (ebib-db-get-filter ebib--cur-db))
                                          (format "  |%s|" (ebib--filters-pp-filter (ebib-db-get-filter ebib--cur-db)))
                                        ""))))
  (defun mk/ebib--clean-string (str)
    "Clean the format of STR."
    (or (substring-no-properties (remove ?\n (format "%s" str))) ""))
  (defun mk/ebib--clean-field (key db field)
    "Clean the format of FIELD of KEY in DB."
    (mk/ebib--clean-string (ebib-get-field-value field key db 'noerror 'unbraced 'xref)))

  (defun mk/read-file-content (filename)
    "Read the file content of FILENAME."
    (with-temp-buffer
      (insert-file-contents filename)
      (buffer-string)))
  (defun mk/ebib-complete-rest-note-content (key db)
    "Gerneate the rest content of the note template accroding to KEY in DB."
    (let ((template (mk/read-file-content "~/.emacs.d/snippets/ebib/ebib-notes-template.org"))
          (title (mk/ebib--clean-field key db "title"))
          (date (format-time-string "%FT%T%z"))
          (authors (mk/ebib--clean-field key db "author"))
          (series (mk/ebib--clean-field key db "series")))
      (setq template (string-replace "${citekey}" key template))
      (setq template (string-replace "${orgid}" (org-id-new) template))
      (setq template (string-replace "${title}" title template))
      (setq template (string-replace "${date}" date template))
      (setq template (string-replace "${authors}" authors template))
      (setq template (string-replace "${series}" series template))
      template))
  (setq ebib-notes-template-specifiers '((?K . ebib-create-org-identifier)
                                         (?T . ebib-create-org-description)
                                         (?X . ebib-create-org-title)
                                         (?C . ebib-create-org-cite)
                                         (?L . ebib-create-org-link)
                                         (?F . ebib-create-org-file-link)
                                         (?D . ebib-create-org-doi-link)
                                         (?U . ebib-create-org-url-link)
                                         (?P . mk/ebib-complete-rest-note-content)))
  (setq ebib-notes-template "%%?%P\n")
  )

(defun mk/ebib-reading-list-show-entry ()
  "Jump to the ebib entry from the current reading list item."
  (interactive)
  (let ((custom-id (org-entry-get (point) "Custom_id")))
    (when (string-prefix-p "reading_" custom-id)
      (let ((ebib-key (substring custom-id 8)))
        (message "Jumping to ebib entry with key: %s" ebib-key)
        (ebib nil ebib-key)
        ))))

(use-package ebib-biblio
  :ensure nil
  :after (ebib biblio)
  :defer t
  :bind (:map biblio-selection-mode-map
              ("e" . ebib-biblio-selection-import)))
(use-package zotra
  :defer t
  :config
  (setq zotra-backend 'zotra-server)
  (setq zotra-local-server-directory "~/.emacs.d/third-parties/zotra-server/")
  )
(use-package ebib-collection
  :ensure (:type git :host github :repo "mkvoya/ebib-collection" :files ("*"))
  :after (ebib)
  :defer t)

(provide 'config-bib)
