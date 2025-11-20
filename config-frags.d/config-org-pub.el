;; -*- lexical-binding: t; -*-
;;; Emacs Configuration Fragment: Org Mode Publishing

(use-package ox-publish :after org :ensure nil)

(setq mk/pub-base-dir "~/sites/dong.mk/src")
(setq mk/pub-out-dir "~/sites/dong.mk/gen")
;; (setq mk/pub-out-static-dir "~/sites/dong.mk/gen/static")


(setq mk/pub-html-head
      (concat
       "<meta charset='UTF-8'>\n"
       "<meta name='viewport' content='width=device-width, initial-scale=1.0'>\n"
       "<title>DONG Mingkai - Systems &amp; Storage Research</title>\n"
       "<link rel='stylesheet' href='https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.1/css/all.min.css'>\n"
       "<script src='https://ogbe.net/res/code.js'></script>\n"
       "<link rel='stylesheet' href='https://ogbe.net/res/style.css'>\n"
       "<link rel='stylesheet' href='./static/mkstyle.css'>\n"
       ))

(defun mk/xxxamble (plist pre-or-post)
  "Respect file-local #+HTML_XXXAMBLE if present, else fallback to ./XXXamble.html.
PLIST is the export property list passed by ox; we look up :input-file in it.
PRE-OR-POST is \"pre\" or \"post\". "
  (let* ((input-file (plist-get plist :input-file))
         (dir (and input-file (file-name-directory input-file)))
         (amble-regex (if (string= pre-or-post "pre")
                          "^#\\+HTML_PREAMBLE:[ \t]*\\(.*\\)$"
                        "^#\\+HTML_POSTAMBLE:[ \t]*\\(.*\\)$"))
         (amble-fallback-file (if (string= pre-or-post "pre")
                                  (concat mk/pub-base-dir "preamble.html")
                                (concat mk/pub-base-dir "postamble.html"))))
    (when input-file
      (with-temp-buffer
        (insert-file-contents input-file)
        (goto-char (point-min))
        ;; 捕获 #+HTML_XXXAMBLE: 后面的整行内容
        (if (re-search-forward amble-regex nil t)
            (let* ((val (string-trim (match-string 1)))
                   (maybe-path
                    ;; 如果看起来像路径或文件名（包含 / 或 以 .html/.htm 结尾），则视为文件路径
                    (and (or (string-match-p "[/\\\\]" val)
                             (string-match-p "\\.html?$" val))
                         (if (file-name-absolute-p val) val (and dir (expand-file-name val dir))))))
              (cond
               ((and maybe-path (file-exists-p maybe-path))
                ;; 读取指定的外部文件内容并返回
                (with-temp-buffer
                  (insert-file-contents maybe-path)
                  (buffer-string)))
               (t ;; 直接返回字符串值（可以是内联 HTML）
                val)))
          ;; fallback: 如果项目根目录有 XXXamble.html 就读它
            (when (file-exists-p amble-fallback-file)
              (with-temp-buffer
                (insert-file-contents amble-fallback-file)
                (buffer-string))))))))

(defun mk/html-preamble (plist)
  (mk/xxxamble plist "pre"))

(defun mk/html-postamble (plist)
  (let ((content (mk/xxxamble plist "post")))
    (string-replace "[[date]]" (format-time-string "%B %e, %Y" (current-time)) content)))

(defun mk/org-html-remove-head-title (text backend info)
  "Remove <title> tags from the generated HTML."
  (if (org-export-derived-backend-p backend 'html)
      ;; 使用正则将 <title>...</title> 替换为空字符串
      (replace-regexp-in-string "<title>.*?</title>" "" text)
    text))


(defun mk/org-html-publish-to-html (plist filename pub-dir)
  "Wrapper around org-html-publish-to-html that removes <title>."
  (let ((org-html-title ""))
    (org-html-publish-to-html plist filename pub-dir)
    ;; 删除 <title> 标签
    (let ((target-file (expand-file-name
                        (concat (file-name-sans-extension (file-name-nondirectory filename)) ".html")
                        pub-dir)))
      (when (file-exists-p target-file)
        (with-temp-buffer
          (insert-file-contents target-file)
          (goto-char (point-min))
          (re-search-forward "<title>.*?</title>" nil t)
          (replace-match "")
          (write-region (point-min) (point-max) target-file))))))


(setq org-export-allow-bind-keywords t)

(setq org-publish-project-alist
      `(("pages"
         :base-directory ,mk/pub-base-dir
         :base-extension "org"
         :publishing-directory ,mk/pub-out-dir
         :publishing-function mk/org-html-publish-to-html
         :recursive t
         :section-numbers nil
         :with-toc nil
         :html-preamble mk/html-preamble
         :html-postamble mk/html-postamble
         :auto-sitemap nil
         :html-head ,mk/pub-html-head
         )
        ("static"
         :base-directory ,mk/pub-base-dir
         :base-extension "jpg\\|gif\\|png\\|svg\\|css\\|js"
         :recursive t
         :publishing-directory ,mk/pub-out-dir
         :publishing-function org-publish-attachment)
        ("dong.mk"
         :components ("pages" "static"))))

(provide 'config-org-pub)
