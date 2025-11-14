;; -*- lexical-binding: t; -*-
;;; Emacs Configuration Fragment: Org Mode Publishing

(use-package ox-publish)

(setq mk/pub-base-dir "~/sites/dong.mk/src")
(setq mk/pub-out-dir "~/sites/dong.mk/gen")
(setq mk/pub-out-static-dir "~/sites/dong.mk/gen/static")


(setq mk/pub-html-head
      (concat
       "<meta charset='UTF-8'>\n"
       "<meta name='viewport' content='width=device-width, initial-scale=1.0'>\n"
       "<title>DONG Mingkai - Systems &amp; Storage Research</title>\n"
       "<link rel='stylesheet' href='https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.1/css/all.min.css'>\n"
       "<script src='https://ogbe.net/res/code.js'></script>\n"
       "<link rel='stylesheet' href='https://ogbe.net/res/style.css'>\n"
       ))

(defun mk/html-preamble (plist)
  "Respect file-local #+HTML_PREAMBLE if present, else fallback to ./preamble.html.
PLIST is the export property list passed by ox; we look up :input-file in it."
  (let* ((input-file (plist-get plist :input-file))
         (dir (and input-file (file-name-directory input-file))))
    (when input-file
      (with-temp-buffer
        (insert-file-contents input-file)
        (goto-char (point-min))
        ;; 捕获 #+HTML_PREAMBLE: 后面的整行内容
        (if (re-search-forward "^#\\+HTML_PREAMBLE:[ \t]*\\(.*\\)$" nil t)
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
               (t
                ;; 直接返回字符串值（可以是内联 HTML）
                val)))
          ;; fallback: 如果项目根目录有 preamble.html 就读它
          (let ((fallback (concat mk/pub-base-dir "preamble.html")))
            (when (file-exists-p fallback)
              (with-temp-buffer
                (insert-file-contents fallback)
                (buffer-string)))))))))


(defun mk/org-export-footnotes-as-tooltips (text backend info)
  "把脚注转换为 tooltip"
  (when (org-export-derived-backend-p backend 'html)
    (let* ((footnotes (plist-get info :footnotes))
           footdef-map)
      ;; 建立 footnote 映射表
      (dolist (f footnotes)
        (let* ((id (car f))
               (def (org-trim (org-export-data (cdr f) info))))
          (push (cons id def) footdef-map)))

      ;; 修改脚注引用（footref）
      (setq text
            (replace-regexp-in-string
             (rx "<a href=\"#fn:" (group (+? digit)) "\" class=\"footref\"" (+? anything) ">" (+? anything) "</a>")
             (lambda (match)
               (let* ((id (match-string 1 match))
                      (content (or (cdr (assoc id footdef-map)) "")))
                 (format "<a class=\"footref\" data-footnote=\"%s\">[%s]</a>"
                         (org-html-encode-plain-text content)
                         id)))
             text))
       text)))


(setq org-export-allow-bind-keywords t)

(setq org-publish-project-alist
      `(("pages"
         :base-directory ,mk/pub-base-dir
         :base-extension "org"
         :publishing-directory ,mk/pub-out-dir
         :publishing-function org-html-publish-to-html
         :recursive t
         :section-numbers nil
         :with-toc nil
         :html-preamble mk/html-preamble
         :html-postamble t
         :auto-sitemap nil
         :html-head ,mk/pub-html-head
         :filter-final-output (mk/org-export-footnotes-as-tooltips)
         )
        ("images"
         :base-directory ,mk/pub-base-dir
         :base-extension "jpg\\|gif\\|png"
         :recursive t
         :publishing-directory ,mk/pub-out-static-dir
         :publishing-function org-publish-attachment)
        ("dong.mk"
         :components ("pages" "images"))))

(provide 'config-org-pub)
