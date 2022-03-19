;;; biblio-acm.el --- Lookup and import bibliographic entries from dl.acm.org -*- lexical-binding: t -*-

;; Copyright (C) 2022  Mingkai Dong

;; Author: Mingkai Dong <mk@dong.mk>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Lookup and download bibliographic records from dl.acm.org using
;; `biblio-acm'.
;;
;; This package uses `biblio-selection-mode', and plugs into the more general
;; `biblio' package (which see for more documentation).

;;; Code:

(require 'biblio-core)
(require 'biblio-doi)
(require 'request)
(require 'elquery)
(require 'json)
(require 'citeproc)

(defun biblio-acm--extract-doi (elq-item)
  "Extract a doi from search result ELQ-ITEM."
  (let* ((a (car (elquery-$ ".issue-item__title a" elq-item)))
         (href (elquery-prop a "href")))
    ;; parse doi from /doi/10.1016/j.compag.2014.05.008
    (if href (substring href 5 nil) "")))

(defun biblio-acm--extract-dois (html)
  "Extract dois from search result HTML."
  (let ((items (elquery-$ ".issue-item-container" html)))
    (mapcar #'biblio-acm--extract-doi items)))

(defun biblio-acm--extract-X (elq-item cls)
  "Extract full text under CLS node from search result ELQ-ITEM."
  (elquery-full-text (car (elquery-$ cls elq-item)) "  "))

(defun biblio-acm--extract-title (elq-item)
  "Extract a title from search result ELQ-ITEM."
  (biblio-acm--extract-X elq-item ".issue-item__title"))

(defun biblio-acm--extract-authors (elq-item)
  "Extract authors from search result ELQ-ITEM."
  (string-join (mapcar 'elquery-full-text
                       (elquery-$ ".issue-item__content-right ul[aria-label=authors] a"
                                  elq-item))
  "; "))

(defun biblio-acm--extract-interesting-fields (elq-item)
  "Prepare an ACM Digital Library search result ELQ-ITEM for display."
  (let* ((.title (biblio-acm--extract-title elq-item))
         (.doi (biblio-acm--extract-doi elq-item))
         (.authors- (biblio-acm--extract-X elq-item ".issue-item__content-right ul[aria-label=authors]"))
         (.authors (split-string .authors- "," t))
         (.info (biblio-acm--extract-X elq-item ".issue-item__detail"))
         (.publication_year "0000")
         ;; (.abstract_url "abs_url")
         )
    ;; (print .info)
    ;; (print .title)
    ;; (print .authors)
    ;; (print .doi)
    (list (cons 'doi .doi)
          ;; (cons 'year .publication_year)
          (cons 'title .title)
          (cons 'authors .authors)
          (cons 'container .info))
          ;; (cons 'references .doi)
          ;; (cons 'url .abstract_url)
          ))

(defun biblio-acm--elquery-tree ()
  "Parse current buffer as an elquery tree."
  (let ((tree (libxml-parse-html-region (point-min) (point-max))))
    (thread-last tree
                 (--tree-map (if (stringp it) (s-trim (s-collapse-whitespace it)) it))
                 (elquery--parse-libxml-tree nil))))

(defun biblio-acm--parse-search-results ()
  "Extract search results from ACM Digital Library response."
  (biblio-decode-url-buffer 'utf-8)
  (let* ((html (biblio-acm--elquery-tree))
         (items (elquery-$ ".issue-item-container" html)))
    (reverse (mapcar #'biblio-acm--extract-interesting-fields items))))

(defun biblio-acm--sync-bibtex-request (doi)
  "Synchronous request to retrieve the DOI bibtex."
  (request "https://dl.acm.org/action/exportCiteProcCitation"
    :type "POST"
    :sync t
    :data (format "dois=%s&targetFile=custom-bibtex&format=bibTex" (url-hexify-string doi))
    :headers '(("Content-Type" . "application/x-www-form-urlencoded"))
    :parser 'json-read))

(defun biblio-acm--locale (locale)
  "Construct the LOCALE object."
  (with-temp-buffer
    (erase-buffer)
    (insert locale)
    (libxml-parse-xml-region (point-min) (point-max))))

(defun biblio-acm--retrieve-bibtex (doi)
  "Retrieve the bibtex of DOI from ACM Digital Library URL."
  (let* ((json-key-type 'symbol)
         (json-array-type 'list)
         (json-object-type 'alist)
         (response (biblio-acm--sync-bibtex-request doi))
         (res (request-response-data response))
         (style (alist-get 'style res))
         (locale (alist-get 'locale res))
         (items (alist-get 'items res))
         (item (cdr (car (car items))))
         (locale-obj (biblio-acm--locale locale))
         (style-obj (citeproc-create-style style #'(lambda (x) locale-obj))))
    ;; (print item)
    (citeproc-render-item item style-obj 'bib 'latex)
    ))

(defun biblio-acm--forward-bibtex (metadata forward-to)
  "Forward BibTeX for ACM Xplore entry METADATA to FORWARD-TO."
  (let ((doi (biblio-alist-get 'doi metadata)))
    (funcall forward-to
             (biblio-acm--retrieve-bibtex doi))))

(defun biblio-acm--url (query)
  "Create an ACM Xplore url to look up QUERY."
  (format "https://dl.acm.org/action/doSearch?AllField=%s&expand=all"
          (url-encode-url query)))

;;;###autoload
(defun biblio-acm-backend (command &optional arg &rest more)
  "An ACM Digital Library backend for biblio.el.
COMMAND, ARG, MORE: See `biblio-backends'."
  (pcase command
    (`name "ACM Digital Library")
    (`prompt "ACM Digital Library query: ")
    (`url (biblio-acm--url arg))
    (`parse-buffer (biblio-acm--parse-search-results))
    (`forward-bibtex (biblio-acm--forward-bibtex arg (car more)))
    (`register (add-to-list 'biblio-backends #'biblio-acm-backend))))

;;;###autoload
(add-hook 'biblio-init-hook #'biblio-acm-backend)

;;;###autoload
(defun biblio-acm-lookup (&optional query)
  "Start a ACM search for QUERY, prompting if needed."
  (interactive)
  (biblio-lookup #'biblio-acm-backend query))

;;;###autoload
(defalias 'acm-lookup 'biblio-acm-lookup)

(provide 'biblio-acm)
;;; biblio-acm.el ends here
