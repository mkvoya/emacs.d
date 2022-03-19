;;; package --- Feishu Wiki Mode -*- lexical-binding: t -*-
;;; Commentary:
;;; This file connects Feishu to sync wiki contents.
;;; Code:

(setq lexical-binding t)

(setq feishu/app-id "cli_a2bb73218d3a5013")
(setq feishu/app-secret nil)
(setq feishu/user-access-token nil)
(setq feishu/app-access-token nil)

(require 'elnode)
(require 'request)

(defun feishu/get-app-access-token (fn)
  (request
    "https://open.feishu.cn/open-apis/auth/v3/tenant_access_token/internal"
    :headers '(("Content-Type" . "application/json; charset=utf-8"))
    :data (json-encode `(("app_id" . ,feishu/app-id)
                         ("app_secret" . ,feishu/app-secret)))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall fn (assoc-default 'tenant_access_token data))))))

(defun feishu/get-user-access-token (code fn)
  "Get user access token via CODE and call FN."
  (let ((auth-token (concat "Bearer " feishu/app-access-token)))
    (request
      "https://open.feishu.cn/open-apis/authen/v1/access_token"
      :headers `(("Content-Type" . "application/json; charset=utf-8")
                 ("Authorization" . ,auth-token))
      :data (json-encode `(("grant_type" . "authorization_code")
                           ("code" . ,code)))
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (funcall fn (assoc-default 'data data)))))
    ))

(defun feishu/get-all-tokens (code)
  "Get all feishu tokens via user login CODE."
  (feishu/get-app-access-token
   #'(lambda (app-access-token)
       (setq feishu/app-access-token app-access-token)
       (feishu/get-user-access-token
        code
        #'(lambda (user-info)
            (setq feishu/user-info user-info)
            (setq feishu/user-access-token (assoc-default 'access_token feishu/user-info))
            (print user-info))
        ))
   ))

(defun feishu/user-code-handler (httpcon)
  "Handle feishu user access token via HTTPCON."
  (let ((code (elnode-http-param httpcon "code")))
    (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
    (feishu/get-all-tokens code)
    (elnode-http-return httpcon (format "<html><b>Code: %s</b></html>" code))))

(defun feishu/get-wiki-spaces (user-token fn)
  "Get wiki spaces with USER-TOKEN and call FN."
  (let ((auth-token (concat "Bearer " user-token)))
    (request
      "https://open.feishu.cn/open-apis/wiki/v2/spaces"
      :headers `(("Content-Type" . "application/json")
                 ("Authorization" . ,auth-token))
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (funcall fn data))))))


(defun print-data (data)
  "Print DATA."
  (print (format "I sent: %S" data)))

(feishu/get-wiki-spaces feishu/user-access-token #'print-data)

(defun feishu/get-doc-raw-content (doc-token fn)
  "Get wiki spaces with DOC-TOKEN and call FN."
  (let ((auth-token (concat "Bearer " feishu/user-access-token))
        (url (format "https://open.feishu.cn/open-apis/doc/v2/%s/raw_content"
                      doc-token)))
    (request
      url
      :headers `(("Content-Type" . "application/json")
                 ("Authorization" . ,auth-token))
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (funcall fn (assoc-default 'data data)))))))

(feishu/get-doc-raw-content "wikcnPuSinisLeGSefQLQsVAbod" #'print-data)

(elnode-start 'feishu/user-code-handler :port 12301 :host "127.0.0.1")

(provide 'feishu)
;;; feishu.el ends here
