;; -*- lexical-binding: t; -*-
;;; Emacs Config Fragement: News & Feeds


(use-package elfeed
  :defer t
  :bind ("C-c f" . elfeed)
  :config
  ;; The feed list
  (setq elfeed-feeds
        '(("http://nullprogram.com/feed/" blog emacs)
          "http://www.50ply.com/atom.xml"  ; no autotagging
          ;; Apple news
          ;; ("https://feeds.macrumors.com/MacRumors-All" apple)
          ;; storage news
          ("https://thessdguy.com/feed/" storage)
          ("https://thememoryguy.com/feed/" storage)
          ("https://blocksandfiles.com/feed/" storage)
          ;; ("https://thesanguy.com/feed/" storage) website down
          ;;
          ("https://www.nextplatform.com/feed/" it)
          ("https://devclass.com/feed/" it)
          ;; ("https://www.theregister.com/headlines.atom" it)
          ("http://ithare.com/rssfeed/" it)
          ("http://nedroid.com/feed/" webcomic)
          "http://planet.emacsen.org/atom.xml"
          "https://hnrss.org/show?points=100&comments=10"
          ;; Linux LWN
          "https://lwn.net/headlines/rss"))
  (setq-default elfeed-search-filter "@1-week-ago +unread ")
  )

(use-package elfeed-summarize
  :ensure
  :after (elfeed llm)
  :config
  ;; (require 'llm-ollama)
  (require 'llm-openai)
  ;; (setq elfeed-summarize-llm-provider
  ;;       (make-llm-ollama :chat-model "qwen3.5:0.8b"))
  (setq elfeed-summarize-llm-provider
        (make-llm-openai-compatible
         :url llm-sjtu-url
         :key llm-sjtu-key
         :chat-model "qwen3.5-27b"))
  (setq elfeed-summarize-system-prompt
        "You write short summaries for an RSS feed reader.
The summary appears below the article headline and helps the user decide
whether to read the full article. You MUST respond with exactly one
short sentence. No preamble, no quotation marks, no bullet points, no
multiple paragraphs. 请使用中文进行总结。")

  (elfeed-summarize-mode 1))


(provide 'config-feed)
