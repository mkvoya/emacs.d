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
          ("https://feeds.macrumors.com/MacRumors-All" apple)
          ;; storage news
          ("https://thessdguy.com/feed/" storage)
          ("https://thememoryguy.com/feed/" storage)
          ("https://blocksandfiles.com/feed/" storage)
          ;; ("https://thesanguy.com/feed/" storage) website down
          ;;
          ("https://www.nextplatform.com/feed/" it)
          ("https://devclass.com/feed/" it)
          ("https://www.theregister.com/headlines.atom" it)
          ("http://ithare.com/rssfeed/" it)
          ("http://nedroid.com/feed/" webcomic)
          "http://planet.emacsen.org/atom.xml"))
  (setq-default elfeed-search-filter "@1-week-ago +unread ")
  )

(provide 'config-feed)
