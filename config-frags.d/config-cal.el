;; -*- lexical-binding: t; -*-
;;; Emacs Config Fragement: Calender

;; https://raw.githubusercontent.com/wowhxj/emacs-from-scratch/master/emacs-config.org
(use-package calendar
  :init
  (setq calendar-longitude 121.4737
        calendar-latitude 31.2304
        calendar-location-name "SH")
  :ensure nil
  :hook (calendar-today-visible . calendar-mark-today)
  :custom
  ;; 是否显示中国节日，我们使用 `cal-chinese-x' 插件
  (calendar-chinese-all-holidays-flag nil)
  ;; 是否显示节日
  (calendar-mark-holidays-flag t)
  ;; 是否显示Emacs的日记，我们使用org的日记
  (calendar-mark-diary-entries-flag nil)
  ;; 数字方式显示时区，如 +0800，默认是字符方式如 CST
  (calendar-time-zone-style 'numeric)
  ;; 日期显示方式：year/month/day
  (calendar-date-style 'iso)
  ;; 中文天干地支设置
  (calendar-chinese-celestial-stem ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"])
  (calendar-chinese-terrestrial-branch ["子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"])
  ;; 设置中文月份
  ;; (calendar-month-name-array ["一月" "二月" "三月" "四月" "五月" "六月" "七月" "八月" "九月" "十月" "十一月" "十二月"])
  ;; 设置星期标题显示
  ;; (calendar-day-name-array ["日" "一" "二" "三" "四" "五" "六"])
  ;; 周一作为一周第一天
  (calendar-week-start-day 0)
  )
;; 时间解析增加中文拼音
(use-package parse-time
  :ensure nil
  :defer t
  :config
  (setq parse-time-months
        (append '(("yiy" . 1) ("ery" . 2) ("sany" . 3)
                  ("siy" . 4) ("wuy" . 5) ("liuy" . 6)
                  ("qiy" . 7) ("bay" . 8) ("jiuy" . 9)
                  ("shiy" . 10) ("shiyiy" . 11) ("shiery" . 12)
                  ("yiyue" . 1) ("eryue" . 2) ("sanyue" . 3)
                  ("siyue" . 4) ("wuyue" . 5) ("liuyue" . 6)
                  ("qiyue" . 7) ("bayue" . 8) ("jiuyue" . 9)
                  ("shiyue" . 10) ("shiyiyue" . 11) ("shieryue" . 12))
                parse-time-months))

  (setq parse-time-weekdays
        (append '(("zri" . 0) ("zqi" . 0)
                  ("zyi" . 1) ("zer" . 2) ("zsan" . 3)
                  ("zsi" . 4) ("zwu" . 5) ("zliu" . 6)
                  ("zr" . 0) ("zq" . 0)
                  ("zy" . 1) ("ze" . 2) ("zs" . 3)
                  ("zsi" . 4) ("zw" . 5) ("zl" . 6))
                parse-time-weekdays)))

;; 中国节日设置
(use-package cal-china-x
  :ensure t
  :commands cal-china-x-setup
  :hook (elpaca-after-init . cal-china-x-setup)
  :config
  ;; 重要节日设置
  (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
  ;; 所有节日设置
  (setq cal-china-x-general-holidays
        '(;;公历节日
          (holiday-fixed 1 1 "元旦")
          (holiday-fixed 2 14 "情人节")
          (holiday-fixed 3 8 "妇女节")
          (holiday-fixed 4 1 "愚人节")
          (holiday-fixed 5 1 "劳动节")
          (holiday-fixed 5 4 "青年节")
          (holiday-float 5 0 2 "母亲节")
          (holiday-fixed 6 1 "儿童节")
          (holiday-float 6 0 3 "父亲节")
          (holiday-fixed 9 10 "教师节")
          (holiday-fixed 10 1 "国庆节")
          (holiday-fixed 12 25 "圣诞节")
          ;; 农历节日
          (holiday-lunar 12 30 "除夕" 0)
          (holiday-lunar 1 1 "春节" 0)
          (holiday-lunar 1 15 "元宵" 0)
          (holiday-solar-term "清明" "清明")
          (holiday-solar-term "小寒" "小寒")
          (holiday-solar-term "大寒" "大寒")
          (holiday-solar-term "立春" "立春")
          (holiday-solar-term "雨水" "雨水")
          (holiday-solar-term "惊蛰" "惊蛰")
          (holiday-solar-term "春分" "春分")
          (holiday-solar-term "谷雨" "谷雨")
          (holiday-solar-term "立夏" "立夏")
          (holiday-solar-term "小满" "小满")
          (holiday-solar-term "芒种" "芒种")
          (holiday-solar-term "夏至" "夏至")
          (holiday-solar-term "小暑" "小暑")
          (holiday-solar-term "大暑" "大暑")
          (holiday-solar-term "立秋" "立秋")
          (holiday-solar-term "处暑" "处暑")
          (holiday-solar-term "白露" "白露")
          (holiday-solar-term "秋分" "秋分")
          (holiday-solar-term "寒露" "寒露")
          (holiday-solar-term "霜降" "霜降")
          (holiday-solar-term "立冬" "立冬")
          (holiday-solar-term "小雪" "小雪")
          (holiday-solar-term "大雪" "大雪")
          (holiday-solar-term "冬至" "冬至")
          (holiday-lunar 5 5 "端午" 0)
          (holiday-lunar 8 15 "中秋" 0)
          (holiday-lunar 7 7 "七夕" 0)
          (holiday-lunar 9 9 "重阳" 0)))
  ;; 设置日历的节日，通用节日已经包含了所有节日
  (setq calendar-holidays (append cal-china-x-general-holidays)))
;; (setq mark-holidays-in-calendar t)

;; * Calfw
(use-package calfw
  :ensure (:host github :repo "haji-ali/emacs-calfw" :files ("*"))
  :init
  (use-package calfw-org :after (org))
  (use-package calfw-blocks
    :ensure (:host github :repo "haji-ali/calfw-blocks" :files ("*")))
  (use-package maccalfw
    :commands maccalfw-open
    :ensure (:host github
                   :repo "haji-ali/maccalfw"
                   :files ("maccalfw.el" ("src" . "src"))))
  (require 'calfw-cal)
  :config
  (defun mk/open-calendar ()
    (interactive)
    (cfw:open-calendar-buffer
     :contents-sources
     (list
      (cfw:org-create-source "Green")  ; orgmode source
      ;; (cfw:howm-create-source "Blue")  ; howm source
      ;; (cfw:cal-create-source "Orange") ; diary source
      ;; (cfw:ical-create-source "Moon" "~/moon.ics" "Gray")  ; ICS source1
      ;; (cfw:ical-create-source "gcal" "https://..../basic.ics" "IndianRed") ; google calendar ICS
      )))
  )

(use-package ox-icalendar
  :defer t
  :ensure nil
  :after (org)
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
  :defer t
  ;; :after (async)
  :init
  (require 'async)
  (setq org-caldav-url "https://dong.mk/radicale/mkvoya/")
  (setq org-caldav-todo-percent-states
        '(
          (0 "TODO")
          (100 "DONE")
          (0 "IDEA")
          (100 "CANCELED")))
  (setq org-caldav-calendars
        '(
          (
           :calendar-id "9d6f9f39-cba5-fe5b-bd49-c61168d64f81"
           ;; :calendar-id "OrgSync"
           :inbox "~/Dropbox/Dreams/Org/Caldav.inbox.org"
           :files ("~/Dropbox/Dreams/Org/Main.org"
                   "~/Dropbox/Dreams/Org/Inbox.org"
                   "~/Dropbox/Dreams/Org/Ebib-ReadingList.org"
                   ))
          ))
  (setq org-icalendar-timezone "Asia/Shanghai")
  )

(provide 'config-cal)
