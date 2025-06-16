#!/bin/bash
#
# <xbar.title>Emacs Org Clocking</xbar.title>
# <xbar.version>v0.2</xbar.version>
# <xbar.author>Mingkai Dong</xbar.author>
# <xbar.author.github>mkvoya</xbar.author.github>
# <xbar.desc>Emacs Org Clocking</xbar.desc>

case "$1" in
#    "email")
#	open -a Emacs    # bring to front
#	emacsclient -e '(mu4e-compose-new)'
#	exit
#	;;
"buffer")
  open -a Emacs
  emacsclient -e '(progn (select-frame (make-frame))(switch-to-buffer (get-buffer-create "*scratch*")) (set-frame-size (selected-frame) 92 72)(set-frame-position (selected-frame) 840 0))'
  exit
  ;;
"today")
  open -a Emacs
  emacsclient -c -n -e '(org-journal-new-entry nil)'
  exit
  ;;
"view-journal")
  open -a Emacs
  emacsclient -c -n -e '(org-journal-open-current-journal-file)'
  exit
  ;;
"clock-out")
  emacsclient -e '(org-clock-out)'
  open -g "swiftbar://refreshPlugin?name=org-clocking"
  exit
  ;;
"clock-in")
  emacsclient -e "(swiftbar-clock-in \"$2\")"
  open -g "swiftbar://refreshPlugin?name=org-clocking"
  exit
  ;;
esac

# Get current clocking task from Emacs org-mode
current_clock=$(emacsclient -e '(if (org-clocking-p)
								(substring-no-properties (org-clock-get-clock-string))
                                  "")')

current_clock=$(echo "$current_clock" | sed 's/^"\(.*\)"$/\1/')

echo "🦄 $current_clock"
echo "---"
#echo "📬 Email | bash=\"$0\" param1=email terminal=false"
echo "📠 *scratch* Buffer | bash=\"$0\" param1=buffer terminal=false"
echo "📅 Today"
echo "--📝 New Entry | bash=\"$0\" param1=today terminal=false"
echo "--📊 View Journal | bash=\"$0\" param1=view-journal terminal=false"
echo "🔚 Clock Out | bash=\"$0\" param1=clock-out terminal=false"
echo "🔜 Clock in | bash=\"$0\" param1=clock-in terminal=false"
emacsclient -e '(swiftbar-list-agenda-swiftbar-items)' | sed 's/^"\(.*\)"$/\1/' | tr ',' '\n' | while read -r line; do
    echo "--$line | bash=\"$0\" param1=clock-in param2=\"$line\" terminal=false"
done