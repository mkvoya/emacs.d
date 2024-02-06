(defun mk/frame-pin ()
  "Pin current frame to always-on-top."
  (interactive)
  (modify-frame-parameters
   nil '((z-group . above))))
(defun mk/frame-unpin ()
  "Unpin current frame to always-on-top."
  (interactive)
  (modify-frame-parameters
   nil '((z-group . nil))))


(defun mk/make-pinned-frame ()
  "Make a new pined frame."
  (interactive)
  (make-frame '((undecorated . 1)
                (z-group . above)
                (width . 80)
                (height . 10)
                (user-size . t)
                (left . 1)
                (top . 1)
                (user-position . t)
                (tab-bar-lines . 0)
                (drag-with-header-line . t)
                (drag-with-tab-line . t)
                (drag-with-mode-line . t))))

(defun mk/ns-get-window-title-and-buffer ()
  (interactive)
  ;; Apple script from the activitywatch project
  (do-applescript "
  global frontApp, frontAppName, windowTitle
  set windowTitle to \"\"
  tell application \"System Events\"
          set frontApp to first application process whose frontmost is true
          set frontAppName to name of frontApp
          tell process frontAppName
                  try
                          tell (1st window whose value of attribute \"AXMain\" is true)
                                  set windowTitle to value of attribute \"AXTitle\"
                          end tell
                  end try
          end tell
  end tell
  return frontAppName & \"
  \" & windowTitle"))
