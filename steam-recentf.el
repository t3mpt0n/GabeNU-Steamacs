(require 'url)
(require 'json)
(require 'ivy)
(require 'ivy-rich)

(defvar steam_api_key nil
  "Steam WebAPI Key.
   You can either register or get your key over at this link: https://steamcommunity.com/dev/apikey")
(defvar steam_id nil
  "Steam User ID
   You can get it from this link: https://steamdb.info/calculator")

(defun steam-get-steaminfo-recentf ()
  "Return a list of recently played games"
  (let* ((url-string (format "https://api.steampowered.com/IPlayerService/GetRecentlyPlayedGames/v0001/?key=%s&steamid=%s&format=json" steam_api_key steam_id))
         (json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string))
    (with-current-buffer
      (url-retrieve-synchronously url-string)
      (goto-char (point-min))
      (re-search-forward "^$")
      (let* ((json (json-read)))
        (let* ((games (gethash "games" (gethash "response" json))))
          (mapcar (lambda (game)
                    (let ((name (gethash "name" game))
                          (appid (concat "(" (number-to-string (gethash "appid" game)) ")"))
                          (playtime (concat "⏲" (format-seconds "%02h:%02m" (* (gethash "playtime_forever" game) 60))))
                          (checkarchive (concat "~/.steam/steam/steamapps/appmanifest_" (number-to-string (gethash "appid" game)) ".acf")))
                      (if (file-exists-p checkarchive)
                          (format "%-98s %s" (concat appid " " name) playtime)
                        (format "%-98s %-10s %s%s" (concat appid " " name) playtime (propertize "⚠" 'face '(:foreground "yellow" :height 1.5)) (propertize "NOT INSTALLED!" 'face '(:foreground  "red"))))))
                  games))))))

(setq display-buffer-alist
      '(("\\*Async Shell Command\\*.*"
         (display-buffer-no-window))))

(defun counsel-steam-recentf ()
  "Return a menu of your recently played Steam games."
  (interactive)
  (let ((candids (steam-get-steaminfo-recentf)))
    (ivy-read "  Steam Recentf: " candids
              :action (lambda (game)
                        (let ((regexp "\\((\\([0-9]+\\))\\)")) ; Regular expression to match text in parentheses
                          (if (string-match regexp game)
                              (async-shell-command (concat "/bin/bash ~/.local/opt/scripts/steam_launcher.sh " (format "%s" (match-string 2 game))))
                            nil))))))
