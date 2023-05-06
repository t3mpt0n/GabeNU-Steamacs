(require 'url)
(require 'json)

(defvar steam_api_key nil
  "Steam WebAPI Key.
   You can either register or get your key over at this link: https://steamcommunity.com/dev/apikey")
(defvar steam_id nil
  "Steam User ID
   You can get it from this link: https://steamdb.info/calculator")

(defvar steam_account_id nil
  "Steam Account ID
  This is different from your `steam_id` which is your user ID.
  Like `steam_id` you can also get it at: https://steamdb.info/calculator")

(defvar steamdir "steamacs"
  "Name of Steam Directory on ~/.emacs.d")

(unless (file-exists-p (concat "~/.emacs.d/" steamdir))
  (make-directory (concat "~/.emacs.d/" steamdir)))

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

(defun steam-get-steaminfo-gamemenu ()
  "Return all the games owned by the user"
  (let ((url-string (format "https://api.steampowered.com/IPlayerService/GetOwnedGames/v0001/?key=%s&steamid=%s&include_appinfo=1&include_played_free-games=1&format=json" steam_api_key steam_id))
        (json-object-type 'hash-table)
        (json-array-type 'list)
        (json-key-type 'string))
    (with-current-buffer
      (url-retrieve-synchronously url-string)
      (goto-char (point-min))
      (re-search-forward "^$")
      (let ((json (json-read)))
        (let ((games (gethash "games" (gethash "response" json))))
          (mapcar (lambda (game)
                    (let* ((name (gethash "name" game))
                          (appid (concat "(" (number-to-string (gethash "appid" game)) ")"))
                          (playtime (concat "⏲" (format-seconds "%02h:%02m" (* (gethash "playtime_forever" game) 60))))
                          (checkarchive (concat "~/.steam/steam/steamapps/appmanifest_" (number-to-string (gethash "appid" game)) ".acf")))
                      (if (file-exists-p checkarchive)
                          (format "%-98s %s" (concat appid " " name) playtime)
                        (format "%-98s %-10s %s%s" (concat appid " " name) playtime (propertize "⚠" 'face '(:foreground "yellow" :height 1.5)) (propertize "NOT INSTALLED!" 'face '(:foreground  "red"))))))
                  games))))))

(provide 'steaminfo)
