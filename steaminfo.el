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
  "Name of Steamacs Directory on `user-emacs-directory`")

(defcustom steamlauncher_sh (concat user-emacs-directory steamdir "/steamlauncher.sh")
  "Path of `steamlauncher.sh` executable."
  :type 'string
  :group 'paths)

(defcustom steamlaunchoptions_py (concat user-emacs-directory steamdir "/steamlaunchoptions.py")
  "Path of `steamlaunchoptions.py` executable."
  :type 'string
  :group 'paths)

(unless (file-exists-p (concat user-emacs-directory steamdir))
  (make-directory (concat user-emacs-directory steamdir)))

(unless (file-exists-p steamlauncher_sh)
  (with-temp-buffer
    (insert
     (concat
      "#!/bin/bash\n"
      "\n"
      "steam -silent -applaunch $1 2>&1 | tee >(\n"
      "  while read line; do\n"
      "    if [[ $line == *\"Game process removed: AppID $1\"* ]]; then\n"
      "      sleep 2; pkill steam; break\n"
      "    fi\n"
      "  done\n"
      ")"))
    (write-region (point-min) (point-max) steamlauncher_sh)
    (set-file-modes steamlauncher_sh #o755)))

(unless (file-exists-p steamlaunchoptions_py)
  (with-temp-buffer
    (insert
     (concat
      "#!/usr/bin/env python3\n"
      "\n"
      "import vdf\n"
      "import json\n"
      "import subprocess\n"
      "import os\n"
      "import sys\n"
      "\n"
      "localconfig = os.getenv('HOME') + \"/.steam/steam/userdata/" steam_account_id "/config/localconfig.vdf\"\n"
      "with open(localconfig, 'r') as f:\n"
      "  data = vdf.parse(f)\n"
      "  try:\n"
      "    appdata = data['UserLocalConfigStore']['Software']['valve']['Steam']['apps']\n"
      "  except KeyError:\n"
      "    appdata = data['UserLocalConfigStore']['Software']['Valve']['Steam']['apps']\n"
      "\n"
      "def getlaunchoptions(appdata):\n"
      "  appstr = \"{\\n\"\n"
      "  for key1, inner_dict in appdata.items():\n"
      "    appstr += '\"' + f\"{key1}\" + '\"' + \": {\\n\"\n"
      "    for key2, value in inner_dict.items():\n"
      "      if key2 == \"LaunchOptions\":\n"
      "        value = value.replace('\"', '\\\\\"')\n"
      "        appstr += '\"' + f\"{key2}\" + '\"' + \": \" + '\"' + f\"{value}\" + '\"\\n'\n"
      "    appstr += \"},\\n\"\n"
      "  appstr += \"}\"\n"
      "  return appstr\n"
      "if len(sys.argv) > 1:\n"
      "  appid = sys.argv[1]\n"
      "  launchopt = sys.argv[2]\n"
      "  appdata[appid]['LaunchOptions'] = launchopt\n"
      "  vdf.dump(data, open(localconfig, 'w'), pretty=True)\n"
      "  old, new = '},', '}'\n"
      "  instance = getlaunchoptions(appdata).rfind(old)\n"
      "  glo = getlaunchoptions(appdata)[:instance] + new + getlaunchoptions(appdata)[instance+len(old):]\n"
      "  with open(" (concat "\"" (expand-file-name "launchoptions.json" (concat user-emacs-directory steamdir)) "\"") ", 'w') as f:\n"
      "    print(glo, file=f)\n"
      "else:\n"
      "  old, new = '},', '}'\n"
      "  instance = getlaunchoptions(appdata).rfind(old)\n"
      "  glo = getlaunchoptions(appdata)[:instance] + new + getlaunchoptions(appdata)[instance+len(old):]\n"
      "  with open(" (concat "\"" (expand-file-name "launchoptions.json" (concat user-emacs-directory steamdir)) "\"") ", 'w') as f:\n"
      "    print(glo, file=f)"))
    (write-region (point-min) (point-max) steamlaunchoptions_py)
    (set-file-modes steamlaunchoptions_py #o755)))

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
