(require 'url)
(require 'json)
(require 'ivy)
(require 'ivy-rich)

(defvar steam_account_id nil
  "Steam Account ID
  This is different from your `steam_id` which is your user ID.
  Like `steam_id` you can also get it at: https://steamdb.info/calculator")

(defun sec2last (regexp 2string)
  (save-excursion
    (let ((count 0))
      (while (and (search-backward-regexp regexp nil t)
                  (< count 1))
        (setq count (1+ count)))
      (when (= count 1)
        (replace-match 2string nil nil nil 0)))))

(defun parse-smlo (id)
  (let ((file2parse "/home/jd/.emacs.d/steam/launchoptions.json")
        (json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'string))
    (with-temp-buffer
      (insert-file-contents file2parse)
      (goto-char (point-min))
      ;(re-search-forward "^$")
      (let* ((json (json-read-from-string (buffer-string)))
             (lo (cdr (assoc "LaunchOptions" (cdr (assoc (number-to-string id) json))))))
        (message "%s" lo)))))

(defun parse-smlo-from-string (str)
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'string))
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (re-search-forward "^$")
      (sec2last ",}" "}")
      (message "%s" (buffer-string)))))

(defun parse-smlo-full ()
  (let ((file2parse "/home/jd/.emacs.d/steam/launchoptions.json")
        (json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'string))
    (with-temp-buffer
      (insert-file-contents file2parse)
      (goto-char (point-min))
      (re-search-forward "^$")
      (sec2last ",}" "}")
      (message "%s" (buffer-string)))))

(defun counsel-steam-game-mod-launchopt ()
  (interactive)
  (let ((candids (steam-get-steaminfo-gamelist)))
    (ivy-read "Game List: " candids
              :action (lambda (game)
                        (let ((regexp "\\((\\([0-9]+\\))\\)"))
                          (if (string-match regexp game)
                              (let* ((id (format "%s" (match-string 2 game)))
                                     (launchopts (parse-smlo (string-to-number id)))
                                     (prompt (concat "\"" (read-string "Launch Options: " launchopts) "\"")))
                                (async-shell-command (concat "python ~/.local/src/steamacs/steammodlaunchopt.py " id " " prompt))))
                            nil)))))


