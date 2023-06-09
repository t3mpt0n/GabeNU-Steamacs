(require 'url)
(require 'json)
(require 'ivy)
(require 'ivy-rich)
(require 'steaminfo)

(defun sec2last (regexp 2string)
  (save-excursion
    (let ((count 0))
      (while (and (search-backward-regexp regexp nil t)
                  (< count 1))
        (setq count (1+ count)))
      (when (= count 1)
        (replace-match 2string nil nil nil 0)))))

(defun parse-smlo (id)
  (let ((file2parse (concat user-emacs-directory steamdir "/launchoptions.json"))
        (json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'string))
    (with-temp-buffer
      (insert-file-contents file2parse)
      (goto-char (point-min))
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
  (let ((file2parse (concat user-emacs-directory steamdir "/launchoptions.json"))
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
  (let ((candids (steam-get-steaminfo-gamemenu)))
    (ivy-read "  Launch Options: " candids
              :action (lambda (game)
                        (let ((regexp "\\((\\([0-9]+\\))\\)"))
                          (if (string-match regexp game)
                              (let* ((id (format "%s" (match-string 2 game)))
                                     (launchopts (parse-smlo (string-to-number id)))
                                     (prompt (concat "\"" (read-string "Launch Options: " launchopts) "\"")))
                                (async-shell-command (concat "python " steamlaunchoptions_py " " id " " prompt))))
                            nil)))))

(provide 'counsel-steam-game-mod-launchopt)
