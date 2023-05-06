(require 'ivy)
(require 'ivy-rich)
(require 'steaminfo)

(defun counsel-steam-game-menu ()
  "Return a menu of all your Steam games in your library."
  (interactive)
  (let ((candids (steam-get-steaminfo-gamemenu)))
    (ivy-read "  Steam Library: " candids
              :action (lambda (game)
                        (let ((regexp "\\((\\([0-9]+\\))\\)"))
                          (if (string-match regexp game)
                              (async-shell-command (concat "/bin/bash " steamlauncher_sh " " (format "%s" (match-string 2 game))))
                            nil))))))

(provide 'counsel-steam-game-menu)
