(require 'url)
(require 'json)
(require 'ivy)
(require 'ivy-rich)
(require 'steaminfo)

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
                              (async-shell-command (concat "/bin/bash " steamlauncher_sh " " (format "%s" (match-string 2 game))))
                            nil))))))

(provide 'counsel-steam-recentf)
