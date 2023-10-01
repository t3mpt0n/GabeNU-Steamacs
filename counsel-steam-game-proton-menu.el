(require 'url)
(require 'json)
(require 'ivy)
(require 'ivy-rich)
(require 'steaminfo)

(defun steam-current-proton-version (id)
  (let ((file2parse (concat user-emacs-directory steamdir "/protonversions.json"))
        (json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'string))
    (with-temp-buffer
      (insert-file-contents file2parse)
      (goto-char (point-min))
      (let* ((json (json-read-from-string (buffer-string)))
             (pg (cdr (assoc "name" (cdr (assoc (number-to-string id) json))))))
        (message "%s" pg)))))

(steam-current-proton-version 70)
