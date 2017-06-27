(defun path (&rest paths)
  (expand-file-name (mapconcat #'identity paths "/")))

(defconst *top* (path "~/dev"))

(defsubst path-org-elisp (&rest paths)
  (apply #'path *top* "org-extensions" "elisp" paths))

(defsubst path-orgfiles (&rest paths)
  (apply #'path *top* "org-extensions" "orgfiles" paths))

(add-to-list 'load-path (path-org-elisp))

(load "org-daily-schedule.el")

;; (load (buffer-file-name))
