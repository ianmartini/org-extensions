(require 'org)

(define-key global-map "\C-ca" 'org-agenda)

(setq org-use-speed-commands t)
(setq org-speed-commands-user nil)

(setq org-todo-kewords
      '((type "TODO(t)" "PROGRESS(p)" "WAIT(w)" "WAITME(m)" "REVIEW(r)" "|"
              "QA(q)" "DONE(d)" "CANCELLED(c)" "DELEGATED(g)")))

(setq org-agenda-custom-commands
      '(("j" tags "+currentSprint")
        ("b" tags "+bugfix")))

(setq org-refile-targets '((nil . (:maxlevel . 2))))

(provide 'org-config)
