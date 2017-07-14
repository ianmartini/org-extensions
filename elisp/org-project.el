(require 'org)

(defconst *project-messages* "*project-messages*")

(defun dos2unix (str) (replace-regexp-in-string "\r" "" str))

(defun project-clear-messages ()
  (with-current-buffer (get-buffer-create *project-messages*)
    (erase-buffer)))

(defun project-message-impl (string)
  (with-current-buffer (get-buffer-create *project-messages*)
    (save-excursion
      (goto-char (point-max))
      (insert (dos2unix string) "\n"))))

(defsubst ts ()
  (format-time-string "%H:%M:%S" nil t))

(defun project-message (str &rest vars)
  (project-message-impl
   (apply #'format (cons (concat "[ " (ts) " ] " str) vars))))

(defun project-message-no-ts (str &rest vars)
  (project-message-impl (apply #'format (cons str vars))))

;; --

(defstruct (project-sublist-struct (:conc-name sublist/))
  parent
  tasks)

(defun project-sublist-struct-new (parent)
  (let ((var (make-project-sublist-struct)))
    (setf (sublist/parent var) parent)
    var))

(defstruct (project-struct (:conc-name project/))
  all-tasks
  all-task-names
  sublists)

(defun project-struct-new ()
  (let ((var (make-project-struct)))
    (setf (project/all-tasks var) (make-hash-table))
    var))

(defvar *project* (project-struct-new))

(defun project-task-fullname (task)
  (concat (task/parent task) "/" (task/name task)))

(defun project-add-task (task)
  (let ((key (project-task-fullname task)))
    (push key (project/all-task-names *project*))
    (puthash (intern key) task (project/all-tasks *project*))))

(defun project-get-task (fullname)
  (gethash (intern fullname) (project/all-tasks *project*)))

(defstruct (project-task-struct (:conc-name task/))
  parent
  name
  start-date
  end-date
  days
  dependency)

(defun project-task-new (parent name)
  (let ((var (make-project-task-struct)))
    (setf (task/parent var) parent)
    (setf (task/name var) name)
    var))

;; ---

(defun get-headline ()
  (plist-get (cadr (org-element-headline-parser (point-max))) :raw-value))

(defun is-task-p ()
  (member "task" (org-get-local-tags)))

(defsubst qs (str)
  (concat "\"" str "\""))

(defun project-build-project-structure ()
  (interactive)
  (let (parent sublist headline tags)
    (setq *project* (project-struct-new))

    (org-map-entries
     (lambda ()
       (setq headline (get-headline))
       (project-message "headline: %s" headline)

       (if (is-task-p)
           (let ((task (project-task-new parent headline)) properties days)
             (setq properties (org-entry-properties))
             (setf (task/start-date task) (cdr (assoc "start-date" properties)))
             (setf (task/dependency task) (cdr (assoc "dependency" properties)))

             (setq days (cdr (assoc "days" properties)))
             (setf (task/days task) (if days (string-to-number days) nil))
             (project-add-task task)

             (unless sublist
               (setq sublist (project-sublist-struct-new parent))
               (push sublist (project/sublists *project*)))
             (push task (sublist/tasks sublist)))
         (setq parent headline)
         (when sublist
           (setf (sublist/tasks sublist) (reverse (sublist/tasks sublist)))
           (setq sublist nil))))
     "+tasks" 'file)

    ;; Reverse the final set of tasks
    (when sublist (setf (sublist/tasks sublist) (reverse (sublist/tasks sublist))))
    
    (setf (project/all-task-names *project*) (reverse (project/all-task-names *project*)))
    (setf (project/sublists *project*) (reverse (project/sublists *project*)))
    (project-message "*sublists: %s" (project/sublists *project*))))

(defun project-date-add-days (date days)
  "Add 'days' number of weekdays to 'date'. E.g. 12 days is 2 weeks and 2 days.
Ensure that the result is always a weekday.
This means that anything landing on Saturday or Sunday will be moved to the next Monday."
  (unless date
    (error "(project-date-add-days ...) invalid date"))
  (let (new-time)
    (when (>= days 5)
      (setq days (+ (mod days 5) (* 7 (floor days 5)))))
    (setq new-time (time-add (org-time-string-to-time date)
                             (seconds-to-time (* days 3600 24))))
    (let ((weekday (string-to-number (format-time-string "%u" new-time))))
      (when (> weekday 5)
        (setq new-time (time-add new-time
                                 (seconds-to-time (* 3600 24 (- 2 (mod weekday 2))))))))
    (format-time-string "%Y-%m-%d" new-time)))

(defun error-if-days-null (task days)
  (unless days (error "task [%s] does not have a 'days' property"
                      (project-task-fullname task))))

(defun project-set-dependent-dates (task)
  (let (dependency dependency-days dependency-start-date)
    (setq dependency (project-get-task (task/dependency task)))
    (setq dependency-days (task/days dependency))
    (error-if-days-null dependency dependency-days)
    (setq dependency-start-date (task/start-date dependency))
    (if (not dependency-start-date)
        nil
      (setf (task/start-date task)
            (project-date-add-days
             (project-date-add-days dependency-start-date dependency-days) 1))
      (setf (task/end-date task)
            (project-date-add-days (task/start-date task) (1- days)))))
  t)

(defun project-calculate-dates ()
  (interactive)
  (let ((continue t) changed date end-date)
    (while continue
      (setq changed nil)
      (dolist (sublist (project/sublists *project*))
        (setq date nil)

        (dolist (task (sublist/tasks sublist))
          (setq days (task/days task))
          (error-if-days-null task days)

          (if (and (task/dependency task) (not (task/end-date task)))
              (setq changed (project-set-dependent-dates task))
            (if date
                (unless (task/start-date task)
                  (setf (task/start-date task) date)
                  (setq changed t))
              (setq date (task/start-date task)))
            (when (and (not (task/end-date task)) date)
              (setf (task/end-date task) (project-date-add-days date (1- days)))
              (setq changed t)))

          (when (task/end-date task)
            (setq date (project-date-add-days (task/end-date task) 1)))))

      (unless changed (setq continue nil)))))

(defun project-set-date-and-days (prefix)
  (interactive "P")
  (let (quoted-headline properties orig-start-date days start-date)
    (setq quoted-headline (qs (get-headline)))
    (setq properties (org-entry-properties))
    (if (not prefix)
        (org-entry-delete (point) "start-date")
      (setq orig-start-date (cdr (assoc "start-date" properties)))
      (setq start-date (org-read-date nil nil nil
                                      (format "Start date for %s: " quoted-headline)
                                      (if (not orig-start-date) nil
                                        (org-time-string-to-time orig-start-date))))
      (org-entry-put (point) "start-date" start-date))
    (setq days (read-from-minibuffer
                (format "Days required for %s: " quoted-headline)
                (or (cdr (assoc "days" properties)) "0")))
    (org-entry-put (point) "days" days)))

(defun project-set-days ()
  (interactive)
  (let (quoted-headline properties days)
    (setq quoted-headline (qs (get-headline)))
    (setq properties (org-entry-properties))
    (setq days (or (cdr (assoc "days" properties)) "0"))
    (setq days (read-from-minibuffer
                (format "Days required for %s: " quoted-headline)
                days))
    (org-entry-put (point) "days" days)))

(defun project-set-dependency ()
  (interactive)
  (project-build-project-structure)
  (let ((dependency (ido-completing-read "Dependency: "
                                         (project/all-task-names *project*)
                                         nil t)))
    (org-entry-put (point) "dependency" dependency)))

(defun project-days-str (days)
  (if (= 1 days) "1 day " (format "%s days" days)))

(defvar project-dblock-pos)

(defun org-dblock-write:project-schedule (params)
  (setq project-dblock-pos (point))
  (dolist (sublist (project/sublists *project*))
    (insert "*** " (sublist/parent sublist))
    (insert "\n    :PROPERTIES:")
    (insert "\n    :VISIBILITY: children")
    (insert "\n    :END:\n")
    (dolist (task (sublist/tasks sublist))
      (let (start-date)
        (setq start-date
              (format-time-string "%Y-%m-%d %a"
                                  (org-time-string-to-time (task/start-date task))))
        (insert (format "***** (%s / %s) %s\n"
                        (project-days-str (task/days task)) start-date (task/name task)))
        (org-deadline nil (task/end-date task)))))
  (insert "*** [END SCHEDULE]"))

(defun project-update-schedule ()
  (interactive)
  (project-build-project-structure)
  (project-calculate-dates)
  (org-update-all-dblocks)
  (goto-char project-dblock-pos)
  (outline-up-heading project-dblock-pos)
  (save-excursion
    (hide-subtree)
    (show-children)
    (while (and (outline-next-heading)
                (not (equal (get-headline) "[END SCHEDULE]")))
      (show-children))))

;; Key map

(defvar project-keymap (make-sparse-keymap))
(define-key project-keymap "d" 'project-set-date-and-days)
(define-key project-keymap "p" 'project-set-dependency)
(define-key project-keymap "s" 'project-update-schedule)

(define-key org-mode-map (kbd "C-c p") project-keymap)

(provide 'project)
