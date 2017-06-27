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

;; (defmacro project-message (str &rest vars)
;;   `(project-message-impl (format (concat "[ " (ts) " ] " ,str) ,@vars)))
;;
;; (defmacro project-message-no-ts (str &rest vars)
;;   `(project-message-impl (format ,str ,@vars)))

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
  sublists)

(defun project-struct-new ()
  (let ((var (make-project-struct)))
    (setf (project/all-tasks var) (make-hash-table))
    var))

(defvar *project* (project-struct-new))

(defun project-add-task (task)
  (let ((key (concat (task/parent task) "/" (task/name task))))
    (puthash (intern key) task (project/all-tasks *project*))))

(defun project-get-task (parent name)
  (gethash (intern (concat parent "/" name))
           (project/all-tasks *project*)))

(defstruct (project-task-struct (:conc-name task/))
  parent
  name
  start-time
  end-time
  calculated-end-time
  days
  dependency)

(defun project-task-new (parent name)
  (let ((var (make-project-task-struct)))
    (setf (task/parent var) parent)
    (setf (task/name var) name)
    var))

;; ---

(defun get-kv (key list)
  (let (retval)
    (while list
      (if (not (eq key (car list)))
          (setq list (cdr list))
        (setq retval (cons (car list) (cadr list)))
        (setq list nil)))
    retval))

(defun get-headline ()
  (cdr (get-kv :raw-value
               (cadr (org-element-headline-parser (point-max))))))

(defun is-task-p ()
  (member "task" (org-get-local-tags)))

(defun project-build-project-structure ()
  (interactive)
  (let (parent sublist headline tags)
    (setq *project* (project-struct-new))
    (org-map-entries

     (lambda ()
       (setq headline (get-headline))
       (project-message "headline: %s" headline)

       (if (is-task-p)
           (let ((task (project-task-new parent headline)))
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
    (setf (project/sublists *project*) (reverse (project/sublists *project*)))
    (project-message "*sublists: %s" (project/sublists *project*))))

(provide 'project)
