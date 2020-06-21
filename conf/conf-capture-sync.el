;;; init-editing-utils.el --- Day-to-day editing helpers -*- lexical-binding: t -*-

;;; Commentary:

;; reqires sync-org-path variable
;; Example: (setq sync-org-path "~/Documents/SyncOrg/")


;;; TODO:
;; - completly redo capture templates

;;; Code:


(when (boundp 'sync-org-path)
  (let
      ((my-org-agenda-files
        (list
         "tasks.org"
         "work.org"
         "univer.org"
         "periodic.org"
         "goals.org")))
    (setq org-agenda-files
          (mapcar (lambda (x) (concat sync-org-path x)) my-org-agenda-files))
    ;; code
    ;; (let ((org-super-agenda-groups
    ;;        '((:auto-category t))))
    ;;   (org-agenda-list))

    (setq org-capture-templates
          `(("t" "Todo" entry (file+headline ,(concat sync-org-path "tasks.org") "Общее")
             "* TODO %?\n   SCHEDULED: %T")
            ("w" "Todo work" entry (file+headline ,(concat sync-org-path "work.org") "Общее")
             "* TODO %?\n   SCHEDULED: %T")
            ("u" "Todo univer" entry (file+headline ,(concat sync-org-path "univer.org") "Общее")
             "* TODO %?\n   SCHEDULED: %T")
            ("r" "Work Report" entry (file+olp+datetree ,(concat sync-org-path "reports/work-report.org"))
             "* %T %?")
            ("j" "Journal" entry (file+olp+datetree ,(concat sync-org-path "journal/j.org"))
             "* %T\n%?")
            ("n" "Work notes" entry (file+olp+datetree ,(concat sync-org-path "notes/notes.org"))
             "* %T\n%?")
            ("c" "Code" entry (file+headline ,(concat sync-org-path "capture/code.org") "ОБЩЕЕ")
             "* %T\n%?")))
    
    ))

(global-set-key (kbd "C-c c") 'org-capture)

(provide 'conf-capture-sync)
;;; init-editing-utils.el ends here
