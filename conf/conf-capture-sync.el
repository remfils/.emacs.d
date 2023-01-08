;;; init-editing-utils.el --- Day-to-day editing helpers -*- lexical-binding: t -*-

;;; Commentary:

;; reqires sync-org-path variable
;; Example: (setq sync-org-path "~/Documents/SyncOrg/")


;;; Code:

(let
    ((remfils/task-file-location "~/refile.org")
     (remfils/journal-file-location "~/refile.org")
     (remfils/event-log-file-location "~/refile.org")
     (remfils/code-file-location "~/refile.org"))

  (when (boundp 'sync-org-path)

    (setq remfils/task-file-location (concat sync-org-path "refile.org"))
    (setq remfils/journal-file-location (concat sync-org-path "refile.org"))
    (setq remfils/event-log-file-location (concat sync-org-path "refile.org"))
    (setq remfils/code-file-location (concat sync-org-path "refile.org"))

    (let
        ((my-org-agenda-files
          (list
           "refile.org"
           "event-calendar.org")))
      (setq org-agenda-files
            (mapcar (lambda (x) (concat sync-org-path x)) my-org-agenda-files))))


  (setq remfils/task-file-location (list remfils/task-file-location))
  (setq remfils/journal-file-location (list remfils/journal-file-location))
  (setq remfils/event-log-file-location (list remfils/event-log-file-location))
  (setq remfils/code-file-location (list remfils/code-file-location))

  (setq org-capture-templates
        `(
          ("t" "Todo" entry (file+headline ,@remfils/task-file-location "Tasks")
           "** TODO %?\n%T")
          ("e" "Event log" entry (file+headline ,@remfils/event-log-file-location "Event logs")
           "* %T%?       :elog:\n:PROPERTY:\n:CATEGORY: event-log\n:END:\n")
          ("j" "Journal" entry (file+headline ,@remfils/journal-file-location "Journal")
           "** %?\n:PROPERTY:\n:CATEGORY: journal\n:END:\n# дата: %T\n")
          ("c" "Code" entry (file+headline ,@remfils/code-file-location "Code")
           "** %?       :LANG:\n:PROPERTY:\n:CATEGORY: code\n:END:\n# дата: %T\n"))
        ))



(global-set-key (kbd "C-c c") 'org-capture)

(provide 'conf-capture-sync)
;;; init-editing-utils.el ends here
