;;; init-editing-utils.el --- Day-to-day editing helpers -*- lexical-binding: t -*-

;;; Commentary:

;; reqires sync-org-path variable
;; Example: (setq sync-org-path "~/Documents/SyncOrg/")


;;; TODO:
;; - completly redo capture templates

;;; Code:

(let
    ((remfils/task-file-location "~/refile.org")
     (remfils/journal-file-location "~/refile.org")
     (remfils/diary-file-location "~/refile.org")
     (remfils/notes-file-location "~/refile.org")
     (remfils/code-file-location "~/refile.org"))

  (when (boundp 'sync-org-path)

    ;;;; TODO: finish ok path
    (setq remfils/diary-file-location (concat sync-org-path "diary.org"))
    ;; (setq remfils/task-file-location (concat sync-org-path "tasks.org"))
    ;; (setq remfils/journal-file-location (concat sync-org-path "journal/j.org"))
    ;; (setq remfils/notes-file-location (concat sync-org-path "notes/notes.org"))
    ;; (setq remfils/code-file-location (concat sync-org-path "capture/code.org"))

    (let
        ((my-org-agenda-files
          (list
           "tasks.org"
           "work.org"
           "periodic.org"
           "goals.org")))
      (setq org-agenda-files
            (mapcar (lambda (x) (concat sync-org-path x)) my-org-agenda-files))))


  (setq remfils/task-file-location (list remfils/task-file-location))
  (setq remfils/diary-file-location (list remfils/diary-file-location))
  (setq remfils/journal-file-location (list remfils/journal-file-location))
  (setq remfils/notes-file-location (list remfils/notes-file-location))
  (setq remfils/code-file-location (list remfils/code-file-location))

  (setq org-capture-templates
        `(
          ("t" "Todo" entry (file+headline ,@remfils/task-file-location "Tasks")
           "* TODO %?\n   SCHEDULED: %T")
          ("d" "Diary" entry (file+headline ,@remfils/diary-file-location "ЕЖЕДНЕВНИК")
           "* %t\n  :PROPERTY:\n  :CATEGORY: diary\n  :END:\n\n*что сделать сегодня*\n\n- [ ] %?\n\n*что купить*\n\n- [ ]\n\n*по еде*\n\n- [ ]")
          ("j" "Journal" entry (file+headline ,@remfils/journal-file-location "Journal")
           "* %T\n  :PROPERTY:\n  :CATEGORY: journal\n  :END:\n%?")
          ("n" "Notes" entry (file+headline ,@remfils/notes-file-location "Notes")
           "* %T\n  :PROPERTY:\n  :CATEGORY: note\n  :END:\n%?")
          ("c" "Code" entry (file+headline ,@remfils/code-file-location "Code")
           "* %T\n  :PROPERTY:\n  :CATEGORY: code-note\n  :END:\n%?"))
        ))



(global-set-key (kbd "C-c c") 'org-capture)

(provide 'conf-capture-sync)
;;; init-editing-utils.el ends here
