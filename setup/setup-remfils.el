;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; templater
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remfils/templater/csv-line-to-list (csv-string)
  "parses line of text into list ignoring comas in quotes. "
  (let (result (csv-item "") (quote-counter 1))
    (dolist (csv-char (append csv-string nil) result)
      (cond
       ((char-equal csv-char ?\")
        (setq quote-counter (* quote-counter -1)))
       ((char-equal csv-char ?\,)
        (if (= quote-counter 1)
            (progn
              (setq result (append result (cons (string-trim csv-item) nil)))
              (setq csv-item ""))
          (setq csv-item (concat csv-item (char-to-string csv-char)))))
       (t (setq csv-item (concat csv-item (char-to-string csv-char))))))
    (setq result (append result (cons (string-trim csv-item) nil)))))

(defun remfils/templater/csv-text-to-list (csv-text)
  (let ((csv-line-list (split-string csv-text "\n" t)) result)
    (dolist (csv-line csv-line-list result)
      (setq result (append result (cons (remfils/templater/csv-line-to-list csv-line) nil))))))

(defun remfils/templater/generate-template-string-from-csv (template csv-text)
  (let ((var-rows (remfils/templater/csv-text-to-list csv-text)) result)
    (dolist (var-row var-rows result)
      (setq result (concat result (s-format template 'elt var-row))))))

(defun remfils/templater/generate-text-from-csv (template csv-text)
  (interactive "stemplate:\nscsv text:")
  (insert (remfils/templater/generate-template-string-from-csv template csv-text)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; image mover
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remfis/org-collect-images (dir-name)
  (interactive "sdir name:")
  (let
      ((new-img-dir (concat default-directory dir-name "/"))
       (images (remfils/grab-all-images-from-org-file)))
    (unless (file-exists-p new-img-dir)
      (make-directory new-img-dir))
    ;; copy found images
    (mapcar (lambda (old-img-url)
              (let* ((new-file-name (file-name-nondirectory old-img-url))
                     (new-img-url (concat new-img-dir new-file-name)))
                (copy-file old-img-url new-img-url)

                (let (
                      (old-url old-img-url)
                      (new-url (replace-regexp-in-string default-directory "./" new-img-url))
                      )
                  (goto-char (point-min))
                  (while (search-forward old-url nil t)
                    (replace-match new-url)))))
            images)))

(defun remfils/grab-all-images-from-org-file ()
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (string-match "\.png" (org-element-property :path link))
        (org-element-property :path link)))))


(defun remfils/open-workspace (dir orientation)
  (interactive "Ddir name:\ncorientation")
  (let ((n-file (concat (file-name-as-directory dir) "notes.org"))
        (t-file (concat (file-name-as-directory dir) "tasks.org")))

    (cond
     ((char-equal orientation ?3)
      (progn
        (set-frame-parameter nil 'fullscreen 'maximized)
        (find-file n-file)
        (outline-show-heading)
        (split-window-right)
        (find-file t-file)
        (outline-show-all)
        ))
     (t (progn
          (set-frame-parameter nil 'fullscreen 'maximized)
          (find-file n-file)
          (split-window-below)
          (find-file t-file)
          ))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; kill abs file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remfils/kill-abs-file-path()
  (interactive)
  (let ((filename
         (if (equal major-mode 'dired-mode)
             default-directory
           (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message filename))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; open file in debugger
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remfils/fedora/open-in-vs-code-current-file ()
  (interactive)
  (let ((filename
         (if (equal major-mode 'dired-mode)
             default-directory
           (buffer-file-name)))
        (linenumber (line-number-at-pos)))
    (when filename
      (async-shell-command (concat "code " filename ":" (number-to-string linenumber) " --goto")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; store file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar remfils/store-location "~/"
  "Location of store where copies of files are stored")
(put 'remfils/store-location 'safe-local-variable #'stringp)

(defun remfils/copy-file-to-store ()
  (interactive)
  (let* ((from-file-name (buffer-file-name (window-buffer (minibuffer-selected-window))))
        (to-file-name (concat (file-name-as-directory remfils/store-location) (file-name-nondirectory from-file-name))))
    (copy-file from-file-name to-file-name "overwrite")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; journal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remfils/create-journal-file ()
  (interactive)
  (let ((file-name (concat (format-time-string "%Y-%m-%d") "__.org")))
    (find-file file-name)
    (prose-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; capture to sync dir
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom remfils/sync/refile-org-path
  "~/refile.org"
  "refile org path"
  :group 'remfils
  :type '(string))

(defun remfils/capture/doc.org-path()
  (if (boundp 'remfils/capture/custom-doc.org)
        remfils/capture/custom-doc.org
      remfils/sync/refile-org-path))

(defun remfils/capture/doc.org-other-window()
  (interactive)
  (find-file-other-window (remfils/capture/doc.org-path)))
(put 'remfils/capture/custom-doc.org 'safe-local-variable #'stringp)

(defun remfils/capture/get-file-location__task()
  (if (boundp 'remfils/capture/project-file-location__task)
      remfils/capture/project-file-location__task
    (remfils/capture/doc.org-path)))
(put 'remfils/capture/project-file-location__task 'safe-local-variable #'stringp)

(defun remfils/capture/get-file-location__event()
  (if (boundp 'remfils/capture/project-file-location__event)
      remfils/capture/project-file-location__event
    (remfils/capture/doc.org-path)))
(put 'remfils/capture/project-file-location__event 'safe-local-variable #'stringp)

(defun remfils/capture/get-file-location__journal()
  (if (boundp 'remfils/capture/project-file-location__journal)
      remfils/capture/project-file-location__journal
    (remfils/capture/doc.org-path)))
(put 'remfils/capture/project-file-location__journal 'safe-local-variable #'stringp)

(defun remfils/capture/get-file-location__code()
  (if (boundp 'remfils/capture/project-file-location__code)
      remfils/capture/project-file-location__code
    (remfils/capture/doc.org-path)))
(put 'remfils/capture/project-file-location__code 'safe-local-variable #'stringp)



(defun remfils/set-agenda-and-refile-after-custom-load ()
  (setq org-agenda-files
          (list
           remfils/sync/refile-org-path))

  (setq org-capture-templates
        '(
          ("t" "Todo" entry (file+headline remfils/capture/get-file-location__task "Tasks")
           "** TODO %?\n%T")
          ("j" "Journal" entry (file+headline remfils/capture/get-file-location__journal "Journal")
           "** %?\n:PROPERTY:\n:CATEGORY: journal\n:END:\n# дата: %T\n")
          ("e" "Event log" entry (file+headline remfils/capture/get-file-location__event "Event logs")
           "* %T%?       :elog:\n:PROPERTY:\n:CATEGORY: event-log\n:END:\n")
          ("c" "Code" entry (file+headline remfils/capture/get-file-location__code "Code")
           "** %?       :LANG:\n:PROPERTY:\n:CATEGORY: code\n:END:\n# дата: %T\n"))
        )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; doc.org features
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; script runners
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remfils/insert-tinkoff-file (f)
  (interactive "fCsv file: ")
  
  (let ((current-buffer-position (mark-marker))
        (csv-parser-location (expand-file-name "~/.emacs.d/scripts/tinkoff-csv-parser.py")))
    (insert (shell-command-to-string (format "python \"%s\" \"%s\"" csv-parser-location (file-truename f))))
    (goto-char current-buffer-position)))


(provide 'setup-remfils)
