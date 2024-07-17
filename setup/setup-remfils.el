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
;; timer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default
 remfils/temp-timer-buffer "*My Timer*"
 remfils/temp-timer-second-counter 0
 remfils/temp-timer nil
 )

(defun remfils/run-timer/format-timer(total-seconds)
  (let ((seconds (% total-seconds 60))
        (minutes (/ total-seconds 60)))
    (format "00:%02d:%02d" minutes seconds)
    ))

(defun remfils/run-timer/stop-timer()
    (cancel-timer remfils/temp-timer)
    (setq remfils/temp-timer nil))

(defun remfils/run-timer/update-timer-buffer()
  (let ((cbuffer (current-buffer)))
    (set-buffer remfils/temp-timer-buffer)
    (erase-buffer)
    (insert (remfils/run-timer/format-timer remfils/temp-timer-second-counter))
    (set-buffer cbuffer)
    ))

(defun remfils/run-timer()
  (setq remfils/temp-timer-second-counter (1+ remfils/temp-timer-second-counter))

  (if (get-buffer remfils/temp-timer-buffer)
      (remfils/run-timer/update-timer-buffer)
    (remfils/run-timer/stop-timer))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; image mover
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remfils/org-collect-images (dir-name)
  (interactive (list (read-string "image directory: " "imgs")))
  (let
      ((new-img-dir (concat default-directory dir-name "/"))
       (images (remfils/grab-all-images-from-org-file)))
    (unless (file-exists-p new-img-dir)
      (make-directory new-img-dir))
    ;; copy found images
    (mapcar (lambda (old-img-url)
              (when (not (string-prefix-p "./" old-img-url))
                (let* ((new-file-name (file-name-nondirectory old-img-url))
                       (new-img-url (concat new-img-dir new-file-name)))
                  (print (concat "found image: [" old-img-url "] moving to [" new-img-url "]"))
                  (copy-file old-img-url new-img-url t)

                  (let (
                        (old-url old-img-url)
                        (new-url (replace-regexp-in-string default-directory "./" new-img-url))
                        )
                    (goto-char (point-min))
                    (while (search-forward old-url nil t)
                      (replace-match new-url))))
                ))
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

(defcustom remfils/sync/calendar-file
  "~/event-calendar.org"
  "path to calendar"
  :group 'remfils
  :type '(string))

(defcustom remfils/sync/workout-file
  "~/workout.org"
  "path to workout"
  :group 'remfils
  :type '(string))

(defcustom remfils/sync/freelance-file
  "~/freelance.org"
  "path to freelance"
  :group 'remfils
  :type '(string))

(defcustom remfils/sync/work-now-file
  "~/work-now.org"
  "path to workout"
  :group 'remfils
  :type '(string))

(defun remfils/capture/doc.org-path()
  (if (boundp 'remfils/capture/custom-doc.org)
        remfils/capture/custom-doc.org
    remfils/sync/refile-org-path))

(defun remfils/capture/calendar-file-other-window()
  (interactive)
  (find-file-other-window (remfils/capture/get-file-location__event)))

(defun remfils/capture/doc.org-other-window()
  (interactive)
  (find-file-other-window (remfils/capture/doc.org-path)))
(put 'remfils/capture/custom-doc.org 'safe-local-variable #'stringp)

(defun remfils/capture/get-file-location__task()
  (remfils/capture/doc.org-path))

(defun remfils/capture/get-file-location__event()
  (if remfils/sync/calendar-file
      remfils/sync/calendar-file
    (remfils/capture/doc.org-path)))

(defun remfils/capture/get-file-location__journal()
  (remfils/capture/doc.org-path))

(defun remfils/capture/get-file-location__code()
  (remfils/capture/doc.org-path))


(defun remfils/set-agenda-and-refile-after-custom-load ()
  (setq org-agenda-files
          (list
           remfils/sync/refile-org-path
           remfils/sync/calendar-file
           remfils/sync/workout-file
           remfils/sync/freelance-file
           remfils/sync/work-now-file))

  (setq org-capture-templates
        '(
          ("t" "Todo" entry (file+headline remfils/capture/get-file-location__task "Tasks")
           "** TODO %?\n%T")
          ("j" "Journal" entry (file+headline remfils/capture/get-file-location__journal "Journal")
           "** %?\n%T\n")
          ("e" "Event log" entry (file+headline remfils/capture/get-file-location__event "Event logs")
           "* %?       :elog:\n%T\n")
          ("c" "Code" entry (file+headline remfils/capture/get-file-location__code "Code")
           "** %?       :LANG:\n%T\n\n#+begin_src LANG\n#+end_src\n"))
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


;; TODO: check if file exists
(defun remfils/c++/open-header-or-source-file()
  (interactive)
  (let* (
         (current-file (buffer-file-name))
         (current-extension (file-name-extension current-file)))
    (cond
     ((string= current-extension "h")
      (find-file-other-window (concat (substring current-file 0 -1) "cpp")))
     ((string= current-extension "cpp")
      (find-file-other-window (concat (substring current-file 0 -3) "h")))
     (t (print (concat "NO LUCK / " current-extension)))
     )))


(provide 'setup-remfils)
