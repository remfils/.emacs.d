;;; init-windows.el --- Working with windows within frames -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(require 'subr-x)
(require 's)

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
;; disable mouse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-minor-mode disable-mouse-mode
  "A minor-mode that disables all mouse keybinds."
  :global t
  :lighter " -M-"
  :keymap (make-sparse-keymap))

(dolist (type '(mouse down-mouse drag-mouse
                      double-mouse triple-mouse))
  (dolist (prefix '("" C- M- S- M-S- C-M- C-S- C-M-S-))
    ;; Yes, I actually HAD to go up to 7 here.
    (dotimes (n 7)
      (let ((k (format "%s%s-%s" prefix type n)))
        (define-key disable-mouse-mode-map
          (vector (intern k)) #'ignore)))))

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

(defun remfils/clear-subtree ()
  (interactive)
  (org-mark-subtree) ;; mark the current subtree
  (forward-line) ;; move point forward, so the headline isn't in the region
  (delete-region (region-beginning) (region-end)) ;; delete the rest
  )

(provide 'init-remfils)
;;; init-windows.el ends here
