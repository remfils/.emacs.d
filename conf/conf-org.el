;;; conf-org.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:


;;; Todo:
;; - study http://doc.norang.ca/org-mode.html

(require 'org)
(require 'epa-file)
(epa-file-enable)

(require-package 'org-cliplink)

(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)

(define-key global-map (kbd "C-c k c") 'epa-encrypt-region)
(define-key global-map (kbd "C-c k d") 'epa-decrypt-region)

(after-load
    'org-agenda
  (add-hook
   'org-agenda-mode-hook
   (lambda () (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t))))

(setq org-odt-category-map-alist
      '(("__Figure__" "Изображение" "value" "Изображение" org-odt--enumerable-image-p)))

;; modules

(add-hook
 'after-init-hook
 (lambda ()
   ;; remove all the modules
   (setq
    org-modules
    '(
; org-w3m ; 
; org-bbdb ; BBDB is a rolodex-like database program for GNU Emacs
;org-bibtex ; Org links to BibTeX entries
      org-datetree ; create date entries in a tree
      org-docview
;org-gnus ; support for links to gnus
; org-info ; Support for links to Info nodes from within Org-Mode
; org-irc ; 
; org-mhe ; 
; org-rmail
      ))))

;; configure load languages

(org-babel-do-load-languages
 'org-babel-load-languages
 (append org-babel-load-languages
         '((python . t))))


(setq org-priority-faces '((?A . (:foreground "#F0DFAF" :weight bold))
                           (?B . (:foreground "LightSteelBlue"))
                           (?C . (:foreground "OliveDrab"))))

(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "CANCELED(c)")))

(setq org-todo-keyword-faces
      '(("TODO" . org-warning) ("WAIT" . "orange")
        ("CANCELED" . (:foreground "#ffb6b2" :weight bold))))

(setq org-confirm-babel-evaluate nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prose-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-golden-ratio-margin()
  (let
      ((width-ratio (/ 1 1.61803399))
       (window-width (window-total-width)))
    (round (/ (* window-width (- 1 width-ratio)) 2))))

(defun get-margin-by-document-width-ratio (document-width-ratio)
  "returns margin width for centered document layout")

(let
    ((current-theme (car custom-enabled-themes)))
  (define-minor-mode prose-mode
    "Set up a buffer for prose editing.
This enables or modifies a number of settings so that the
experience of editing prose is a little more like that of a
typical word processor."
    nil " Prose" nil
    (if prose-mode
        (progn
          (when (fboundp 'writeroom-mode)
            (writeroom-mode 1))
          (setq truncate-lines nil)
          (setq word-wrap t)
          (setq cursor-type 'bar)
          (when (eq major-mode 'org)
            (kill-local-variable 'buffer-face-mode-face))
          (buffer-face-mode 1)
          ;;(delete-selection-mode 1)
          (setq-local blink-cursor-interval 0.6)
          (setq-local show-trailing-whitespace nil)
          (setq-local line-spacing 0.2)
          (setq-local electric-pair-mode nil)
          (set-fringe-style 2)
          (set-frame-parameter (window-frame) 'background-mode 'dark)
          ;; (ignore-errors (flyspell-mode 1))
          (visual-line-mode 1)

          ;; go fullscreen

          ;; (toggle-frame-fullscreen)

          ;; change theme

          (disable-theme current-theme)
          (load-theme 'flatui t)
          
          ;; setup window margins

          ;; (setq left-margin-width (get-golden-ratio-margin))
          ;; (setq right-margin-width (get-golden-ratio-margin))
          ;; (set-window-buffer nil (current-buffer))
          
          )
      (kill-local-variable 'truncate-lines)
      (kill-local-variable 'word-wrap)
      (kill-local-variable 'cursor-type)
      (kill-local-variable 'blink-cursor-interval)
      (kill-local-variable 'show-trailing-whitespace)
      (kill-local-variable 'line-spacing)
      (kill-local-variable 'electric-pair-mode)
      (buffer-face-mode -1)
      ;; (delete-selection-mode -1)
      ;; (flyspell-mode -1)
      (visual-line-mode -1)

      (set-fringe-style nil)

      ;; revert theme

      (disable-theme 'flatui)
      (load-theme current-theme t)

      ;; center window
      
      (setq left-margin-width nil)
      (setq right-margin-width nil)
      (set-window-buffer nil (current-buffer))

      ;; (toggle-frame-fullscreen)
      )))


(let ((fringe-default-foreground (face-foreground 'fringe))
      (fringe-default-background (face-background 'fringe)))

  (add-hook
   'org-clock-in-hook
   (lambda ()
     (set-face-attribute
      'fringe nil
      :foreground (face-foreground 'org-clock-overlay)
      :background (face-background 'org-clock-overlay))))
  (add-hook
   'org-clock-out-hook
   (lambda ()
     (set-face-attribute
      'fringe nil
      :foreground fringe-default-foreground
      :background fringe-default-background))))


(provide 'conf-org)
;;; init-org.el ends here
