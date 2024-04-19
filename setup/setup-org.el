(with-eval-after-load 'org
  (require 'epa-file)

  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages
           '((python . t)))))
(setq-default
 org-confirm-babel-evaluate nil
 org-modules
    '(
; org-w3m ; 
; org-bbdb ; BBDB is a rolodex-like database program for GNU Emacs
;org-bibtex ; Org links to BibTeX entries
      org-datetree ; create date entries in a tree
      ;;org-docview
;org-gnus ; support for links to gnus
; org-info ; Support for links to Info nodes from within Org-Mode
; org-irc ;
; org-mhe ;
; org-rmail
      ))


;; prose mode

(defun remfils/on-word-theme()
  (interactive)
  (setq
   truncate-lines nil
   word-wrap t
   cursor-type 'bar
   org-cycle-separator-lines 0
   )
  (setq-local
   blink-cursor-interval 0.6
   show-trailing-whitespace nil
   line-spacing 0.2
   electric-pair-mode nil)
  (disable-theme remfils/current-theme-value)
  (load-theme 'leuven t)
  )

(defun remfils/off-word-theme()
  (interactive)
  (disable-theme 'leuven)
  (load-theme remfils/current-theme-value t)
  
  (kill-local-variable 'truncate-lines)
  (kill-local-variable 'word-wrap)
  (kill-local-variable 'cursor-type)
  (kill-local-variable 'blink-cursor-interval)
  (kill-local-variable 'show-trailing-whitespace)
  (kill-local-variable 'line-spacing)
  (kill-local-variable 'electric-pair-mode)
)

(setq-default
 remfils/current-theme-value nil)

(with-eval-after-load 'setup-theme
  (setq remfils/current-theme-value (car custom-enabled-themes)))

(define-minor-mode prose-mode
  "Set up a buffer for prose editing.
This enables or modifies a number of settings so that the
experience of editing prose is a little more like that of a
typical word processor."
  :global nil
  :keymap nil
  :lighter " Prose"
  (if prose-mode
      (progn
        (when (fboundp 'writeroom-mode)
          (writeroom-mode 1))

        (remfils/on-word-theme)
        
        (when (eq major-mode 'org)
          (kill-local-variable 'buffer-face-mode-face))
        (buffer-face-mode 1)
        ;;(delete-selection-mode 1)
        
        (set-fringe-style 2)
        (set-frame-parameter (window-frame) 'background-mode 'dark)
        ;; (ignore-errors (flyspell-mode 1))
        (visual-line-mode 1)

        ;; setup window margins

        (setq
         left-margin-width 50
         right-margin-width 50)
        (set-window-buffer nil (window-buffer))

        )
    (buffer-face-mode -1)
    ;; (delete-selection-mode -1)
    ;; (flyspell-mode -1)
    (visual-line-mode -1)

    (set-fringe-style nil)

    ;; revert theme

    (remfils/off-word-theme)

    ;; center window

    (setq left-margin-width nil)
    (setq right-margin-width nil)
    (set-window-buffer nil (current-buffer))

    ;; (toggle-frame-fullscreen)
    ))

(defun remfils/highlight-agenda-lines()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "день рожд:" nil t)
      (goto-char (match-end 0))
      (skip-syntax-forward " " (point-at-eol))
      (forward-char -1)
      (add-text-properties (1- (match-beginning 0)) (point)
                           '(face remfils/agenda-face/birthday)))

    (goto-char (point-min))
    (while (re-search-forward "track:" nil t)
      (add-text-properties (match-beginning 0) (match-end 0)
                           '(face remfils/agenda-face/track)))

    (goto-char (point-min))
    (while (re-search-forward "event-log:" nil t)
      (search-forward "┄┄┄┄┄ " (point-at-eol))
      (add-text-properties (point) (point-at-eol)
                           '(face remfils/agenda-face/event-log)))

    (goto-char (point-min))
    (while (re-search-forward "wo:" nil t)
      (goto-char (match-end 0))
      (skip-syntax-forward " " (point-at-eol))
      (forward-char -1)
      (add-text-properties (1- (match-beginning 0)) (point)
                           '(face remfils/agenda-face/workout)))

    (goto-char (point-min))
    (while (re-search-forward "project:" nil t)
      (goto-char (match-end 0))
      (skip-syntax-forward " " (point-at-eol))
      (forward-char -1)
      (add-text-properties (1- (match-beginning 0)) (point)
                           '(face remfils/agenda-face/project)))

    (goto-char (point-min))
    (while (re-search-forward "вакансия:" nil t)
      (goto-char (match-end 0))
      (skip-syntax-forward " " (point-at-eol))
      (forward-char -1)
      (add-text-properties (1- (match-beginning 0)) (point)
                           '(face remfils/agenda-face/warn)))

    (goto-char (point-min))
    (while (re-search-forward " TODO " nil t)
      (add-text-properties (1+ (match-beginning 0)) (1- (match-end 0))
                           '(face remfils/agenda-face/todo)))
    ))
(add-hook 'org-agenda-finalize-hook 'remfils/highlight-agenda-lines)

(require 'org)
(provide 'setup-org)
