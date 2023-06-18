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

        (disable-theme remfils/current-theme-value)
        (load-theme 'leuven t)

        ;; setup window margins

        (setq left-margin-width 50)
        (setq right-margin-width 50)
        (set-window-buffer nil (window-buffer))

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
    (load-theme remfils/current-theme-value t)

    ;; center window

    (setq left-margin-width nil)
    (setq right-margin-width nil)
    (set-window-buffer nil (current-buffer))

    ;; (toggle-frame-fullscreen)
    ))

(provide 'setup-org)
