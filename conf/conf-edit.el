;;; init-editing-utils.el --- Day-to-day editing helpers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'unfill)

(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))

(setq-default
 blink-cursor-interval 0.4
 bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
 buffers-menu-max-size 30
 case-fold-search t
 column-number-mode t
 delete-selection-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
 create-lockfiles nil
 auto-save-default nil
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil)

;; overwrite on paste
(delete-selection-mode 1)

(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(add-hook 'after-init-hook 'transient-mark-mode)

(setq system-time-locale "C")

(setq default-input-method "russian-computer")

(setq ispell-local-dictionary "ru")

(setq visible-bell 1)

;; large files
;; (require-package 'vlf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; white space
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default show-trailing-whitespace nil)

;(global-set-key [remap just-one-space] 'cycle-spacing)


;; expand region

(require-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; delimites

(require-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)


;; backups

(setq
 make-backup-files t
 backup-directory-alist `(("." . "~/.saves"))
 backup-by-copying t
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t
 )


;; test colors
;; TODO: this is note
;; DEBUG: this is note
;; NOTE: this is note
;; WARN: this is note
;; IMPORTANT: this is note

(require-package 'hl-todo)
(setq hl-todo-keyword-faces
      '(("TODO"   . "#b22222")
        ("NOTE"  . "#228b22")
        ("DEBUG"  . "#ff0000")
        ("WARN" . "#FF4500")
        ("IMPORTANT" . "#d15fee")))
(add-hook 'prog-mode-hook (lambda() (hl-todo-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; isearch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Search back/forth for the symbol at point
;; See http://www.emacswiki.org/emacs/SearchAtPoint
(defun isearch-yank-symbol ()
  "*Put symbol at current point into search string."
  (interactive)
  (let ((sym (thing-at-point 'symbol)))
    (if sym
        (progn
          (setq isearch-regexp t
                isearch-string (concat "\\_<" (regexp-quote sym) "\\_>")
                isearch-message (mapconcat 'isearch-text-char-description isearch-string "")
                isearch-yank-flag t))
      (ding)))
  (isearch-search-and-update))

(define-key isearch-mode-map "\C-\M-w" 'isearch-yank-symbol)


(defun sanityinc/isearch-exit-other-end ()
  "Exit isearch, but at the other end of the search string.
This is useful when followed by an immediate kill."
  (interactive)
  (isearch-exit)
  (goto-char isearch-other-end))

(define-key isearch-mode-map [(control return)] 'sanityinc/isearch-exit-other-end)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hotkeys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "M-Q") 'unfill-paragraph)

(global-set-key (kbd "C-j") 'newline)

(global-set-key (kbd "M-z") 'zap-up-to-char)

;; smarter ctrl-a

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require-package 'magit)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require-package 'multiple-cursors)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default dired-dwim-target t)

;(setq-default dired-listing-switches "-agho --group-directories-first")


(provide 'conf-edit)
;;; init-editing-utils.el ends here
