(setq-default
;; backups
 maake-backup-files t
 backup-directory-alist `(("." . "~/.saves"))
 backup-by-copying t
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t
 
 bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
 buffers-menu-max-size 30
 case-fold-search t
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

 global-auto-revert-non-file-buffers t
 auto-revert-verbose nil

 ;; locales
 system-time-locale "C"
 default-input-method "russian-computer"
 ispell-local-dictionary "ru"

 ;; dired
 dired-dwim-target t
 )


(electric-pair-mode 1)

(cua-selection-mode 1)

;; packages

(require 'unfill)
(require 'expand-region)
(require 'multiple-cursors)

;; (require 'tree-sitter)
;; (require 'tree-sitter-hl)
;; (require 'tree-sitter-langs)
;; (require 'tree-sitter-query)
;; (require 'tree-sitter-indent)

;; (eval-after-load
;;     'tree-sitter
;;   (progn
;;     (setq-default
;;      tree-sitter-major-mode-language-alist
;;      '((php-mode . php)
;;        (js2-mode . javascript)))
;;     (remfils/prog-end)))



(require 'lsp)

(delete-selection-mode 1) ;; overwrite on paste
(add-hook 'after-init-hook 'global-auto-revert-mode)

(require 'company)
(eval-after-load
    'company
  (progn
    (setq
     company-echo-delay 0
     company-tooltip-limit 20
     company-idle-delay 0.0
     company-minimum-prefix-length 1
     company-backends '(
                        company-bbdb
                        company-semantic
                        company-cmake
                        company-capf
                        company-clang
                        company-files
                        (company-dabbrev-code
                         company-gtags
                         company-etags
                         company-keywords)
                        company-oddmuse
                        )
     )
    (remfils/prog-end)
    ))

(require 'rainbow-delimiters)
(eval-after-load
    'rainbow-delimiters
  (progn
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
    (remfils/prog-end)))



;;;; test colors
;; TODO: this is note
;; DEBUG: this is note
;; NOTE: this is note
;; WARN: this is note
;; IMPORTANT: this is note
(require 'hl-todo)
(eval-after-load
    'hl-todo
  (progn
    (setq
     hl-todo-keyword-faces
     '(
       ("TODO"       . "#b22222")
       ("NOTE"       . "#228b22")
       ("DEBUG"      . "#ff0000")
       ("WARN"       . "#FF4500")
       ("IMPORTANT"  . "#d15fee")
       )
     )
    (add-hook 'prog-mode-hook 'hl-todo-mode)
    (remfils/prog-end)
    ))

(require 'flycheck)
(eval-after-load
    'flycheck
  (progn
    (global-flycheck-mode)
    (remfils/prog-end)))

(require 'yasnippet)
(eval-after-load
    'yasnippet
  (progn
    (yas-global-mode)
    (remfils/prog-end)
    ))

(require 'magit)

(provide 'setup-general-development)
