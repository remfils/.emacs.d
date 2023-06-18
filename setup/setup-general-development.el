(require 'unfill)
(require 'expand-region)
(require 'multiple-cursors)
(require 'rainbow-delimiters)
(require 'hl-todo)
(require 'company)
(require 'emmet-mode)

;; (require 'js2-mode)
;; (require 'json-mode) ;;

;;;; slow
;; 


;; packages


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

(setq-default
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


(electric-pair-mode 1)
(delete-selection-mode 1) ;; overwrite on paste

(defun remfils/prog-mode-hook()
  (rainbow-delimiters-mode)
  (hl-todo-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remfils/lisp-mode-hook()
  (company-mode))

(add-hook 'emacs-lisp-mode-hook 'remfils/lisp-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

(when (equal system-type 'windows-nt)
  (prefer-coding-system 'utf-8)
  (setenv "PYTHONIOENCODING" "utf-8"))

(setq-default python-indent-guess-indent-offset t)
(setq-default python-indent-guess-indent-offset-verbose nil)

(defun remfils/python-mode-hook()
  (hack-local-variables)
  (company-mode)
  ;;;; TODO: lsp example
  ;; (when remfils-start-lsp?
  ;;   (lsp-mode))
  )

(with-eval-after-load 'python
  (add-hook 'python-mode-hook 'remfils/python-mode-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; web
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default web-mode-enable-auto-indentation nil)
(setq-default php-mode-template-compatibility nil)

(defun remfils/html-mode()
  (company-mode)
  (emmet-mode))

(add-hook 'sgml-mode-hook 'remfils/html-mode)
(add-hook 'css-mode-hook  'remfils/html-mode)
(with-eval-after-load 'web-mode
  (add-hook 'web-mode-hook 'remfils/html-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; php
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remfils/php-mode-hook()
  (local-set-key [f5] #'web-mode)

  (company-mode)
  ;; TODO: if ide run ide
  )

(defun remfils/web-mode-hook()
  (local-set-key [f5] #'php-mode)

  (company-mode)
  ;; TODO: if ide run ide
  )

(with-eval-after-load 'php-mode
  (add-hook 'php-mode-hook 'remfils/php-mode-hook))

(with-eval-after-load 'web-mode
  (add-hook 'web-mode-hook 'remfils/web-mode-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; javascript
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default
 js2-bounce-indent-p nil
 js2-highlight-level 3
 js-indent-level 2
 js2-basic-offset 2
 )

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(put 'js2-basic-offset 'safe-local-variable #'integerp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general ide
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: when to run ide?

(setq-default
 remfils/is-ide-enabled? nil)
(put 'remfils-start-lsp? 'safe-local-variable #'booleanp)

(defun remfils/setup-ide()
  (interactive)
  (require 'lsp)
  (require 'flycheck)
  (require 'yasnippet)
  (require 'magit)

  (global-flycheck-mode)
  (global-company-mode)
  (yas-global-mode)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook
 'prog-mode-hook 'remfils/prog-mode-hook)

(add-hook 'after-init-hook 'global-auto-revert-mode)


(provide 'setup-general-development)
