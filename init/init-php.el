;;; init-php.el --- Support for working with PHP -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'php-mode)

;; todo: configure run init tests command
;; todo: configure TAGS file generation
;; todo: emmet mode maybe?
;; todo: some basic flycheck config



;; TODO: move to separate file

;; test colors
;; TODO: this is note
;; DEBUG: this is note
;; NOTE: this is note
;; WARN: this is note

(require-package 'hl-todo)
(setq hl-todo-keyword-faces
      '(("TODO"   . "#b22222")
        ("NOTE"  . "#228b22")
        ("DEBUG"  . "#ff0000")
        ("WARN" . "#FF4500")))
(add-hook 'prog-mode-hook (lambda() (hl-todo-mode)))

(require-package 'company-php)
;; (after-load 'company
;;             (push 'company-ac-php-backend company-backends))

(require-package 'lsp-mode)

(add-hook
 'php-mode-hook
 (lambda()
   (setq-local
    lsp-clients-php-server-command (quote ("php" "/home/remfils/Projects/tresio/vendor/felixfbecker/language-server/bin/php-language-server.php")))
   (company-mode)
   (lsp)
   ))

(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)

;; (add-hook
;;  'hack-local-variables-hook
;;  (lambda () (when (derived-mode-p 'php-mode) (lsp) (define-key lsp-mode-map (kbd "C-x C-l") lsp-command-map))))

;; (add-hook 'hack-local-variables-hook (lambda () (when (derived-mode-p 'php-mode) (lsp))))


;; (setq lsp-lens-enable nil)
;; (setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-enable-indentation nil)
(setq lsp-enable-on-type-formatting nil)

(provide 'init-php)
;;; init-php.el ends here
