;;; init-php.el --- Support for working with PHP -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'php-mode)
(require-package 'web-mode)
(require-package 'lsp-mode)
(require-package 'company)

;; (require-package 'company-php)

;; TODO: clean up all this
;; TODO: configure run init tests command
;; TODO: configure TAGS file generation
;; TODO: emmet mode maybe?
;; TODO: some basic flycheck config

(setq lsp-enable-indentation nil)
(setq lsp-enable-on-type-formatting nil)


(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)


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



(defun remfils-php/toggle-lsp-mode()
  (interactive)
  
  (if (bound-and-true-p lsp-mode)
      (progn
        (lsp-disconnect)
        (lsp-mode -1)
        (company-mode -1))
    (progn
      (setq-local
       lsp-clients-php-server-command (quote ("php" "/home/remfils/Projects/tresio/vendor/felixfbecker/language-server/bin/php-language-server.php")))
      (company-mode)
      (lsp)
      (define-key lsp-mode-map (kbd "C-x C-l") lsp-command-map))))


(add-hook
 'php-mode-hook
 (lambda ()
   (local-set-key [f5] #'web-mode)
   (local-set-key [f6] 'remfils-php/toggle-lsp-mode)))

(add-hook
 'web-mode-hook
 (lambda ()
   (local-set-key [f5] #'php-mode)))


;; (after-load 'company
;;             (push 'company-ac-php-backend company-backends))




;; (define-key web-mode-map (kbd "C-n") 'web-mode-tag-match)


;; (add-hook
;;  'web-mode-hook
;;  (lambda()
;;    (define-key 'web-mode-map (kbd "<f5>") php-mode)))


;; (add-hook
;;  'php-mode-hook
;;  (lambda()
;;    (define-key 'php-mode-map (kbd "<f5>") web-mode)
;;    ;; (setq-local
;;    ;;  lsp-clients-php-server-command (quote ("php" "/home/remfils/Projects/tresio/vendor/felixfbecker/language-server/bin/php-language-server.php")))
;;    ;; (company-mode)
;;    ;; (lsp)
;;    ;; (define-key lsp-mode-map (kbd "C-x C-l") lsp-command-map)
;;    ))


;; (add-hook
;;  'hack-local-variables-hook
;;  (lambda () (when (derived-mode-p 'php-mode) (lsp) (define-key lsp-mode-map (kbd "C-x C-l") lsp-command-map))))

;; (add-hook 'hack-local-variables-hook (lambda () (when (derived-mode-p 'php-mode) (lsp))))


;; (setq lsp-lens-enable nil)
;; (setq lsp-headerline-breadcrumb-enable nil)


(provide 'init-php)
;;; init-php.el ends here
