;;; init-php.el --- Support for working with PHP -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; TODO: configure run init tests command
;; TODO: some basic flycheck config
;; TODO: configure lsp server support


(require-package 'php-mode)
(require-package 'web-mode)
(require-package 'emmet-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; web mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq web-mode-enable-auto-indentation nil)

(defun remfils/web-mode-hook ()
  (define-key web-mode-map (kbd "C-c C-j") nil))
(add-hook 'web-mode-hook 'remfils/web-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emmet mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remfils/emmet-mode-hook ()
  (define-key emmet-mode-keymap (kbd "C-j") nil)
  (define-key emmet-mode-keymap (kbd "C-c C-j") 'emmet-expand-line))
(add-hook 'emmet-mode-hook 'remfils/emmet-mode-hook)

(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; f5 toggle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remfils/php-mode-hook ()
  (local-set-key [f5] #'web-mode)
  (local-set-key [f6] 'remfils-php/toggle-lsp-mode)
  (setq php-mode-template-compatibility nil))
(add-hook 'php-mode-hook 'remfils/php-mode-hook)

(defun remfils/web-mode-hook ()
  (local-set-key [f5] #'php-mode)
  (local-set-key '[backtab] 'indent-relative))
(add-hook 'web-mode-hook 'remfils/web-mode-hook)

;;;; NOTE: company junk
;; (setq company-dabbrev-downcase 0)
;; (setq company-idle-delay 0)
;; (setq-default company-tooltip-limit 20)
;; (setq-default company-show-numbers t)
;; (setq-default company-idle-delay 0)
;; (setq-default company-echo-delay 0)


;;;; NOTE: lsp junk
;; optimisations
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
;; (setq gc-cons-threshold 100000000)
;; (setq read-process-output-max (* 1024 1024))
;; (add-hook
;;  'hack-local-variables-hook
;;  (lambda () (when (derived-mode-p 'php-mode) (lsp) (define-key lsp-mode-map (kbd "C-x C-l") lsp-command-map))))

;; (add-hook 'hack-local-variables-hook (lambda () (when (derived-mode-p 'php-mode) (lsp))))


;; (setq lsp-lens-enable nil)
;; (setq lsp-headerline-breadcrumb-enable nil)

;; (defun remfils-php/toggle-lsp-mode()
;;   (interactive)
  
;;   (if (bound-and-true-p lsp-mode)
;;       (progn
;;         (lsp-disconnect)
;;         (lsp-mode -1))
;;     (progn
;;       (setq-local
;;        lsp-clients-php-server-command (quote ("php" "/home/remfils/Projects/tresio/vendor/felixfbecker/language-server/bin/php-language-server.php")))
;;       (lsp)
;;       (define-key lsp-mode-map (kbd "C-x C-l") lsp-command-map))))
 



(provide 'init-php)
;;; init-php.el ends here
