;;; init-php.el --- Support for working with PHP -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; TODO: configure run init tests command
;; TODO: some basic flycheck config
;; TODO: configure lsp server support

;; (require-package 'tree-sitter)
;; (require-package 'tree-sitter-langs)
;; (require-package 'tree-sitter-indent)

(require-package 'php-mode)
(require-package 'web-mode)
(require-package 'emmet-mode)

;(global-tree-sitter-mode)

(require-package 'flycheck)

(setq flycheck-php-phpmd-executable "/home/remfils/.config/composer/vendor/bin/phpmd")
(setq flycheck-php-phpmd-executable "/home/remfils/.config/composer/vendor/bin/phpmd")
(setq flycheck-phpmd-rulesets '("unusedcode"))
(setq flycheck-standard-error-navigation nil)

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
  ;;(tree-sitter-hl-mode)
  (flycheck-mode)
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



(provide 'init-php)
;;; init-php.el ends here
