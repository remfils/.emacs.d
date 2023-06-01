;; TODO: configure run init tests command
;; TODO: some basic flycheck config
;; TODO: configure lsp server support

(require 'php-mode)
(defun remfils/php-mode-hook()
  (local-set-key [f5] #'web-mode)
  ;; (local-set-key [f6] 'remfils-php/toggle-lsp-mode)
  ;; (lsp)
  (setq php-mode-template-compatibility nil)
  ;; (tree-sitter-mode)
  ;; (tree-sitter-hl-mode)
  (company-mode)
  )

(eval-after-load
    'php-mode
  (progn
    (add-hook 'php-mode-hook 'remfils/php-mode-hook)
    (remfils/prog-end)))

;; (use-package
;;  php-mode
;;  :ensure t
;;  :config
;;  (define-key php-mode-map (kbd "C-.") nil)
;;  :init
 
;;  )

(require 'web-mode)
(require 'emmet-mode)

(defun remfils/web-mode-hook()
  ;; NOTE: currently do nothing
  )

(eval-after-load
    'web-mode
  (progn
    (add-hook 'web-mode-hook 'remfils/web-mode-hook)
    (setq web-mode-enable-auto-indentation nil)
    (remfils/prog-end)))

(eval-after-load
    'emmet-mode
  (progn
    (add-hook 'web-mode-hook 'emmet-mode)
    (add-hook 'sgml-mode-hook 'emmet-mode)
    (add-hook 'css-mode-hook  'emmet-mode)
    (remfils/prog-end)))

;; (use-package
;;  web-mode
;;  :ensure t
;;  :bind
;;  (("C-c C-j" . nil))
;;  :init
;;  (add-hook 'web-mode-hook 'emmet-mode)
;;  (add-hook 'sgml-mode-hook 'emmet-mode)
;;  (add-hook 'css-mode-hook  'emmet-mode)
;; )

;; (use-package
;;  emmet-mode
;;  :ensure t
;;  :custom
;;  (web-mode-enable-auto-indentation nil)
;;  :init
;;  (add-hook 'emmet-mode-hook 'remfils/emmet-mode-hook)
;;  :bind
;;  (("C-j" . nil)
;;   ("C-c C-j" . 'emmet-expand-line))
;;  )

;; (use-package
;;  flycheck
;;  :ensure t
;;  :custom
;;  (flycheck-php-phpmd-executable "/home/remfils/.config/composer/vendor/bin/phpmd")
;;  (flycheck-php-phpmd-executable "/home/remfils/.config/composer/vendor/bin/phpmd")
;;  (flycheck-phpmd-rulesets '("unusedcode"))
;;  (flycheck-standard-error-navigation nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; f5 toggle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun remfils/php-mode-hook ()
;;   (local-set-key [f5] #'web-mode)
;;   (local-set-key [f6] 'remfils-php/toggle-lsp-mode)
;;   (lsp)
;;   (setq php-mode-template-compatibility nil))
;; (add-hook 'php-mode-hook 'remfils/php-mode-hook)

;; (defun remfils/web-mode-hook ()
;;   (local-set-key [f5] #'php-mode)
;;   (local-set-key '[backtab] 'indent-relative))
;; (add-hook 'web-mode-hook 'remfils/web-mode-hook)

(provide 'setup-php)
