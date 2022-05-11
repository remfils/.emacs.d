;;; init-php.el --- Support for working with PHP -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'php-mode)
(require-package 'company-php)
(require-package 'ac-php)
(require-package 'web-mode)
(require-package 'company)

(require-package 'emmet-mode)

;; TODO: 
(require-package 'flycheck)

;; (require-package 'company-php)

;; TODO: clean up all this
;; TODO: configure run init tests command
;; TODO: configure TAGS file generation
;; TODO: emmet mode maybe?
;; TODO: some basic flycheck config

(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)


(setq web-mode-enable-auto-indentation nil)


(add-hook
 'web-mode-hook
 '(lambda ()
    (emmet-mode)
    (define-key emmet-mode-keymap (kbd "C-j") nil)
    (define-key web-mode-map (kbd "C-c C-j")
      'emmet-expand-line)))

(add-hook
 'html-mode-hook
 '(lambda ()
    (emmet-mode)
    (define-key emmet-mode-keymap (kbd "C-j") nil)
    (define-key html-mode-map (kbd "C-c C-j")
      'emmet-expand-line)))

(add-hook
 'php-mode-hook
 '(lambda ()
    ;; (auto-complete-mode t)
    ;; (require 'ac-php)
    ;; (setq ac-sources '(ac-source-php))
    ;; (ac-php-core-eldoc-setup)

    (company-mode t)
    (ac-php-core-eldoc-setup)
    (make-local-variable 'company-backends)
    (setq company-backends '(company-ac-php-backend company-files))
    ;;; (setq company-backends (delete 'company-dabbrev company-backends))
    ;; (add-to-list 'company-backends 'company-files)

    ;; Enable company-mode
    ;;(company-mode t)

    ;; (set (make-local-variable 'company-backends)
    ;;      '((company-ac-php-backend)
    ;;        company-capf company-files))

    
    ;;(setq ac-sources '(ac-source-php))
    
    ;;(ac-php-core-eldoc-setup)

    ;; Jump to definition (optional)
    ;; (define-key php-mode-map (kbd "M-]")
    ;;   'ac-php-find-symbol-at-point)

    ;; ;; Return back (optional)
    ;; (define-key php-mode-map (kbd "M-[")
    ;;   'ac-php-location-stack-back)
    ))




(defun remfils-php/toggle-lsp-mode()
  (interactive)
  
  (if (bound-and-true-p lsp-mode)
      (progn
        (lsp-disconnect)
        (lsp-mode -1))
    (progn
      (setq-local
       lsp-clients-php-server-command (quote ("php" "/home/remfils/Projects/tresio/vendor/felixfbecker/language-server/bin/php-language-server.php")))
      (lsp)
      (define-key lsp-mode-map (kbd "C-x C-l") lsp-command-map))))


;; optimisations
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
;; (setq gc-cons-threshold 100000000)
;; (setq read-process-output-max (* 1024 1024))


;; not used company backends
; company-bbdb ; emails
;;company-semantic ; some semantic autocompletion,more here: http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html
;;company-cmake
;(company-dabbrev-code company-gtags company-etags company-keywords)
;;company-oddmuse
;company-dabbrev
; company-keywords



(defun remfils/php-mode-hook ()
  (local-set-key [f5] #'web-mode)
  (local-set-key [f6] 'remfils-php/toggle-lsp-mode)
  (setq php-mode-template-compatibility nil)
  ;; company configuration
  (setq
   company-backends
   '(;company-tabnine
     company-capf
     company-clang
     company-files
     company-elisp))
  (setq company-tooltip-limit 20)
  (setq company-show-numbers t)
  (setq company-idle-delay 0)
  (setq company-echo-delay 0)
  (company-mode))
(add-hook 'php-mode-hook 'remfils/php-mode-hook)



(defun remfils/web-mode-hook ()
  (local-set-key [f5] #'php-mode)
  (local-set-key '[backtab] 'indent-relative))
(add-hook 'web-mode-hook 'remfils/web-mode-hook)


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
