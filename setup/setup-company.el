(use-package
 company
 :ensure t
 :init
 (setq company-echo-delay 0)
 (setq company-tooltip-limit 20)
 (setq company-idle-delay 0.0)
 (setq company-minimum-prefix-length 1)
 :config
 
 (modify-face 'company-preview "#8b8682" nil nil nil nil)
 
 ;; (define-key company-active-map (kbd "TAB") 'company-complete-selection)
 ;; (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
 ;; (define-key company-active-map [return] 'company-complete-selection)
 ;; (define-key company-active-map (kbd "RET") 'company-complete-selection)

 ;; (defun company-ac-setup ()
 ;;   "Sets up `company-mode' to behave similarly to `auto-complete-mode'."
 ;;   (setq company-require-match nil)
 ;;   (setq company-auto-complete #'my-company-visible-and-explicit-action-p)
 ;;   (setq company-frontends '(company-echo-metadata-frontend
 ;;                             company-pseudo-tooltip-unless-just-one-frontend-with-delay
 ;;                             company-preview-frontend))
 ;;   (define-key company-active-map [tab]
 ;;     'company-select-next-if-tooltip-visible-or-complete-selection)
 ;;   (define-key company-active-map (kbd "TAB")
 ;;     'company-select-next-if-tooltip-visible-or-complete-selection))
 
 ;; (company-ac-setup)

 (add-hook
  'js2-mode-hook
  (lambda ()
    (setq company-backends
    '(company-bbdb company-semantic company-cmake company-capf company-clang company-files (company-dabbrev-code company-gtags company-etags company-keywords) company-oddmuse))
    (company-mode)))
 )

(provide 'setup-company)
