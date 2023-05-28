(use-package
 json-mode
 :ensure t)

(define-key js-mode-map (kbd "M-.") nil)

(use-package
 js2-mode
 :ensure t
 :mode (("\\.js\\'" . js2-mode))
 :custom
 (js2-bounce-indent-p nil)
 (js2-highlight-level 3)
 (js-indent-level 2)
 (js2-basic-offset 2)
 (global-auto-highlight-symbol-mode t)
 :config
 (defun remfils/js2-mode-hook()
   (js2-imenu-extras-mode)
   (js2-refactor-mode))
 
 (add-hook 'js2-mode-hook 'remfils/js2-mode-hook)

 (put 'js2-basic-offset 'safe-local-variable #'integerp))

(use-package
 xref-js2
 :ensure t
 :after js2-mode)

;; NOTE: no point of use without embark
;; (use-package
;;  js2-refactor
;;  :ensure t
;;  :after js2-mode
;;  :init
;;  (js2r-add-keybindings-with-prefix "C-c C-r")
;;  (define-key js2-mode-map (kbd "C-k") #'js2r-kill))

;; rest key

(provide 'setup-js)
