(require 'js2-mode)
(require 'vue-mode)
(require 'json-mode)

(defun remfils/js2-mode-hook()
  ;; (js2-imenu-extras-mode)
  ;; (js2-refactor-mode)
  ;; NOTE: tree-sitter is not ok
  ;; (tree-sitter-mode)
  ;; (tree-sitter-hl-mode)
  (company-mode)
  )

(eval-after-load
    'js2-mode
  (progn
    (setq-default
     js2-bounce-indent-p nil
     js2-highlight-level 3
     js-indent-level 2
     js2-basic-offset 2
     )
    (add-hook 'js2-mode-hook 'remfils/js2-mode-hook)
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (remfils/prog-end)
    ))

(put 'js2-basic-offset 'safe-local-variable #'integerp)


;; (define-key js-mode-map (kbd "M-.") nil)

;; (use-package
;;  js2-mode
;;  :ensure t
;;  :mode (("\\.js\\'" . js2-mode))
;;  :custom
 
;;  (global-auto-highlight-symbol-mode t)
;;  :config
;;  (defun remfils/js2-mode-hook()
;;    (js2-imenu-extras-mode)
;;    (js2-refactor-mode))
 
 


;; (use-package
;;  xref-js2
;;  :ensure t
;;  :after js2-mode)

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
