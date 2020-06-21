;;; init-js.el --- Support for Javascript and derivatives -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'json-mode)
(maybe-require-package 'js2-mode)

(add-to-list 'auto-mode-alist '("\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'" . js2-mode))

(setq-default js2-bounce-indent-p nil)
(after-load 'js2-mode
            ;; ;; Disable js2 mode's syntax error highlighting by default...
            ;; (setq-default js2-mode-show-parse-errors nil
            ;;               js2-mode-show-strict-warnings nil)
            
            (add-hook 'js2-mode-hook (lambda () (setq mode-name "JS2")))

            (js2-imenu-extras-setup))

(setq-default js-indent-level 2)
;; In Emacs >= 25, the following is an alias for js-indent-level anyway
(setq-default js2-basic-offset 2)


(provide 'init-js)
;;; init-javascript.el ends here
