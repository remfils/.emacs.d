;;; init-ledger.el --- Support for the ledger CLI accounting tool -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'helm)
;; (require-package 'helm-descbinds)
;; (require-package 'helm-ag)

(require 'helm-config)
;; (require 'helm-eshell)

(setq helm-split-window-in-side-p t
      helm-buffers-fuzzy-matching t
      helm-move-to-line-cycle-in-source t
      helm-ff-search-library-in-sexp t
      helm-ff-file-name-history-use-recentf nil)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

;; (define-key helm-command-map (kbd "o") 'helm-occur)
;; (define-key helm-command-map (kbd "g") 'helm-do-grep)
;; (define-key helm-command-map (kbd "C-c w") 'helm-wikipedia-suggest)
;; (define-key helm-command-map (kbd "SPC") 'helm-all-mark-rings)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-m") 'helm-M-x)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-h f") 'helm-apropos)
(global-set-key (kbd "C-h r") 'helm-info-emacs)
(global-set-key (kbd "C-h C-l") 'helm-locate-library)

(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

(define-key isearch-mode-map (kbd "C-o") 'helm-occur-from-isearch)

(add-hook
 'after-init-hook
 (lambda () (helm-mode 1)))



(provide 'init-helm)
;;; init-ledger.el ends here
