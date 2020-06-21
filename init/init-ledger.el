;;; init-ledger.el --- Support for the ledger CLI accounting tool -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'ledger-mode)

(after-load 'ledger-mode
            (define-key ledger-mode-map (kbd "RET") 'newline)
            (define-key ledger-mode-map (kbd "C-o") 'open-line))

(add-hook 'ledger-mode-hook 'goto-address-prog-mode)

(add-to-list 'auto-mode-alist '("\\.sm\\'" . ledger-mode))

(defun remfils/ledger__insert-ruble-sign ()
  (interactive)
  ;; (insert "₽") ; more progressive ruble display
  (insert "RUB"))
(defun remfils/ledger-mode-hook ()
  (local-set-key (kbd "C-$") #'remfils/ledger__insert-ruble-sign))
(add-hook 'ledger-mode-hook 'remfils/ledger-mode-hook)

(provide 'init-ledger)
;;; init-ledger.el ends here
