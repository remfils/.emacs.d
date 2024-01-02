(defun remfils/ledger-mode-hook()
  (goto-address-prog-mode)
  (company-mode))

(eval-after-load
 'ledger-mode
 (progn
   (add-to-list 'auto-mode-alist '("\\.sm\\'" . ledger-mode))
   (add-hook
    'ledger-mode-hook 'remfils/ledger-mode-hook)
   (remfils/prog-end)))

(provide 'setup-ledger)
