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

(defun remfils/insert-tinkoff-file (f)
  (interactive "fCsv file: ")
  
  (let ((current-buffer-position (mark-marker))
        (csv-parser-location (expand-file-name "~/.emacs.d/scripts/tinkoff-csv-parser.py")))
    (insert (shell-command-to-string (format "python \"%s\" \"%s\"" csv-parser-location f)))
    (goto-char current-buffer-position)))

(provide 'setup-ledger)
