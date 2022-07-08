;;; init-ledger.el --- Support for the ledger CLI accounting tool -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'ledger-mode)

(require-package 'company)

;; (require-package 'request)

(after-load 'ledger-mode
            (define-key ledger-mode-map (kbd "RET") 'newline)
            (define-key ledger-mode-map (kbd "C-o") 'open-line))

(add-hook 'ledger-mode-hook (lambda () (goto-address-prog-mode) (company-mode)))

(add-to-list 'auto-mode-alist '("\\.sm\\'" . ledger-mode))

(defun remfils/ledger__insert-ruble-sign ()
  (interactive)
  ;; (insert "₽") ; more progressive ruble display
  (insert "RUB"))
(defun remfils/ledger-mode-hook ()
  (local-set-key (kbd "C-$") #'remfils/ledger__insert-ruble-sign))
(add-hook 'ledger-mode-hook 'remfils/ledger-mode-hook)


(defun remfils/insert-tinkoff-file (f)
  (interactive "fCsv file: ")
  (let ((csv-parser-location (expand-file-name "~/.emacs.d/scripts/tinkoff-csv-parser.py")))
    (insert (shell-command-to-string (format "python \"%s\" \"%s\"" csv-parser-location f)))))
;; cb rf

(defun remfils/load-currencies-to-buffer()
  (interactive)

  (let ((cbr-url "https://www.cbr-xml-daily.ru/daily_json.js"))

    (url-retrieve
     cbr-url
     (lambda (events)
       (goto-char url-http-end-of-headers)
       (let ((json-object-type 'plist)
             (json-key-type 'symbol)
             (json-array-type 'vector))
         (let ((result (json-read)))
           ;; (print (plist-get 'Valute result))

           (print (cdr result))
           ;; (print json-read)
           ;; Do something with RESULT here
           )))

     ;; (request cbr-url
     ;;          :parser 'json-read
     ;;          :success (cl-function
     ;;                    (lambda (&key data &allow-other-keys)
     ;;                      (message "I sent: %S" (assoc-default 'args data)))))
     )

  
   ))


(provide 'init-ledger)
;;; init-ledger.el ends here
