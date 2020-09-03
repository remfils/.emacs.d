;;; init-ledger.el --- Support for the ledger CLI accounting tool -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'rx)

(require-package 'csharp-mode)




(defun remfils/inline-object-declaration (class-name fields)
  (interactive "sclass name: \nsclass fields: ")

  (let ((lines (split-string fields "\n"))
        (field-regex
         (rx bol
             (zero-or-more space)
             "public"
             space
             (group-n 2 (one-or-more (any "a-z" "A-Z" "0-9" "?_")))
             space
             (group-n 1 (one-or-more (any "a-z" "A-Z" "0-9" "?_")))
             space
             (zero-or-more anything)
             eol)))
    
    (insert "var obj = new ")
    (insert class-name)
    (insert " {\n")
    
    (dolist (line lines value)
      ;; (insert line)

      (if (string-match field-regex line)
          (insert (match-string 1 line) " = null, // type: " (match-string 2 line) "\n")
        (insert "// skiped: " line "\n")))
    
    (insert "};")))

(provide 'init-csharp)
;;; init-ledger.el ends here
