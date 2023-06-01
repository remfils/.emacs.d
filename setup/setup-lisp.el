(defun remfils/lisp-mode()
  (company-mode)
  ;; NOTE: no tree-sitter support for emacs-lisp...
  ;; (tree-sitter-mode)
  ;; (tree-sitter-hl-mode)
  )
(add-hook 'lisp-mode-hook 'remfils/lisp-mode)
(add-hook 'emacs-lisp-mode-hook 'remfils/lisp-mode)

(provide 'setup-lisp)
