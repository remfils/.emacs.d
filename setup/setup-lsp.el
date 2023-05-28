(use-package
 lsp-mode
 :ensure t)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      lsp-idle-delay 0.1)

(provide 'setup-lsp)
