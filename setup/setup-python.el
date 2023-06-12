(require 'python)

(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

(when (equal system-type 'windows-nt)
  (prefer-coding-system 'utf-8)
  (setenv "PYTHONIOENCODING" "utf-8"))

(setq python-indent-guess-indent-offset t)  
(setq python-indent-guess-indent-offset-verbose nil)

(defun remfils/python-mode-hook()
  (hack-local-variables)
  (when remfils-start-lsp?
    (lsp-mode))
  )

(add-hook 'python-mode-hook 'remfils/python-mode-hook)

(provide 'setup-python)
