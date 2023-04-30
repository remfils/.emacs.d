(require-package 'yasnippet)


(defun remfils/yas-php-mode-hook ()
  (yas-minor-mode))
(add-hook 'php-mode-hook 'remfils/yas-php-mode-hook)


(provide 'init-yas)
