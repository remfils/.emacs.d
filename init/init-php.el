;;; init-php.el --- Support for working with PHP -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'php-mode)

(require-package 'company-php)
(after-load 'company
            (push 'company-ac-php-backend company-backends))

(provide 'init-php)
;;; init-php.el ends here
