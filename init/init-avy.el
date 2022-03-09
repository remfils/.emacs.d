;;; init-windows.el --- Working with windows within frames -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:


(require-package 'avy)

(global-set-key (kbd "C-c j") 'avy-goto-word-1)

(setq avy-background t)

(provide 'init-avy)
;;; init-windows.el ends here
