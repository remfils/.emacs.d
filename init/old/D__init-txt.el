
;;; init-txt.el --- tbd
;;; Commentary:
;;; Code:


(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (setq ispell-local-dictionary "en") (flyspell-mode 1))))

(setq ispell-program-name "aspell")

(provide 'init-txt)
;;; init-txt.el ends here
