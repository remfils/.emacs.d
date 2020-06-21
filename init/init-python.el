;;; init-python.el --- Python editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; See the following note about how I set up python + virtualenv to
;; work seamlessly with Emacs:
;; https://gist.github.com/purcell/81f76c50a42eee710dcfc9a14bfc7240

(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))


;; python cyrylic in term fix
(when (equal system-type 'windows-nt)
  (prefer-coding-system 'utf-8)
  (setenv "PYTHONIOENCODING" "utf-8"))

;; (setq python-shell-interpreter "python3")

(setq python-indent-guess-indent-offset t)  
(setq python-indent-guess-indent-offset-verbose nil)

(provide 'init-python)
;;; init-python.el ends here
