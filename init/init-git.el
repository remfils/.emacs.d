;;; init-git.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:

;; See also init-github.el.

;;; Code:

(require-package 'magit)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)

(setenv "SSH_ASKPASS" "git-gui--askpass")


(provide 'init-git)
;;; init-git.el ends here
