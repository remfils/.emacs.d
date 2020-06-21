;;; conf-org.el --- Org-mode config -*- lexical-binding: t -*-

;;; Commentary:

;; these are functions that are heavy to load (>1000ms second add to load time)

;;; Code:


(when (equal system-type 'windows-nt)
  (org-babel-do-load-languages
   'org-babel-load-languages '((shell . t) (lisp . t) (python . t) (perl . t) (sql . t))))

(when (equal system-type 'gnu/linux)
  (org-babel-do-load-languages
   'org-babel-load-languages '((shell . t) (lisp . t) (python . t) (perl . t) (sql . t))))


(provide 'conf-org-heavy)
;;; init-org.el ends here
