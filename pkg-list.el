;;; pkg-list.el --- list and install packages
;;
;; Author: Pereskokov Vladislav (pereskokow@gmail.com)
;;; Commentary:
;; File, that contains packages to install
;;; Code:
;; Added by Package.el.  This must come before configurations of

(setq remfils/package-list
      '(helm))

(setq package-archives '(("elpa" . "http://tromey.com/elpa/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package remfils/package-list)
  (unless (package-installed-p package)
    (package-install package)))

(provide 'pkg-list)
;;; pkg-list.el ends here
