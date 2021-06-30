;; to read: http://tuhdo.github.io/

;; source: https://github.com/purcell/emacs.d

;; TODO:
;;  - fix comments
;;  - merge with previous config

;; TODO (later):
;;  - spell mode
;;  - web-mode (mmm mode?), emmet, css
;;  - helm find files is slow at first
;;  - openwith

(add-to-list 'load-path (expand-file-name "conf" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "init" user-emacs-directory))

(require 'init-benchmarking)

(require 'conf-init)

(require 'init-elpa)      ;; Machinery for installing required packages

(require 'org-yt)

(require 'conf-themes)
(require 'conf-gui)
(require 'conf-edit)
(require 'conf-org)
(require 'conf-backups)

(require 'init-yasnippet)
(require 'init-cpp)
(require 'init-multiple-cursors)
(require 'init-avy)
(require 'init-helm)
;; (require 'init-ivy)
(require 'init-windows)
(require 'init-dired)
(require 'init-isearch)
(require 'init-hippie-expand)
(require 'init-company)
(require 'init-git)
(require 'init-js)
(require 'init-php)
(require 'init-nxml)
(require 'init-python)
(require 'init-ledger)
(require 'init-csharp)
(require 'init-windows)
(require 'init-pdf-tools)
(require 'init-openwith)
(require 'init-remfils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Allow access from emacsclient
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))


(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(require 'conf-capture-sync)

(provide 'init)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'downcase-region 'disabled nil)
