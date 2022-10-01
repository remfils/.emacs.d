;; to read: http://tuhdo.github.io/

;; source: https://github.com/purcell/emacs.d

;; TODO (later):
;;  - spell mode
;;  - web-mode (mmm mode?), emmet, css
;;  - helm find files is slow at first
;;  - openwith


;; WARN: this is security risck
;; (setq package-check-signature nil) 

(add-to-list 'load-path (expand-file-name "conf" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "init" user-emacs-directory))

;; (require 'init-benchmarking)

(require 'conf-init)

(require 'conf-gui)
(require 'conf-edit)
(require 'conf-org)


(require 'init-avy)
(require 'init-cpp)
(require 'init-helm)
(require 'init-js)
(require 'init-ledger)
(require 'init-php)
(require 'init-python)
;; (require 'init-csharp)
;; (require 'init-openwith)
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
