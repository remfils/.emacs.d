(add-to-list 'load-path (expand-file-name "setup" user-emacs-directory))

(require 'setup-use-package)

;; UTF-8 as default encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)

(require 'setup-gui)
(require 'setup-navigation)

(require 'setup-company)
(require 'setup-lisp)
(require 'setup-js)

(global-set-key (kbd "C-j") 'newline)
(global-set-key (kbd "M-z") 'zap-up-to-char)


(require 'setup-modeline)


;; 
;; (require 'setup-projects)

;; (require 'setup-lsp)
;; (require 'setup-php)
;; (require 'setup-js)

;; (require 'setup-edit)

;; (setq lsp-intelephense-files-exclude '("**/.git/**" "**/.svn/**" "**/.hg/**" "**/CVS/**" "**/.DS_Store/**" "**/node_modules/**" "**/bower_components/**" "**/vendor/**/{Test,test,Tests,tests}/**"))



;; (require 'conf-unset-keybingings)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(embark-consult embark consult orderless vertico use-package zenburn-theme yasnippet web-mode vue-mode unfill switch-window s rainbow-delimiters php-mode org-cliplink multiple-cursors magit ledger-mode json-mode js2-mode hl-todo helm flycheck flatui-theme expand-region emmet-mode company avy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
