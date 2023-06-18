(add-to-list 'load-path (expand-file-name "setup" user-emacs-directory))


;; UTF-8 as default encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)

(defun remfils/prog-end())

(setq gc-cons-threshold most-positive-fixnum)

;; Lower threshold back to 8 MiB (default is 800kB)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 25))
            (insert (emacs-init-time))))

(require 'setup-packages)
(require 'setup-navigation)

(require 'setup-org)

(require 'setup-general-development)
(require 'setup-lisp)
(require 'setup-js)
(require 'setup-php)
(require 'setup-python)

(require 'setup-ledger)

(require 'setup-theme)
(require 'setup-gui)
(require 'setup-modeline)
(require 'setup-keymap)

;; (setq lsp-intelephense-files-exclude '("**/.git/**" "**/.svn/**" "**/.hg/**" "**/CVS/**" "**/.DS_Store/**" "**/node_modules/**" "**/bower_components/**" "**/vendor/**/{Test,test,Tests,tests}/**"))

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
