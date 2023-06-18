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
(setq custom-file "~/.emacs.d/custom.el")

(require 'setup-packages)
(require 'setup-navigation)

(require 'setup-org)

(require 'setup-general-development)
(require 'setup-lisp)
(require 'setup-js)
(require 'setup-php)
(require 'setup-python)

(require 'setup-ledger)

(require 'setup-remfils)

(require 'setup-theme)
(require 'setup-gui)
(require 'setup-modeline)
(require 'setup-keymap)

(load custom-file)

(remfils/set-agenda-and-refile-after-custom-load)
;; (setq lsp-intelephense-files-exclude '("**/.git/**" "**/.svn/**" "**/.hg/**" "**/CVS/**" "**/.DS_Store/**" "**/node_modules/**" "**/bower_components/**" "**/vendor/**/{Test,test,Tests,tests}/**"))
