;; Allow emacs to hog all memory for startup and reset it to lsp
;; default
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024))
            (print (emacs-init-time))))

(defun remfils/prog-end())
(add-to-list 'load-path (expand-file-name "setup" user-emacs-directory))
(setq custom-file "~/.emacs.d/custom.el")

(require 'setup-packages)
(require 'setup-navigation)

(require 'setup-calc)
(require 'setup-org)
(require 'setup-remfils)
(require 'setup-general-development)
(require 'setup-ledger)


(require 'setup-gui)
(require 'setup-theme)
(require 'setup-modeline)
(require 'setup-keymap)

(load custom-file)
(remfils/set-agenda-and-refile-after-custom-load)
