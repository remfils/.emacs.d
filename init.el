(setq gc-cons-threshold most-positive-fixnum)

;; Lower threshold back to 8 MiB (default is 800kB)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 25))
            (print (emacs-init-time))))

(defun remfils/prog-end())
(add-to-list 'load-path (expand-file-name "setup" user-emacs-directory))
(setq custom-file "~/.emacs.d/custom.el")

(require 'setup-packages)
(require 'setup-navigation)

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
