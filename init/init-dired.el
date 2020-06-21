;;; init-dired.el --- Dired customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default dired-dwim-target t)

(require 'dired-x)
;; TODO: figure out how to work these keys
(define-key ctl-x-map "\C-j" 'dired-jump)
(define-key ctl-x-4-map "\C-j" 'dired-jump-other-window)

(provide 'init-dired)
;;; init-dired.el ends here
