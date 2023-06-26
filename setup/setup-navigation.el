;; TODO: is this even needed?
(defvar remfils/current-project-path "~/"
  "Location of the currently editing project")
(put 'remfils/current-project-path 'safe-local-variable #'stringp)


(require 'vertico)
(require 'vertico-directory)
(setq-default
 vertico-count 15
 read-file-name-completion-ignore-case t
 read-buffer-completion-ignore-case t
 completion-ignore-case t
 enable-recursive-minibuffers t
 )
(vertico-mode)

(require 'marginalia)
(setq-default
 marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)
 marginalia-mode
 marginalia-align right
 )

(marginalia-mode)

(require 'orderless)
(with-eval-after-load 'orderless
  (setq
   completion-styles '(orderless basic)
   completion-category-defaults nil
   completion-category-overrides '((file (styles orderless partial-completion)))
   ))


(require 'consult)

(defun consult-project-function(t)
   remfils/current-project-path)

;; (use-package
;;  embark
;;  :ensure t
;;  :bind
;;  (("C-." . embark-act))
;;  :config
;;  (set-face-attribute 'embark-target nil :background "#2B2B2B" :foreground nil)
;;  )

;; (use-package
;;  embark-consult
;;  :ensure t
;;  :after embark
;;  )

;; TODO: rename


;; TODO:


(require 'embark)
(require 'embark-consult)

(eval-after-load
    'embark
  '(progn
     (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)
     (remfils/prog-end)))

(require 'ace-window)
(eval-after-load
    'ace-window
  '(progn
     (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
     (remfils/prog-end)))

(provide 'setup-navigation)
