(defvar remfils/current-project-path "~/"
  "Location of the currently editing project")
(put 'remfils/current-project-path 'safe-local-variable #'stringp)


(use-package
 vertico
 :ensure t
 :custom
 (vertico-count 15)
 (read-file-name-completion-ignore-case t)
 (read-buffer-completion-ignore-case t)
 (completion-ignore-case t)
 :init
 (vertico-mode)
 :bind
 (:map vertico-map
       ("C-j" . vertico-insert)))

(use-package
 vertico-directory
 :after vertico
 :ensure nil
 :bind
 (:map vertico-map
       ("C-l" . vertico-directory-delete-word)))


(use-package
 marginalia
 :after vertico
 :ensure t
 :custom
 (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
 :init
 (marginalia-mode)
 (setq marginalia-align 'right)
 (modify-face 'marginalia-date "#dddddd" nil nil nil nil)
 (modify-face 'marginalia-file-priv-link "#dddddd" nil nil nil nil)
 (modify-face 'marginalia-file-priv-read "#dddddd" nil nil nil nil)
 (modify-face 'marginalia-file-priv-write "#dddddd" nil nil nil nil)
 (modify-face 'marginalia-file-priv-exec "#dddddd" nil nil nil nil)
 (modify-face 'marginalia-file-priv-exec "#dddddd" nil nil nil nil)
 (modify-face 'marginalia-file-priv-dir "#BFEBBF" nil nil nil nil)
 (modify-face 'marginalia-file-name "#BFEBBF" nil nil nil nil)
 )

(use-package
 orderless
 :ensure t
 :after vertico
 :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
 (setq completion-styles '(orderless basic)
       completion-category-defaults nil
       completion-category-overrides '((file (styles partial-completion)))
       ))

(use-package
 consult
 :ensure t
 :init
 
 (defun consult-project-function(t)
   remfils/current-project-path)
 :bind
 (
  ("M-s l" . consult-line)
  ("M-y" . consult-yank-pop)
  )
 )

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
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)

;; (setq completion-styles '(initials partial-completion flex)) ; > Emacs 27.1
;; (setq completion-cycle-threshold 10)

;; TODO:
;; (cua-selection-mode)

(provide 'setup-navigation)
