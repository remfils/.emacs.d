(load-theme 'zenburn t)

(set-face-attribute 'lazy-highlight nil :background "#2B2B2B" :underline t)

;; marginalia

;; TODO: change to set-face-attribute
(add-hook
 'marginalia-mode-hook
 (lambda ()
   (modify-face 'marginalia-date "#dddddd" nil nil nil nil)
   (modify-face 'marginalia-file-priv-link "#dddddd" nil nil nil nil)
   (modify-face 'marginalia-file-priv-read "#dddddd" nil nil nil nil)
   (modify-face 'marginalia-file-priv-write "#dddddd" nil nil nil nil)
   (modify-face 'marginalia-file-priv-exec "#dddddd" nil nil nil nil)
   (modify-face 'marginalia-file-priv-exec "#dddddd" nil nil nil nil)
   (modify-face 'marginalia-file-priv-dir "#BFEBBF" nil nil nil nil)
   (modify-face 'marginalia-file-name "#BFEBBF" nil nil nil nil)))

;; company

;; TODO: change to set-face-attribute
(add-hook
 'company-mode-hook
 (lambda ()
   (modify-face 'company-preview "#8b8682" nil nil nil nil)))

;; tree sitter
;; (add-hook
;;  'prog-mode-hook
;;  (lambda ()
;;    (modify-face 'tree-sitter-hl-face:property nil nil nil nil nil nil nil)))

(with-eval-after-load 'ace-window
  (set-face-attribute 'aw-leading-char-face nil :height 200))

(add-hook
 'mmm-mode-hook
 (lambda ()
   (set-face-attribute 'mmm-default-submode-face nil :background nil)))

(provide 'setup-theme)
