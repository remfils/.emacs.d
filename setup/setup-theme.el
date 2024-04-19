(load-theme 'zenburn t)

(set-face-attribute 'lazy-highlight nil :background "#2B2B2B" :underline t)
(set-face-attribute 'header-line nil :inherit 'default)


(set-face-attribute 'org-agenda-date-today nil :background "#da70d6" :bold nil)

(defface remfils/agenda-face/birthday
  '((t :foreground "white"
       :background "#1e90ff"
       :weight bold))
  "Mode line modified face."
  :group 'agenda-faces)

(defface remfils/agenda-face/event-log
  '((t :foreground "#87cefa"
       :underline t
       ))
  "Mode line modified face."
  :group 'agenda-faces)

(defface remfils/agenda-face/todo
  '((t :foreground "white"
       :background "#ff4500"
       ))
  "Mode line modified face."
  :group 'agenda-faces)

(defface remfils/agenda-face/project
  '((t :foreground "black"
       :background "#00ff7f"
       ))
  "Mode line modified face."
  :group 'agenda-faces)

(defface remfils/agenda-face/warn
  '((t :foreground "white"
       :background "#D6001C"
       :weight bold
       ))
  "Mode line modified face."
  :group 'agenda-faces)

(defface remfils/agenda-face/track
  '((t :foreground "white"
       :background "#d2691e"
       :weight bold))
  "Mode line modified face."
  :group 'agenda-faces)

(defface remfils/agenda-face/workout
  '((t :foreground "white"
       :background "#836fff"
       :weight bold))
  "Mode line modified face."
  :group 'agenda-faces)

;; (set-face-attribute 'remfils/agenda-face/important nil :background "#2B2B2B" :underline t)
;; (set-face-attribute 'remfils/agenda-face/now nil :background "#2B2B2B" :underline t)
;; (set-face-attribute 'remfils/agenda-face/goal nil :background "#2B2B2B" :underline t)

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
