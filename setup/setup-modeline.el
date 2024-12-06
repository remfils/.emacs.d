(defface remfils/file-modified-face
  '((t :foreground "white"
       :background "#2e8b57"
       :weight bold))
  "Mode line modified face."
  :group 'basic-faces)

(defface remfils/vc/feature-branch
  '((t :foreground "#00bfff"))
  "Mode line modified face."
  :group 'basic-faces)

(defface remfils/vc/task-branch
  '((t :foreground "#000000" :background "#f0f0f0"))
  "Mode line modified face."
  :group 'basic-faces)

(defface remfils/vc/basic-branch
  '((t :foreground "white"))
  "Mode line modified face."
  :group 'basic-faces)

(setq
 remfils/current-input-method
 '("["
   (current-input-method-title
    current-input-method-title
    "EN")
   "]"))

(defun remfils/format-branch-name(branch-name)
  (let ((branch-face
         (cond
          ((string-prefix-p "t-" branch-name) 'remfils/vc/task-branch)
          ((string-prefix-p "feature" branch-name) 'remfils/vc/feature-branch)
          (t '("black", "white") 'remfils/vc/basic-branch)
          )))
        (propertize
         branch-name
         'face branch-face)))

(defun remfils/version-control-mode-line()
  (let* ((vc-back (vc-backend buffer-file-name)))
    (if vc-back
        
        (let (
              (vc-back-name (symbol-name vc-back))
              (vc-branch-name (vc-git--symbolic-ref buffer-file-name)) ; NOTE: git specific
              )
          (list "[" vc-back-name "/" (remfils/format-branch-name vc-branch-name) "]"))
      "[no vc]")))

(setq-default
 mode-line-format
 (list
  "%e"
  mode-line-front-space
  mode-line-mule-info
  mode-line-client
  mode-line-remote
  mode-line-frame-identification
  " | ["
  '(:eval (if buffer-read-only
              (propertize "R" 'face 'font-lock-string-face)
            (if (buffer-modified-p)
                (propertize "W" 'face 'remfils/file-modified-face)
              "W")
              ))
  "] "
  '(:eval (propertize "%12b" 'face 'mode-line-buffer-id 'help-echo default-directory))
  " "
  '(:eval (remfils/version-control-mode-line))
  " "
  mode-line-modes
  mode-line-misc-info
  "%p (%l:%c)"
  mode-line-end-spaces))


(provide 'setup-modeline)
