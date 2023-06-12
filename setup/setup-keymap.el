;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)
(global-set-key (kbd "C-j") 'newline)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(eval-after-load
    'unfill
  '(progn
     (global-set-key (kbd "M-Q") 'unfill-paragraph)
     (remfils/prog-end)
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-after-load
    'vertico-directory
  '(progn
     (define-key vertico-map (kbd "C-j") 'vertico-insert)
     (define-key vertico-map (kbd "C-l") 'vertico-directory-delete-word)
     (remfils/prog-end)
     ))

(defun remfils/consult-ripgrep(dir)
  (interactive (list (read-directory-name "Ripgrep dir: ")))
  (consult-ripgrep dir))

(eval-after-load
    'consult
  '(progn
     (global-set-key (kbd "M-s l") 'consult-line)
     (global-set-key (kbd "M-s i") 'consult-imenu)
     (global-set-key (kbd "M-y") 'consult-yank-pop)
     (global-set-key (kbd "C-x b") 'consult-buffer)
     (global-set-key (kbd "M-s g") 'remfils/consult-ripgrep)
     (remfils/prog-end)
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load
    'company
  '(progn
     (define-key company-active-map (kbd "TAB") 'company-complete-selection)
     (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
     (define-key company-active-map [return] 'company-complete-selection)
     (define-key company-active-map (kbd "RET") 'company-complete-selection)
     (remfils/prog-end)
     ))
 
(eval-after-load
    'expand-region
  '(progn
     (global-set-key (kbd "C-=") 'er/expand-region)
     (remfils/prog-end)
     ))

(eval-after-load
    'ace-window
  '(progn
     (global-set-key (kbd "M-o") 'ace-window)
     (remfils/prog-end)))

(eval-after-load
    'embark
  '(progn
     (global-set-key (kbd "M-`") 'embark-act)
     (remfils/prog-end)))

(eval-after-load
    'multiple-cursors
  '(progn
     (global-set-key (kbd "C->") 'mc/mark-next-like-this)
     (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
     (remfils/prog-end)
     ))

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
(global-set-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)


(eval-after-load
    'magit
  (progn
    (global-set-key (kbd "C-x g") 'magit-status)
    (remfils/prog-end)
    ))

(eval-after-load
    'php-mode
  (progn
    ;; (define-key php-mode-map (kbd "C-.") nil)
    (remfils/prog-end)
    ))

(eval-after-load
    'web-mode
  (progn
    (define-key web-mode-map (kbd "<f5>") 'php-mode)
    (define-key web-mode-map (kbd "M-o") 'ace-window)
    ; (global-set-key (kbd "C-c C-j") nil)
    (remfils/prog-end)
    ))


(eval-after-load
    'emmet-mode
  (progn
    (define-key emmet-mode-keymap (kbd "C-j") nil)
    (define-key emmet-mode-keymap (kbd "C-c C-j") 'emmet-expand-line)
    (remfils/prog-end)
    ))

(eval-after-load
    'window
  (progn
    (global-set-key (kbd "C-x 2") 'remfils/split-window-vertically)
    (global-set-key (kbd "C-x 3") 'remfils/split-window-horizontally)
    (remfils/prog-end)))

(print (type-of #'split-window-below))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ledger
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load
    'ledger-mode
  (progn
    ;; (define-key ledger-mode-map (kbd "RET") 'newline)
    ;; (define-key ledger-mode-map (kbd "C-o") 'open-line)
    (remfils/prog-end)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)

(define-key global-map (kbd "C-c k c") 'epa-encrypt-region)
(define-key global-map (kbd "C-c k d") 'epa-decrypt-region)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; js
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load
    'vue-mode
  (progn
    (define-key vue-html-mode-map (kbd "M-o") 'ace-window)
    (remfils/prog-end)))


(provide 'setup-keymap)
