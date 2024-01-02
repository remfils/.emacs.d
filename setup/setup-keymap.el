;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)
(global-set-key (kbd "C-j") 'newline)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "C-x C-n") 'switch-to-next-buffer)
(global-set-key (kbd "C-x C-p") 'switch-to-prev-buffer)

(with-eval-after-load 'unfill
  (global-set-key (kbd "M-Q") 'unfill-paragraph))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-eval-after-load 'vertico-directory
  (define-key vertico-map (kbd "C-j") 'vertico-insert)
  (define-key vertico-map (kbd "C-l") 'vertico-directory-delete-word))

(defun remfils/consult-ripgrep(dir)
  (interactive (list (read-directory-name "Ripgrep dir: ")))
  (consult-ripgrep dir))

(with-eval-after-load 'consult
  (global-set-key (kbd "M-s l") 'consult-line)
  (global-set-key (kbd "M-s i") 'consult-imenu)
  (global-set-key (kbd "M-y") 'consult-yank-pop)
  (global-set-key (kbd "C-x b") 'consult-buffer)
  (global-set-key (kbd "M-s g") 'remfils/consult-ripgrep))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'company
  (define-key company-active-map (kbd "TAB") 'company-complete-selection)
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
  (define-key company-active-map [return] 'company-complete-selection)
  (define-key company-active-map (kbd "RET") 'company-complete-selection))

(with-eval-after-load 'expand-region
  (global-set-key (kbd "C-=") 'er/expand-region))

(with-eval-after-load 'ace-window
  (global-set-key (kbd "M-o") 'ace-window))

(with-eval-after-load 'embark
  (global-set-key (kbd "M-`") 'embark-act))

(with-eval-after-load 'multiple-cursors
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this))

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


(with-eval-after-load 'magit
  (global-set-key (kbd "C-x g") 'magit-status))

(with-eval-after-load 'php-mode
  ;; (define-key php-mode-map (kbd "C-.") nil)
  )

(with-eval-after-load 'web-mode
  (define-key web-mode-map (kbd "<f5>") 'php-mode)
  (define-key web-mode-map (kbd "M-o") 'ace-window))


(with-eval-after-load 'emmet-mode
  (define-key emmet-mode-keymap (kbd "C-j") nil)
  (define-key emmet-mode-keymap (kbd "C-c C-j") 'emmet-expand-line))

(with-eval-after-load 'window
  (global-set-key (kbd "C-x 2") 'remfils/split-window-vertically)
  (global-set-key (kbd "C-x 3") 'remfils/split-window-horizontally))

(with-eval-after-load 'cc-mode
  (define-key c-mode-map (kbd "M-O") 'remfils/c++/open-header-or-source-file)
  (define-key c++-mode-map (kbd "M-O") 'remfils/c++/open-header-or-source-file))

(print (type-of #'split-window-below))

(with-eval-after-load 'csharp-mode
  (define-key csharp-mode-map (kbd "<f5>") 'recompile))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ledger
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'ledger-mode
  ;; (define-key ledger-mode-map (kbd "RET") 'newline)
  ;; (define-key ledger-mode-map (kbd "C-o") 'open-line)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-key global-map (kbd "C-c o a") 'org-agenda)
(define-key global-map (kbd "C-c o l") 'org-store-link)
(define-key global-map (kbd "C-c o c") 'org-capture)
(define-key global-map (kbd "C-c o o") 'remfils/capture/doc.org-other-window)


(define-key global-map (kbd "C-c k c") 'epa-encrypt-region)
(define-key global-map (kbd "C-c k d") 'epa-decrypt-region)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; js
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'vue-mode
  (define-key vue-html-mode-map (kbd "M-o") 'ace-window))


(provide 'setup-keymap)
