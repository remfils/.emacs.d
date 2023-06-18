(setq-default
 blink-cursor-interval 0.4
 column-number-mode t
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil
 inhibit-startup-screen t
 use-file-dialog nil
 use-dialog-box nil
 view-read-only t
 visible-bell 1
 show-trailing-whitespace t
 )

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; opacity
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sanityinc/adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         ;; The 'alpha frame param became a pair at some point in
         ;; emacs 24.x, e.g. (100 100)
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

(sanityinc/adjust-opacity nil -2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; frames
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Borrowed from http://postmomentum.ch/blog/201304/blog-on-emacs
;; (defun sanityinc/split-window()
;;   "Split the window to see the most recent buffer in the other window.
;; Call a second time to restore the original window configuration."
;;   (interactive)
;;   (if (eq last-command 'sanityinc/split-window)
;;       (progn
;;         (jump-to-register :sanityinc/split-window)
;;         (setq this-command 'sanityinc/unsplit-window))
;;     (window-configuration-to-register :sanityinc/split-window)
;;     (switch-to-buffer-other-window nil)))

;; (global-set-key (kbd "<f7>") 'sanityinc/split-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; When splitting window, show (other-buffer) in the new window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun remfils/split-window-vertically(arg)
  (interactive "P")
    (split-window-vertically)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (unless arg
        (select-window target-window))))


(defun remfils/split-window-horizontally(arg)
  (interactive "P")
    (split-window-horizontally)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (unless arg
        (select-window target-window))))

(defun pulse-line (&rest _)
      "Pulse the current line."
      (pulse-momentary-highlight-one-line (point)))

(dolist (command '(scroll-up-command scroll-down-command
                   recenter-top-bottom other-window))
  (advice-add command :after #'pulse-line))

(provide 'setup-gui)
