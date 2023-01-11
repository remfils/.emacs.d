;;; init-ledger.el --- Support for the ledger CLI accounting tool -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'helm)
;; (require-package 'helm-descbinds)
;; (require-package 'helm-ag)

(require 'helm-buffers)
(require 'helm-files)
;; (require 'helm-eshell)

(setq helm-split-window-in-side-p t
      helm-buffers-fuzzy-matching t
      helm-move-to-line-cycle-in-source t
      helm-ff-search-library-in-sexp t
      helm-ff-file-name-history-use-recentf nil)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

;; (define-key helm-command-map (kbd "o") 'helm-occur)
;; (define-key helm-command-map (kbd "g") 'helm-do-grep)
;; (define-key helm-command-map (kbd "C-c w") 'helm-wikipedia-suggest)
;; (define-key helm-command-map (kbd "SPC") 'helm-all-mark-rings)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-m") 'helm-M-x)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-h f") 'helm-apropos)
(global-set-key (kbd "C-h r") 'helm-info-emacs)
(global-set-key (kbd "C-h C-l") 'helm-locate-library)

(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

(define-key isearch-mode-map (kbd "C-o") 'helm-occur-from-isearch)

(setq helm-buffer-max-length 40)

;; TODO: clean up

;; (defun remfils/helm-switch-other-window-horizontally ()
;;   (interactive)
;;   (with-helm-alive-p
;;     (helm-exit-and-execute-action
;;      (lambda (candidate)
;;        (let ((helm-window-prefer-horizontal-split nil))
;;          (helm-find-files-other-window candidate))))))

;; (defun remfils/helm-switch-other-window-vertically ()
;;   (interactive)
;;   (with-helm-alive-p
;;     (helm-exit-and-execute-action
;;      (lambda (candidate)
;;        (let ((helm-window-prefer-horizontal-split t))
;;          (helm-find-files-other-window candidate))))))


;; (define-key helm-find-files-map (kbd "C-c 2") 'remfils/helm-switch-other-window-horizontally)
;; (define-key helm-find-files-map (kbd "C-c 3") 'remfils/helm-switch-other-window-vertically)
;; (define-key helm-buffer-map (kbd "C-c 2") 'remfils/helm-switch-other-window-horizontally)
;; (define-key helm-buffer-map (kbd "C-c 3") 'remfils/helm-switch-other-window-vertically)

;; source: https://occasionallycogent.com/emacs_custom_helm_actions/index.html
(defun helm-file-switch-to-new--horizontal-window (_candidate)
  ;; The candidate to open is passed in to the function
  ;; but we're going to use `helm-marked-candidates' to explicitly
  ;; get the selected list of candidates anyway
  ;; (so the user can use C-SPC to select multiple files & open them all in
  ;; horizontal splits)
  (dolist (cand (helm-marked-candidates))
    ;; focus a new horizontal split...
    (select-window (split-window-right))
    ;; and open the file there
    (find-file cand))
  ;; adjust the windows after opening all those splits
  (balance-windows))

(defun helm-file-switch-to-new--vertical-window (_candidate)
  ;; The candidate to open is passed in to the function
  ;; but we're going to use `helm-marked-candidates' to explicitly
  ;; get the selected list of candidates anyway
  ;; (so the user can use C-SPC to select multiple files & open them all in
  ;; horizontal splits)
  (dolist (cand (helm-marked-candidates))
    ;; focus a new horizontal split...
    (select-window (split-window-below))
    ;; and open the file there
    (find-file cand))
  ;; adjust the windows after opening all those splits
  (balance-windows))

(defun helm-buffer-switch-to-new--horizontal-window (_candidate)
  ;; The candidate to open is passed in to the function
  ;; but we're going to use `helm-marked-candidates' to explicitly
  ;; get the selected list of candidates anyway
  ;; (so the user can use C-SPC to select multiple files & open them all in
  ;; horizontal splits)
  (dolist (cand (helm-marked-candidates))
    ;; focus a new horizontal split...
    (select-window (split-window-right))
    ;; and open the file there
    (switch-to-buffer cand))
  ;; adjust the windows after opening all those splits
  (balance-windows))

(defun helm-buffer-switch-to-new--vertical-window (_candidate)
  ;; The candidate to open is passed in to the function
  ;; but we're going to use `helm-marked-candidates' to explicitly
  ;; get the selected list of candidates anyway
  ;; (so the user can use C-SPC to select multiple files & open them all in
  ;; horizontal splits)
  (dolist (cand (helm-marked-candidates))
    ;; focus a new horizontal split...
    (select-window (split-window-below))
    ;; and open the file there
    (switch-to-buffer cand))
  ;; adjust the windows after opening all those splits
  (balance-windows))


(defun helm-file-switch-new-horizontal-window ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action #'helm-file-switch-to-new--horizontal-window)))
(defun helm-file-switch-new-vertical-window ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action #'helm-file-switch-to-new--vertical-window)))
(defun helm-buffer-switch-new-horizontal-window ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action #'helm-buffer-switch-to-new--horizontal-window)))
(defun helm-buffer-switch-new-vertical-window ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action #'helm-buffer-switch-to-new--vertical-window)))

(define-key helm-buffer-map (kbd "C-x 2") 'helm-buffer-switch-new-vertical-window)
(define-key helm-buffer-map (kbd "C-x 3") 'helm-buffer-switch-new-horizontal-window)
(define-key helm-find-files-map (kbd "C-x 2") 'helm-file-switch-new-vertical-window)
(define-key helm-find-files-map (kbd "C-x 3") 'helm-file-switch-new-horizontal-window)

(add-hook
 'helm-find-files-after-init-hook
 (lambda ()
   (helm-add-action-to-source "Display file(s) in new vertical split(s) `C-v'"
                              #'helm-file-switch-to-new--vertical-window
                              helm-source-find-files)

   (helm-add-action-to-source "Display file(s) in new horizontal split(s) `C-s'"
                              #'helm-file-switch-to-new--horizontal-window
                              helm-source-find-files)))


(defun cogent/add-helm-buffer-actions (&rest _args)
      (helm-add-action-to-source "Display buffer(s) in new vertical split(s) `C-v'"
                                 #'helm-buffer-switch-to-new--vertical-window
                                 helm-source-buffers-list)
      (helm-add-action-to-source "Display buffer(s) in new horizontal split(s) `C-s'"
                                 #'helm-buffer-switch-to-new--horizontal-window
                                 helm-source-buffers-list))
(advice-add 'helm-buffers-list--init :after #'cogent/add-helm-buffer-actions)


(add-hook
 'after-init-hook
 (lambda () (helm-mode 1)))



(provide 'init-helm)
;;; init-ledger.el ends here
