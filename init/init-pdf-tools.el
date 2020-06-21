;;; init-windows.el --- Working with windows within frames -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(require-package 'pdf-tools)

(pdf-tools-install)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hotkeys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
(define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
(define-key pdf-view-mode-map (kbd "u") 'pdf-annot-add-underline-markup-annotation)
(define-key pdf-view-mode-map (kbd "s") 'pdf-annot-add-squiggly-markup-annotation)
(define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete)


(provide 'init-pdf-tools)
;;; init-windows.el ends here
