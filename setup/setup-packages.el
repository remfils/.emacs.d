(require 'package)

(add-to-list 'package-archives '("MELPA" . "https://melpa.org/packages/") t)
(package-initialize)

(setq
 package-selected-packages
 '(
   ;;;; file and buffer navigation
   vertico
   marginalia
   orderless
   consult
   ace-window

   ;;;; additional menu
   embark
   embark-consult

   ;;;; diagnostics
   discover-my-major

   ;;;; general dev
   company
   tree-sitter
   tree-sitter-langs
   tree-sitter-indent
   lsp-mode ;; TODO: configure flycheck for each language
   flycheck ;; TODO: configure flycheck for each language
   rainbow-delimiters
   expand-region
   magit
   yasnippet
   hl-todo
   unfill
   multiple-cursors
   
   ;;;; php dev
   php-mode
   emmet-mode
   web-mode
   
   ;;;; js dev
   js2-mode
   json-mode
   vue-mode

   ;; theme
   zenburn-theme

   ;;;; misc
   ;; ledger-mode

   ;; 
   ;; helm ; DEBUG
   ;; avy ; DEBUG
   ;; org-cliplink
   ;; switch-window
   ;; gnu-elpa-keyring-update
   ))

(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)


(provide 'setup-packages)
