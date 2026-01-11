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
   embark-consult
   embark

   ;;;; diagnostics
   discover-my-major

   ;;;; general dev
   company
   ;; tree-sitter
   ;; tree-sitter-langs
   ;; tree-sitter-indent
   lsp-mode ;; TODO: configure flycheck for each language
   lsp-ui ;; TODO: configure flycheck for each language
   consult-lsp
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
   ;;company-php
   emmet-mode
   web-mode
   
   ;;;; js dev
   js2-mode
   json-mode
   vue-mode

   ;; theme
   zenburn-theme
   leuven-theme

   ;;;; misc
   ledger-mode

   ))

(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)


(provide 'setup-packages)
