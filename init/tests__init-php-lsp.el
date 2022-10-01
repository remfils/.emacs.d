(require-package 'lsp-mode)
(require 'lsp-mode)

;; Managed to start using lsp via tcp
;;
;; External server is running on port 8043 via console command and lsp
;; client is connecting to it
;; 
;; Result - lsp server communication works but still not good enough

(setq lsp--tcp-server-port 8043)
(setq lsp--tcp-port 8043)
(setq lsp-keymap-prefix "C-c l")

(defun remfils/lsp-tcp-connection-query ()
    "Create connection which will ask for server and port before starting."
    (list
     :connect (lambda (filter sentinel name environment-fn)
                (let* ((host "localhost")
                       (port 8043)
                       (tcp-proc (lsp--open-network-stream host port (concat name "::tcp"))))
                  (set-process-query-on-exit-flag tcp-proc nil)
                  (set-process-filter tcp-proc filter)
                  (cons tcp-proc tcp-proc)))
     :test? (lambda () t)))


(defun remfils/lsp-serenata-server-start-fun (port)
  "Define serenata start function, it requires a PORT."
  `("php7" "/home/remfils/Downloads/test.php"
    "-u" ,(number-to-string port)))

(lsp-register-client
 (make-lsp-client
  :new-connection (remfils/lsp-tcp-connection-query)
  :activation-fn (lsp-activate-on "php")
  :priority -2
  :notification-handlers (ht ("serenata/didProgressIndexing"
                              (lambda (_server data)
                                (lsp-log "%s" (lsp:serenata-did-progress-indexing-info data)))))

  :initialization-options #'lsp-serenata-init-options
  :initialized-fn (lambda (workspace)
                    (when (equal (length lsp-serenata-uris) 0)
                      (let* ((lsp-root (lsp--path-to-uri (lsp-workspace-root))))
                        (setq lsp-serenata-uris (vector lsp-root))))
                    (with-lsp-workspace workspace
                      (lsp--set-configuration
                       (lsp-configuration-section "serenata"))))
  :server-id 'remfils/serenata))

(setq lsp-enabled-clients '(remfils/serenata))
(setq lsp-disabled-clients '(php-ls iph intelephense serenata))


;;(setq lsp-serenata-server-path '("php7", "/home/remfils/Downloads/distribution.phar"))




;;;; NOTE: lsp junk
;; optimisations
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
;; (setq gc-cons-threshold 100000000)
;; (setq read-process-output-max (* 1024 1024))
;; (add-hook
;;  'hack-local-variables-hook
;;  (lambda () (when (derived-mode-p 'php-mode) (lsp) (define-key lsp-mode-map (kbd "C-x C-l") lsp-command-map))))

;; (add-hook 'hack-local-variables-hook (lambda () (when (derived-mode-p 'php-mode) (lsp))))


;; (setq lsp-lens-enable nil)
;; (setq lsp-headerline-breadcrumb-enable nil)

;; (defun remfils-php/toggle-lsp-mode()
;;   (interactive)
  
;;   (if (bound-and-true-p lsp-mode)
;;       (progn
;;         (lsp-disconnect)
;;         (lsp-mode -1))
;;     (progn
;;       (setq-local
;;        lsp-clients-php-server-command (quote ("php" "/home/remfils/Projects/tresio/vendor/felixfbecker/language-server/bin/php-language-server.php")))
;;       (lsp)
;;       (define-key lsp-mode-map (kbd "C-x C-l") lsp-command-map))))
 

(provide 'tests__init-php-lsp)
