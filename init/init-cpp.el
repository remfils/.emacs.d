;;; init-cpp.el --- Working with windows within frames -*- lexical-binding: t -*-
;;; Commentary:

;; This is not about the "Windows" OS, but rather Emacs's "windows"
;; concept: these are the panels within an Emacs frame which contain
;; buffers.

;;; Code:



(c-add-style "microsoft"
          '("stroustrup"
            (c-offsets-alist
             (innamespace . -)
             (inline-open . 0)
             (inher-cont . c-lineup-multi-inher)
             (arglist-cont-nonempty . +)
             (template-args-cont . +))))

(setq c-default-style "microsoft")

(defun remfils/code-compile()
  (interactive)
  (compile compile-command))

(global-set-key (kbd "<f9>") 'remfils/code-compile)

(provide 'init-cpp)
;;; init-cpp.el ends here
