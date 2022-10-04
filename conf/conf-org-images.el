(setq remfils/image-refile-directory "~/refile-images/")

(when (boundp 'sync-org-path)
  (setq remfils/image-refile-directory (concat sync-org-path "refile-images/")))

(defun remfils/open-images-to-refile()
    (interactive)
    (dired remfils/image-refile-directory "-a"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; image input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: refactor

(defun remfils/org-paste-image-refile-link ()
  "Switch between or create eshell buffers using helm"
  (interactive)
  (helm :sources
        (helm-build-sync-source "refile-images"
          :candidates
          (lambda ()
            (seq-filter
             (lambda (x) (not (member x '("." ".."))))
             (mapcar
              (apply-partially #'nth 0)
              (sort
               (directory-files-and-attributes remfils/image-refile-directory)
               #'(lambda (x y) (time-less-p (nth 6 y) (nth 6 x)))))))
          :action (list
                   (cons
                    "Paste image"
                    (lambda (candidate)
                      (insert (concat "[[" remfils/image-refile-directory candidate "][" candidate "]")))))
          ;; make the candidates get re-generated on input, so one can
          ;; actually create an eshell in a new directory
          :volatile t)
        ;;:buffer "*pa*"
        :prompt "org image: "))

(provide 'conf-org-images)
;;; init-org-images.el ends here
