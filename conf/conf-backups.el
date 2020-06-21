;;; init-ledger.el --- Support for the ledger CLI accounting tool -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(setq
 make-backup-files t
 backup-directory-alist `(("." . "~/.saves"))
 backup-by-copying t
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t
 )


(provide 'conf-backups)
;;; init-ledger.el ends here
