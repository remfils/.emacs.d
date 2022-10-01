;;; init-python.el --- Python editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'openwith)
(require 'openwith)

(setq remfils/programs
      (cond
       ;; windows
       ((string-equal system-type "windows-nt")
        '(("writer" . "C:\\Program Files\\LibreOffice\\program\\swriter.exe")
          ("calc" . "C:\\Program Files\\LibreOffice\\program\\scalc.exe")
          ("movie" . "C:\\Program Files\\VideoLAN\\VLC\\vlc.exe"))
        )
       ;; linux
       ((string-equal system-type "gnu/linux") ; linux
        '(("writer" . "libreoffice")
          ("calc" . "libreoffice")
          ("movie" . "vlc"))
        )))

(setq openwith-associations
      (list
       (list (openwith-make-extension-regexp
              '("doc" "docx" "odt"))
             (cdr (assoc "writer" remfils/programs))
             '(file))
       (list (openwith-make-extension-regexp
              '("xls" "xlsx" "ods"))
             (cdr (assoc "calc" remfils/programs))
             '(file))
       (list (openwith-make-extension-regexp
              '("mp4" "avi"))
             (cdr (assoc "movie" remfils/programs))
             '(file))
       ))

(openwith-mode 1)

(provide 'init-openwith)
;;; init-python.el ends here
