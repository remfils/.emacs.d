;;; org-yt.el --- Org youtube links.                 -*- lexical-binding: t; -*-

;; author: https://github.com/tobiaszawada/org-yt

;; copyright (c) 2018  u-esi-internal\toz

;; author: u-esi-internal\toz <toz@smtp.1und1.de>
;; keywords: multimedia

;; this program is free software; you can redistribute it and/or modify
;; it under the terms of the gnu general public license as published by
;; the free software foundation, either version 3 of the license, or
;; (at your option) any later version.

;; this program is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  see the
;; gnu general public license for more details.

;; you should have received a copy of the gnu general public license
;; along with this program.  if not, see <http://www.gnu.org/licenses/>.

;;; commentary:

;; idea from  https://emacs.stackexchange.com/questions/38098/org-mode-custom-youtube-link-syntax

;;; code:

(require 'org)
(require 'org-element)

(defcustom org-yt-url-protocol "yt"
  "protocol identifier for youtube links."
  :group 'org-yt
  :type 'string)

(defun org-image-update-overlay (file link &optional data-p refresh)
  "create image overlay for file associtated with org-element link.
if data-p is non-nil file is not a file name but a string with the image data.
if refresh is non-nil don't download the file but refresh the image.
see also `create-image'.
this function is almost a duplicate of a part of `org-display-inline-images'."
  (when (or data-p (file-exists-p file))
    (let ((width
           ;; apply `org-image-actual-width' specifications.
           (cond
            ((not (image-type-available-p 'imagemagick)) nil)
            ((eq org-image-actual-width t) nil)
            ((listp org-image-actual-width)
             (or
              ;; first try to find a width among
              ;; attributes associated to the paragraph
              ;; containing link.
              (let ((paragraph
                     (let ((e link))
                       (while (and (setq e (org-element-property
                                            :parent e))
                                   (not (eq (org-element-type e)
                                            'paragraph))))
                       e)))
                (when paragraph
                  (save-excursion
                    (goto-char (org-element-property :begin paragraph))
                    (when
                        (re-search-forward
                         "^[ \t]*#\\+attr_.*?: +.*?:width +\\(\\s-+\\)"
                         (org-element-property
                          :post-affiliated paragraph)
                         t)
                      (string-to-number (match-string 1))))))
              ;; otherwise, fall-back to provided number.
              (car org-image-actual-width)))
            ((numberp org-image-actual-width)
             org-image-actual-width)))
          (old (get-char-property-and-overlay
                (org-element-property :begin link)
                'org-image-overlay)))
      (if (and (car-safe old) refresh)
          (image-refresh (overlay-get (cdr old) 'display))
        (let ((image (create-image file
                                   (and width 'imagemagick)
                                   data-p
                                   :width width)))
          (when image
            (let* ((link
                    ;; if inline image is the description
                    ;; of another link, be sure to
                    ;; consider the latter as the one to
                    ;; apply the overlay on.
                    (let ((parent
                           (org-element-property :parent link)))
                      (if (eq (org-element-type parent) 'link)
                          parent
                        link)))
                   (ov (make-overlay
                        (org-element-property :begin link)
                        (progn
                          (goto-char
                           (org-element-property :end link))
                          (skip-chars-backward " \t")
                          (point)))))
              (overlay-put ov 'display image)
              (overlay-put ov 'face 'default)
              (overlay-put ov 'org-image-overlay t)
              (overlay-put
               ov 'modification-hooks
               (list 'org-display-inline-remove-overlay))
              (push ov org-inline-image-overlays)
              ov)))))))

(defun org-yt-get-image (url)
  "retrieve image from url."
  (let ((image-buf (url-retrieve-synchronously url)))
    (when image-buf
      (with-current-buffer image-buf
        (goto-char (point-min))
        (when (looking-at "http/")
          (delete-region (point-min)
                         (progn (re-search-forward "\n[\n]+")
                                (point))))
        (buffer-substring-no-properties (point-min) (point-max))))))

(defconst org-yt-video-id-regexp "[-_[:alnum:]]\\{10\\}[aeimquycgkosw048]"
  "regexp matching youtube video id's taken from `https://webapps.stackexchange.com/questions/54443/format-for-id-of-youtube-video'.")

(defun org-yt-follow (video-id)
  "open youtube with video-id."
  (browse-url (concat "https://youtu.be/" video-id)))

(defun org-yt-image-data-fun (_protocol link _description)
  "get image corresponding to link from youtube.
use this as :image-data-fun property in `org-link-properties'.
see `org-display-user-inline-images' for a description of :image-data-fun."
  (when (string-match org-yt-video-id-regexp link)
    (org-yt-get-image (format "http://img.youtube.com/vi/%s/0.jpg" link))))

(org-link-set-parameters org-yt-url-protocol
			 :follow #'org-yt-follow
			 :image-data-fun #'org-yt-image-data-fun)

(require 'subr-x)

(defun org-display-user-inline-images (&optional _include-linked _refresh beg end)
  "like `org-display-inline-images' but for image data links.
_INCLUDE-LINKED and _REFRESH are ignored.
Restrict to region between BEG and END if both are non-nil.
Image data links have a :image-data-fun parameter.
\(See `org-link-set-parameters'.)
The value of the :image-data-fun parameter is a function
taking the PROTOCOL, the LINK, and the DESCRIPTION as arguments.
If that function returns nil the link is not interpreted as image.
Otherwise the return value is the image data string to be displayed.

Note that only bracket links are allowed as image data links
with one of the formats [[PROTOCOL:LINK]] or [[PROTOCOL:LINK][DESCRIPTION]] are recognized."
  (interactive)
  (when (and (called-interactively-p 'any)
             (use-region-p))
    (setq beg (region-beginning)
          end (region-end)))
  (when (display-graphic-p)
    (org-with-wide-buffer
     (goto-char (or beg (point-min)))
     (when-let ((image-data-link-parameters
		 (cl-loop for link-par-entry in org-link-parameters
			  with fun
			  when (setq fun (plist-get (cdr link-par-entry) :image-data-fun))
			  collect (cons (car link-par-entry) fun)))
		(image-data-link-re (regexp-opt (mapcar 'car image-data-link-parameters)))
		(re (format "\\[\\[\\(%s\\):\\([^]]+\\)\\]\\(?:\\[\\([^]]+\\)\\]\\)?\\]"
			    image-data-link-re)))
       (while (re-search-forward re end t)
         (let* ((protocol (match-string-no-properties 1))
		(link (match-string-no-properties 2))
		(description (match-string-no-properties 3))
		(image-data-link (assoc-string protocol image-data-link-parameters))
		(el (save-excursion (goto-char (match-beginning 1)) (org-element-context)))
		image-data)
           (when el
             (setq image-data
                   (or (let ((old (get-char-property-and-overlay
                                   (org-element-property :begin el)
                                   'org-image-overlay)))
                         (and old
                              (car-safe old)
                              (overlay-get (cdr old) 'display)))
		       (funcall (cdr image-data-link) protocol link description)))
             (when image-data
               (let ((ol (org-image-update-overlay image-data el t t)))
                 (when (and ol description)
                   (overlay-put ol 'after-string description)))))))))))

(advice-add #'org-display-inline-images :after #'org-display-user-inline-images)

;; org http|https images

(defun org-image-link (protocol link _description)
  "Interpret LINK as base64-encoded image data."
  (cl-assert (string-match "\\`img" protocol) nil
             "Expected protocol type starting with img")
  (let ((buf (url-retrieve-synchronously (concat (substring protocol 3) ":" link))))
    (cl-assert buf nil
               "Download of image \"%s\" failed." link)
    (with-current-buffer buf
      (goto-char (point-min))
      (re-search-forward "\r?\n\r?\n")
      (buffer-substring-no-properties (point) (point-max)))))

(org-link-set-parameters
 "imghttp"
 :image-data-fun #'org-image-link)

(org-link-set-parameters
 "imghttps"
 :image-data-fun #'org-image-link)

(provide 'org-yt)
;;; org-yt.el ends here
