
;; -*- coding: utf-8; lexical-binding: t; -*-
;; basic gnu emacs settings. Everything in this file is for plain gnu emacs only.
;; for emacs 28 or later.
;; 2022-11-11

;; ssss---------------------------------------------------

;; UTF-8 as default encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)

;; ssss---------------------------------------------------
;; initial window and default window

(setq inhibit-startup-screen t)

(setq default-frame-alist
      (if (display-graphic-p)
          '((tool-bar-lines . 0)
            (background-color . "honeydew")
            (width . 80)
            (height . 46))
        '((tool-bar-lines . 0))))

;; ssss---------------------------------------------------
;; Emacs: Font Setup http://xahlee.info/emacs/emacs/emacs_list_and_set_font.html

(set-frame-font
 (cond
  ((string-equal system-type "windows-nt")
   (if (member "Consolas" (font-family-list)) "Consolas-14" nil ))
  ((string-equal system-type "darwin")
   (if (member "Menlo" (font-family-list)) "Menlo-16" nil ))
  ((string-equal system-type "gnu/linux")
   (if (member "DejaVu Sans Mono" (font-family-list)) "DejaVu Sans Mono" nil ))
  (t nil))
 nil t
 )

;; set font for symbols (symbol . [8220 8704 9472])
(set-fontset-font
 t
 'symbol
 (cond
  ((string-equal system-type "windows-nt")
   (cond
    ((member "Segoe UI Symbol" (font-family-list)) "Segoe UI Symbol")))
  ((string-equal system-type "darwin")
   (cond
    ((member "Apple Symbols" (font-family-list)) "Apple Symbols")))
  ((string-equal system-type "gnu/linux")
   (cond
    ((member "Symbola" (font-family-list)) "Symbola")))))

(progn
  ;; set font for emoji (if before emacs 28, should come after setting symbols. emacs 28 now has 'emoji . before, emoji is part of 'symbol)
  (set-fontset-font
   t
   (if (version< emacs-version "28.1")
       '(#x1f300 . #x1fad0)
     'emoji
     )
   (cond
    ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
    ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
    ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
    ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
    ((member "Symbola" (font-family-list)) "Symbola"))))

;; set font for cuneiform
(set-fontset-font
 t
 'cuneiform
 (cond
  ((string-equal system-type "windows-nt")
   (cond
    ((member "Segoe UI Historic" (font-family-list)) "Segoe UI Historic")))
  ((string-equal system-type "darwin")
   (cond
    ((member "Noto Sans Cuneiform Regular" (font-family-list)) "Noto Sans Cuneiform Regular")))
  ((string-equal system-type "gnu/linux")
   (cond
    ((member "Noto Sans Cuneiform Regular" (font-family-list)) "Noto Sans Cuneiform Regular")))))

(set-fontset-font
 t
 'phoenician
 (cond
  ((string-equal system-type "windows-nt")
   (cond
    ((member "Segoe UI Historic" (font-family-list)) "Segoe UI Historic")))
  ((string-equal system-type "darwin")
   (cond
    ((member "Noto Sans Phoenician Regular" (font-family-list)) "Noto Sans Phoenician Regular")))
  ((string-equal system-type "gnu/linux")
   (cond
    ((member "Noto Sans Phoenician Regular" (font-family-list)) "Noto Sans Phoenician Regular")))))

(set-fontset-font
 t
 'deseret
 (cond
  ((string-equal system-type "windows-nt")
   (cond
    ((member "Segoe UI Symbol" (font-family-list)) "Segoe UI Symbol")))
  ((string-equal system-type "darwin")
   (cond
    ((member "Apple Symbols" (font-family-list)) "Apple Symbols")))
  ((string-equal system-type "gnu/linux")
   (cond
    ((member "Noto Sans Deseret" (font-family-list)) "Noto Sans Deseret")))))

(set-fontset-font
 t
 'shavian
 (cond
  ((string-equal system-type "windows-nt")
   (cond
    ((member "Segoe UI Historic" (font-family-list)) "Segoe UI Historic")))
  ((string-equal system-type "darwin")
   (cond
    ((member "Apple Symbols" (font-family-list)) "Apple Symbols")))
  ((string-equal system-type "gnu/linux")
   (cond
    ((member "Noto Sans Shavian Regular" (font-family-list)) "Noto Sans Shavian Regular")))))

(set-fontset-font
 t
 'egyptian
 (cond
  ((string-equal system-type "windows-nt")
   (cond
    ((member "Segoe UI Historic" (font-family-list)) "Segoe UI Historic")))
  ((string-equal system-type "darwin")
   (cond
    ((member "Noto Sans Egyptian Hieroglyphs" (font-family-list)) "Noto Sans Egyptian Hieroglyphs")))
  ((string-equal system-type "gnu/linux")
   (cond
    ((member "Aegyptus" (font-family-list)) "Aegyptus")))))

;; set font for chinese
(set-fontset-font
 t
 'han
 (cond
  ((string-equal system-type "windows-nt")
   (cond
    ((member "Microsoft YaHei" (font-family-list)) "Microsoft YaHei")
    ((member "Microsoft JhengHei" (font-family-list)) "Microsoft JhengHei")
    ((member "SimHei" (font-family-list)) "SimHei")))
  ((string-equal system-type "darwin")
   (cond
    ((member "Hei" (font-family-list)) "Hei")
    ((member "Heiti SC" (font-family-list)) "Heiti SC")
    ((member "Heiti TC" (font-family-list)) "Heiti TC")))
  ((string-equal system-type "gnu/linux")
   (cond
    ((member "WenQuanYi Micro Hei" (font-family-list)) "WenQuanYi Micro Hei")))))

;; ssss---------------------------------------------------
;; backup and file related

(defun xah-save-all-unsaved ()
  "Save all unsaved files. no ask.
Version 2019-11-05"
  (interactive)
  (save-some-buffers t ))

(if (version< emacs-version "27")
    (add-hook 'focus-out-hook 'xah-save-all-unsaved)
  (setq after-focus-change-function 'xah-save-all-unsaved))

(setq make-backup-files nil)
(setq backup-by-copying t)
(setq create-lockfiles nil)
(setq auto-save-default nil)

(require 'recentf)
(recentf-mode 1)

(progn
  ;; (desktop-save-mode 1)
  (setq desktop-restore-frames t)
  (setq desktop-auto-save-timeout 300)
  (setq desktop-globals-to-save nil)
  ;; (setq desktop-globals-to-save '(desktop-missing-file-warning tags-file-name tags-table-list search-ring regexp-search-ring register-alist file-name-history))
  (setq desktop-save t))

(global-auto-revert-mode 1)

;; ssss---------------------------------------------------
;; user interface

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(column-number-mode 1)
(blink-cursor-mode 0)
(setq use-dialog-box nil)

(progn
  ;; no need to warn
  (put 'narrow-to-region 'disabled nil)
  (put 'narrow-to-page 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'erase-buffer 'disabled nil)
  (put 'scroll-left 'disabled nil)
  (put 'dired-find-alternate-file 'disabled nil)
)

;; ssss---------------------------------------------------

(progn
  (require 'dired-x)
  (setq dired-dwim-target t)
  ;; (cond
  ;;  ((string-equal system-type "gnu/linux") (setq dired-listing-switches "-al --time-style long-iso"))
  ;;  ((string-equal system-type "darwin") (setq dired-listing-switches "-alh")))
  (setq dired-recursive-copies 'top)
  (setq dired-recursive-deletes 'top))

;; ssss---------------------------------------------------

(setq set-mark-command-repeat-pop nil)
(setq mark-ring-max 10)
(setq global-mark-ring-max 10)

;; ssss---------------------------------------------------

(progn
  ;; minibuffer setup
  (setq enable-recursive-minibuffers t)
  (savehist-mode 0)
  ;; big minibuffer height, for ido to show choices vertically
  (setq max-mini-window-height 0.5)
  ;; minibuffer, stop cursor going into prompt
  (customize-set-variable
   'minibuffer-prompt-properties
   (quote (read-only t cursor-intangible t face minibuffer-prompt))))

(if (version< emacs-version "28.1")
    (progn
      (progn
        ;; make buffer switch command do suggestions, also for find-file command
        (require 'ido)
        (ido-mode 1)
        ;; show choices vertically
        (setf (nth 2 ido-decorations) "\n")
        ;; show any name that has the chars you typed
        (setq ido-enable-flex-matching t)
        ;; use current pane for newly opened file
        (setq ido-default-file-method 'selected-window)
        ;; use current pane for newly switched buffer
        (setq ido-default-buffer-method 'selected-window)
        )
      (progn
        ;; minibuffer enhanced completion icomplete
        (require 'icomplete)
        (icomplete-mode 1)
        ;; show choices vertically
        (setq icomplete-separator "\n")
        (setq icomplete-hide-common-prefix nil)
        (setq icomplete-in-buffer t)
        (define-key icomplete-minibuffer-map (kbd "<right>") 'icomplete-forward-completions)
        (define-key icomplete-minibuffer-map (kbd "<left>") 'icomplete-backward-completions)))
  (fido-vertical-mode 1))

;; ssss---------------------------------------------------

;; remember cursor position
(if (version< emacs-version "25.0")
    (progn
      (require 'saveplace)
      (setq-default save-place t))
  (save-place-mode 1))

;; ssss---------------------------------------------------
;;; editing related

;; make typing delete/overwrites selected text
(delete-selection-mode 1)

;; disable shift select
(setq shift-select-mode nil)

(electric-pair-mode 1)

;; set highlighting brackets
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)

;; for isearch-forward, make these equivalent: space newline tab hyphen underscore
(setq search-whitespace-regexp "[-_ \t\n]+")

(defun xah-toggle-search-whitespace ()
  "Set `search-whitespace-regexp' to nil or includes hyphen lowline tab newline.
Explanation: When in isearch (M-x `isearch-forward'), space key can also stand for other chars such as hyphen lowline tab newline. It depend on a regex. It's convenient. But sometimes you want literal. This command makes it easy to toggle.

Emacs Isearch Space Toggle
URL `http://xahlee.info/emacs/emacs/emacs_isearch_space.html'
Version 2019-02-22 2021-11-13"
  (interactive)
  (if (string-equal search-whitespace-regexp nil)
      (progn
        (setq search-whitespace-regexp "[-_ \t\n]+")
        (message "Space set to hyphen lowline tab newline space"))
    (progn
      (setq search-whitespace-regexp nil)
      (message "Space set to literal."))))

;; 2015-07-04 bug of pasting in emacs.
;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=16737#17
;; http://xahlee.info/emacs/misc/emacs_bug_cant_paste_2015.html
;; (setq x-selection-timeout 300)
(setq save-interprogram-paste-before-kill t)
(setq x-select-enable-clipboard-manager nil)

;; ssss---------------------------------------------------
;; indentation, end of line

(electric-indent-mode 0)

(set-default 'tab-always-indent 'complete)

;; no mixed tab space
(setq-default indent-tabs-mode nil)
 ; gnu emacs 23.1, 24.4.1 default is t

;; 4 is more popular than 8.
(setq-default tab-width 1)

(setq sentence-end-double-space nil )

;; ssss---------------------------------------------------

;; load emacs 24's package system. Add MELPA repository.
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "https://melpa.org/packages/")
   t))

;; ssss---------------------------------------------------

(progn
  ;; Make whitespace-mode with very basic background coloring for whitespaces.
  ;; http://xahlee.info/emacs/emacs/whitespace-mode.html
  (setq whitespace-style (quote (face spaces tabs newline space-mark tab-mark newline-mark)))

  ;; Make whitespace-mode and whitespace-newline-mode use â€œÂ¶â€ for end of line char and â€œâ–·â€ for tab.
  (setq whitespace-display-mappings
        ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
        '((space-mark 32 [183] [46]) ; SPACE 32 ã€Œ ã€, 183 MIDDLE DOT ã€ŒÂ·ã€, 46 FULL STOP ã€Œ.ã€
          (newline-mark 10 [182 10]) ; LINE FEED,
          (tab-mark 9 [9655 9] [92 9]) ; tab
          )))

;; ssss---------------------------------------------------
;; edit related

(setq hippie-expand-try-functions-list
      '(
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        ;; try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-complete-file-name-partially
        try-complete-file-name
        ;; try-expand-all-abbrevs
        ;; try-expand-list
        ;; try-expand-line
        ))

;; ssss---------------------------------------------------

(if (version< emacs-version "28.1")
    (defalias 'yes-or-no-p 'y-or-n-p)
  (setq use-short-answers t))

;; ssss---------------------------------------------------

(when (fboundp 'eww)
  (defun xah-rename-eww-buffer ()
    "Rename `eww-mode' buffer so sites open in new page.
URL `http://xahlee.info/emacs/emacs/emacs_eww_web_browser.html'
Version 2017-11-10"
    (let (($title (plist-get eww-data :title)))
      (when (eq major-mode 'eww-mode )
        (if $title
            (rename-buffer (concat "eww " $title ) t)
          (rename-buffer "eww" t)))))

  (add-hook 'eww-after-render-hook 'xah-rename-eww-buffer))

;; 2021-12-21. fuck Alan Mackenzie
;; Emacs Lisp Doc String Curly Quote Controversy
;; http://xahlee.info/emacs/misc/emacs_lisp_curly_quote_controversy.html
(setq text-quoting-style 'straight)

(setq mouse-highlight nil)

(setq line-move-visual t)

(setq byte-compile-docstring-max-column 999)

(if (version< emacs-version "28.1")
    nil
  (setq mode-line-compact t))

;; ssss---------------------------------------------------

;; (global-tab-line-mode)

(tooltip-mode -1)