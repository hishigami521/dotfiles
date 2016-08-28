;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; ------------------------------------------------------------------------
;; @ cask setting
;; (require 'cask )
(cond ((eq system-type 'darwin)
       (require 'cask))
      ((eq system-type 'cygwin)
       (require 'cask "~/.cask/cask.el"))
      ((eq system-type 'gnu/linux)
       (require 'cask "~/.cask/cask.el"))
      )
(cask-initialize)

;; ------------------------------------------------------------------------
;; @ font
(global-font-lock-mode t)
(setq initial-default-font-size 12)

;; ------------------------------------------------------------------------
;; @ bar
;; (tool-bar-mode -1)
(menu-bar-mode -1)
;; (set-scroll-bar-mode nil)
;; (toggle-scroll-bar nil)

;; title bar
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))

;; ------------------------------------------------------------------------
;; @ line number
(global-linum-mode t)
(set-face-attribute 'linum nil
                    :foreground "#080"
                    :height 0.9)
(setq linum-format "%4d  ")

;; ------------------------------------------------------------------------
;; @ paren
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'mixed)
;;(setq show-paren-style 'expression)
;;(set-face-background 'show-paren-match-face "#500") ;; color for paran

;; ------------------------------------------------------------------------
;; @ mark-mode
(transient-mark-mode t)
;; Color for selected region
(set-face-background 'region "#888")

;; rectangular region
(global-set-key "\C-cv" 'rectangle-mark-mode)


;; ------------------------------------------------------------------------
;; @ recentf
(require 'recentf)
(setq recentf-save-file "~/.emacs.d/cache/.recentf")
(setq recentf-max-saved-items 1000)            ;; recentf に保存するファイルの数
(setq recentf-exclude '(".recentf"))           ;; .recentf自体は含まない
(setq recentf-auto-cleanup 'never)             ;; 保存する内容を整理
(run-with-idle-timer 30 t '(lambda ()          ;; 30秒ごとに .recentf を保存
                             (with-suppressed-message (recentf-save-list))))
;; show on menu
(recentf-mode t)
(setq recentf-max-menu-items 10)

;; ------------------------------------------------------------------------
;; @ savehist
;; automatically saved
(setq savehist-file "~/.emacs.d/cache/savehist/history")
(setq savehist-ignored-variables '(file-name-history))
(savehist-mode 1)
(setq history-length 3000)

;; ------------------------------------------------------------------------
;; @ mini-buffer
(line-number-mode t)
(column-number-mode t)

;; 改行コードを表示する
(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")

;; treatment for symbolic files
(setq vc-follow-symlinks t)

;; ------------------------------------------------------------------------
;; @ startup message
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)

;; ------------------------------------------------------------------------
;; @ list of nil 
(setq-default indent-tabs-mode nil)
;; (setq-default tab-width 4 indent-tabs-mode nil)

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq delete-auto-save-files t)

;; ------------------------------------------------------------------------
;; Underline on cursored column
(setq hl-line-face 'underline)
(global-hl-line-mode)

(setq use-dialog-box nil)
(setq-default cursor-type 'hbar)

;; ------------------------------------------------------------------------
;; @ window-resizer

(defun window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1
              -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1
              -1))
        action c)
    (catch 'end-flag
      (while t
        (setq action
              (read-key-sequence-vector (format "size[%dx%d]"
                                                (window-width)
                                                (window-height))))
        (setq c (aref action 0))
        (cond ((= c ?l)
               (enlarge-window-horizontally dx))
              ((= c ?h)
	       (shrink-window-horizontally dx))
              ((= c ?j)
               (enlarge-window dy))
              ((= c ?k)
               (shrink-window dy))
              ;; otherwise
              (t
               (let ((last-command-char (aref action 0))
                     (command (key-binding action)))
                 (when command
                   (call-interactively command)))
               (message "Quit")
               (throw 'end-flag t)))))))
(global-set-key "\C-c\C-r" 'window-resizer)

;; ;; setting for frame
;; (add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0")
;; (when (require `color-theme)
;;   (color-theme-initialize)
;;   (color-theme-taylor))
;; (setq default-frame-alist
;;       (append (list
;; 	       '(width . 200)  ;;
;; 	       '(height . 55)  ;;
;; 	       '(alpha . (100 100 100 100))
;; 	       )
;; 	      default-frame-alist))

;; ------------------------------------------------------------------------
;; @ global truncation

(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)

;; ------------------------------------------------------------------------
;; @ global key-bind

(global-set-key "\C-ci" 'indent-region)
(global-set-key "\C-cc" 'comment-region)
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\C-o" 'goto-line)
(define-key global-map "\C-c\C-p" nil)
;; (global-set-key "\C-x\C-f" 'anything-my-find-files)

;; ------------------------------------------------------------------------
;; @ swap screen

(defun swap-screen()
  "Swap two screen,leaving cursor at current window."
  (interactive)
  (let ((thiswin (selected-window))
        (nextbuf (window-buffer (next-window))))
    (set-window-buffer (next-window) (window-buffer))
    (set-window-buffer thiswin nextbuf)))
(defun swap-screen-with-cursor()
  "Swap two screen,with cursor in same buffer."
  (interactive)
  (let ((thiswin (selected-window))
        (thisbuf (window-buffer)))
    (other-window 1)
    (set-window-buffer thiswin (window-buffer))
    (set-window-buffer (selected-window) thisbuf)))
(global-set-key [f2] 'swap-screen)
(global-set-key [S-f2] 'swap-screen-with-cursor)

;; show spaces at the end of lines
(when (boundp 'show-trailing-whitespace)
  (setq-default show-trailing-whitespace t))
;; and delete
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

;; ------------------------------------------------------------------------
;; @ flycheck mode
(add-hook 'after-init-hook #'global-flycheck-mode)

;; ------------------------------------------------------------------------
;; @ auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(add-to-list 'ac-modes 'fundamental-mode)
(add-to-list 'ac-modes 'org-mode)
(add-to-list 'ac-modes 'yatex-mode)
(add-to-list 'ac-modes 'nxml-mode)
`(ac-set-trigger-key "TAB")
(setq ac-delay 0.1)
(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)
;; (when (require 'auto-complete-config nil t)
;;   (ac-config-default)
;;   (setq ac-auto-show-menu 0.3)
;; ;; (add-hook 'html-mode-hook 'ac-html-enable)
;; ;; (add-hook 'web-mode-hook 'ac-html-enable))
;; ;; (eval-after-load "web-mode"
;; ;;   '(progn
;; ;;      (add-to-list 'web-mode-ac-sources-alist
;; ;;                   '("html" . (
;; ;;                               ;; attribute-value better to be first
;; ;;                               ac-source-html-attribute-value
;; ;;                               ac-source-html-tag
;; ;;                               ac-source-html-attribute)))))

;; look command with auto-complete
(defun my/ac-look () 
  "`look' command with auto-completelook"
  (interactive)
  (unless (executable-find "look")
    (error "Please install `look' command"))
  (let ((cmd (format "look %s" ac-prefix)))
    (with-temp-buffer
      (call-process-shell-command cmd nil t)
      (split-string-and-unquote (buffer-string) "\n"))))

(defun ac-look ()
  (interactive)
  (let ((ac-menu-height 50)
        (ac-candidate-limit t))
    (auto-complete '(ac-source-look))))
(global-set-key (kbd "C-j") 'ac-look)

(defvar ac-source-look
  '((candidates . my/ac-look)
    (requires . 2)))

;; ------------------------------------------------------------------------
;; @ spell check
(setq-default ispell-program-name "aspell")
(eval-after-load "ispell"
  '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))
;; comment ここに書いたモードではコメント領域のところだけ
(mapc ;; only for comments
 (lambda (hook)
   (add-hook hook 'flyspell-prog-mode))
 '(
   c-mode-common-hook
   emacs-lisp-mode-hook
   sh-mode-hook
   ))
(mapc ;; always 
 (lambda (hook)
   (add-hook hook
             '(lambda () (flyspell-mode 1))))
 '(
   yatex-mode-hook
<<<<<<< HEAD
   ;; latex-mode-hook
        ))
=======
        ))

;; ------------------------------------------------------------------------
;; @ makefile-mode
(autoload 'makefile-mode "makefile-bsdmake-mode"
  "Major mode for editing Makefile files" t)
(add-to-list 'auto-mode-alist '("Makefile" . makefile-bsdmake-mode))
(add-to-list 'auto-mode-alist '("Makefile\\..*$" . makefile-bsdmake-mode))
(add-to-list 'auto-mode-alist '("Makefile_\\..*$" . makefile-bsdmake-mode))
(add-to-list 'auto-mode-alist '("\\.mk\\'" . makefile-bsdmake-mode))
(add-to-list 'ac-modes 'makefile-bsdmake-mode)

;; ------------------------------------------------------------------------
;; @ shell-script-mode
(autoload 'shell-script-mode "sh-mode"
  "Major mode for editing Shell-Script files" t)
(add-to-list 'auto-mode-alist '("[.]zsh.*" . sh-mode))
;; (add-to-list 'auto-mode-alist '("\\.zshrc[.|_].*" . sh-mode))
(add-to-list 'ac-modes 'sh-mode)
>>>>>>> 82d8716d20a94602e713bd57bc662a28e1058c7e

;; ------------------------------------------------------------------------
;; @ markdown-mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode))
;; (setq auto-mode-alist
;;          (cons '("\\.text" . markdown-mode) auto-mode-alist))
(add-to-list 'ac-modes 'markdown-mode)

;; ------------------------------------------------------------------------
;; @ C-mode
(add-hook 'c-mode-hook
          (lambda()
            (c-set-style "stroustrup")))

;; ------------------------------------------------------------------------
;; @ LaTeX
(setq tex-start-commands nil)
(setq latex-run-command "pdflatex -shell-escape")
(setq tex-bibtex-command "bibtex")
(setq platex-run-command "platex -shell-escape")
(setq tex-dvipdf-command "dvipdfmx")
(setq tex-pbibtex-command "pbibtex")
(setq tex-view-pdf-command "evince")

;; (setq tex-fontify-script nil)

(defun tex-get-temp-cmd ()
  "Check the head line of the current buffer, and if it begins by %#!, return the command following it."
  (let ((head-of-buffer (car (split-string (buffer-string) "\n"))))
    (if (and (>= (length head-of-buffer) 3)
             (string= (substring head-of-buffer 0 3) "%#!"))
        (substring head-of-buffer 3))))

(defun my-tex-bibtex-file (&optional arg)
  "Run BibTeX on the current buffer's file."
  (interactive "P")
  (if (tex-shell-running)
      (tex-kill-job)
    (tex-start-shell))
  (let* (shell-dirtrack-verbose
         (source-file (tex-main-file))
         (tex-out-file
          (tex-append (file-name-nondirectory source-file) ""))
         (file-dir (file-name-directory (expand-file-name source-file))))
    (tex-send-command tex-shell-cd-command file-dir)
    (if (consp arg)
        (tex-send-command tex-pbibtex-command tex-out-file)
      (tex-send-command tex-bibtex-command tex-out-file)))
  (tex-display-shell))

(defun my-latex-file (&optional arg)
  "Prompt to save all buffers and run LaTeX on current buffer's file.
This function is more useful than \\[tex-buffer] when you need the
`.aux' file of LaTeX to have the correct name."
  (interactive "P")
  (when tex-offer-save
    (save-some-buffers))
  (let* ((source-file (tex-main-file))
         (file-dir (file-name-directory (expand-file-name source-file))))
    (if (tex-shell-running)
        (tex-kill-job)
      (tex-start-shell))
    (setq tex-temp-cmd (tex-get-temp-cmd))
    (cond (tex-temp-cmd
           (if (<= (length (split-string tex-temp-cmd)) 1)
               (setq tex-temp-cmd (concat tex-temp-cmd " " source-file)))
           (tex-send-tex-command tex-temp-cmd file-dir))
          (t
           (if (not (consp arg))
               (tex-start-tex latex-run-command source-file file-dir)
             (tex-start-tex platex-run-command source-file file-dir)
             (setq tex-print-file (expand-file-name source-file))
             (let ((print-file-name-dvi (tex-append tex-print-file ".dvi")))
               (tex-send-command tex-dvipdf-command print-file-name-dvi nil)))))))

(defun tex-view-pdf (&optional arg)
  "Run PDF viewer."
  (interactive "P")
  (if (tex-shell-running)
      (tex-kill-job)
    (tex-start-shell))
  (let ((view-file-name-pdf (tex-append (buffer-file-name) ".pdf")))
    (tex-send-command tex-view-pdf-command view-file-name-pdf t)))

;; tex-mode hook
(add-hook 'tex-mode-hook
	  (lambda ()
	    (flyspell-mode)
            (ac-flyspell-workaround)
            ;; (ac-l-setup)
	    (turn-on-reftex)
            (define-key latex-mode-map "\C-c\C-c" 'comment-region)
            (define-key latex-mode-map "\C-c\C-f" 'my-latex-file)
            (define-key latex-mode-map "\C-c\C-i" 'my-tex-bibtex-file)
            (define-key latex-mode-map "\C-c\C-v" 'tex-view-pdf)
	    (define-key latex-mode-map "\C-cx" 'reftex-reset-mode)))

;; latex-mode hook
(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (flyspell-mode)
            (ac-flyspell-workaround)
            ;; (ac-l-setup)
	    (turn-on-reftex)
            (flyspell-mode)
            (define-key latex-mode-map "\C-c\C-c" 'comment-region)
            (define-key latex-mode-map "\C-c\C-f" 'my-latex-file)
            (define-key latex-mode-map "\C-c\C-i" 'my-tex-bibtex-file)
            (define-key latex-mode-map "\C-c\C-v" 'tex-view-pdf)
	    (define-key latex-mode-map "\C-cx" 'reftex-reset-mode)))

;;; reftex-default-bib
(setq reftex-default-bibliography '("/home/hishigami/Dropbox/texwork/hishigami.bib"))

;; bibtex-mode hook
(add-hook 'bibtex-mode-hook
	  (lambda ()
	    (turn-on-reftex)
	    (flyspell-mode)
            (ac-flyspell-workaround)
            (ac-l-setup)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ------------------------------------------------------------------------
