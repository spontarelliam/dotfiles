;; -------------------------------
;; Emacs initialization file
;; -------------------------------
;;
(server-start)

(require 'package)
(package-initialize)
(defun refresh-packages ()
  "Refresh packages for ver 24"
  (when (>= emacs-major-version 24)
    (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
    (add-to-list 'package-archives 
                 '("melpa" . "http://melpa.milkbox.net/packages/"))
    (add-to-list 'package-archives
                 '("marmalade" . "http://marmalade-repo.org/packages/"))
    (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
    (package-refresh-contents)
    )
  )
(refresh-packages)



(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(require 'use-package)


(use-package magit
  :bind ("C-x m" . magit-status)
  :ensure t)

(use-package git-gutter
  :init (global-git-gutter-mode t)
  :ensure t)

(use-package smex
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands))
  :ensure t)

(use-package undo-tree
  :init (global-undo-tree-mode 1)
  :ensure t)

(use-package switch-window
  :bind ("C-x o" . switch-window)
  :ensure t)

(use-package jedi
  :defer t
  :ensure t)

(use-package ein
  :defer t
  :ensure t)

(use-package smartparens
  :defer t
  :ensure t)

(use-package fill-column-indicator
  :config (setq-default fill-column 80
			indent-tabs-mode nil)
  :ensure t)

(use-package py-autopep8
  :defer t
  :ensure t)

(use-package auctex
  :defer t
  :ensure t)

;;; Expand-region
(use-package expand-region
  :bind ("M-m" . er/expand-region)
  :ensure t)

(use-package flycheck
  :init (add-hook 'after-init-hook #'global-flycheck-mode)
  :ensure t)

(use-package org-drill
  :defer t
  :ensure f)

(use-package csv-mode
  :defer t
  :ensure f)

;; (use-package discover-my-major
;;   :config ((global-unset-key (kbd "C-h h"))        ; original "C-h h" displays "hello world" in different languages
;;            (define-key 'help-command (kbd "h m") 'discover-my-major))
;;   :ensure t)

(use-package color-theme :ensure t)
(use-package color-theme-solarized
  :init (color-theme-solarized-dark)
  :ensure t)


(add-hook 'shell-mode-hook 
          'ansi-color-for-comint-mode-on)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "1297a022df4228b81bc0436230f211bad168a117282c20ddcba2db8c6a200743" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(org-agenda-files (quote ("~/org/notes.org")))
 '(org-drill-optimal-factor-matrix
   (quote
    ((1
      (2.5 . 4.0)
      (1.7000000000000002 . 3.44)
      (2.36 . 3.86)
      (2.1799999999999997 . 3.72)
      (1.96 . 3.58)
      (2.6 . 4.14)))))
 '(vc-follow-symlinks t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Save all backup files to /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; turn on syntax highlighting for all buffers
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; default to unified diffs
(setq diff-switches "-u")

;; always end a file with a newline
(setq require-final-newline 'query)

;; Enable colors
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Enables text highlighting
(setq transient-mark-mode t)

;; Associate Fortran mode with *.s files
(setq auto-mode-alist (cons '("\\.s$" . fortran-mode) auto-mode-alist))

;; Associate C-mode with *.ino files
(setq auto-mode-alist (cons '("\\.ino$" . c-mode) auto-mode-alist))

;; ;; Associate R-mode with .r and .R files
;; (auto-mode-alist (append (list '("\\.r$" . R-mode) 
;;                                '("\\.R$" . R-mode))
;;                                auto-mode-alist))

;; Show column number
(setq column-number-mode t)

;; automatically save eshell history and stop annoying me
(load "em-hist") ; So the history vars are defined
(if (boundp 'eshell-save-history-on-exit)
    (setq eshell-save-history-on-exit t)) ; Don't ask, just save
;(message "eshell-ask-to-save-history is %s" eshell-ask-to-save-history)
(if (boundp 'eshell-ask-to-save-history)
    (setq eshell-ask-to-save-history 'always)) ; For older(?) version
;(message "eshell-ask-to-save-history is %s" eshell-ask-to-save-history)

;(require 'magit)
(global-unset-key (kbd "C-x g"))
(global-set-key (kbd "C-x g h") 'magit-log)
(global-set-key (kbd "C-x g f") 'magit-file-log)
(global-set-key (kbd "C-x g b") 'magit-blame-mode)
(global-set-key (kbd "C-x g m") 'magit-branch-manager)
(global-set-key (kbd "C-x g c") 'magit-branch)
(global-set-key (kbd "C-x g s") 'magit-status)
(global-set-key (kbd "C-x g r") 'magit-reflog)
(global-set-key (kbd "C-x g t") 'magit-tag)

;; Flyspell
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(autoload 'flyspell-delay-command "flyspell" "Delay on command." t) 
(autoload 'tex-mode-flyspell-verify "flyspell" "" t) 
;; Enable flyspell automatically in Latex mode
(add-hook 'latex-mode-hook 'flyspell-mode)
(add-hook 'tex-mode-hook 'flyspell-mode)

;; Erc Nick Colors (version 24)
;; (add-to-list 'load-path "~/.emacs.d/erc-hl-nicks")
;; (if (>= (string-to-number emacs-version) 24)                        ; this is the test, the "if"
;;     (require 'erc-hl-nicks)
;;   (ding)                                              ; From here on is the "else"
;;     (message "Time to upgrade, don't you think?"))


;; Erc hide join messages
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; Disable irrelevant stuff
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(blink-cursor-mode -1)

;; Enable mouse zooming of images
(global-set-key [C-mouse-4] 'text-scale-increase)
(global-set-key [C-mouse-5] 'text-scale-decrease)

;; Fortran settings
(setq fortran-do-indent 2)
(setq fortran-if-indent 2)
(setq fortran-continuation-indent 4)
(setq fortran-line-number-indent 4)
(setq fortran-comment-line-start "!")
(setq fortran-comment-region "!")
(setq fortran-continuation-string "&")
(setq fortran-comment-indent-style nil)

;; Enable IDO mode for buffer switching
(ido-mode 1)
(setq ido-enable-flex-matching t)

(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

(global-set-key (read-kbd-macro "<M-DEL>") 'backward-delete-word)

;; Rebind M-d so that it deletes instead of kills.
;; This prevents addition to the kill ring.
(global-unset-key (kbd "M-d"))
(global-set-key (kbd "M-d") 'delete-word)

;; visual window switching
(add-to-list 'load-path "~/.emacs.d/switch-window-master")
(require 'switch-window)
(global-set-key (kbd "C-x o") 'switch-window)

;;(require 'sunrise-commander)

;; This forces ediff to split windows vertically. Yes, the nomenclature
;; backwards.
(setq ediff-split-window-function 'split-window-horizontally)

;; Set the shell path for Windows
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

;; Python
;; Standard Jedi.el setting
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)

(add-hook 'python-mode-hook 'fci-mode)

;; Python IDE
(setq py-install-directory "/home/smed/Downloads/python-mode.el-6.1.3")
(add-to-list 'load-path py-install-directory)
;(require 'python-mode)

;; use IPython
(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")
; use the wx backend, for both mayavi and matplotlib
(setq py-python-command-args
  '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
(setq py-force-py-shell-name-p t)

; switch to the interpreter after executing code
;; (setq py-shell-switch-buffers-on-execute-p t)
;; (setq py-switch-buffers-on-execute-p t)
; don't split windows
;; (setq py-split-windows-on-execute-p nil)
; try to automagically figure out indentation
(setq py-smart-indentation t)


;(set-frame-font "-xos4-terminus-medium-r-normal--14-140-*-*-*-*-*-*" nil t)

(add-to-list 'load-path "~/.emacs.d/el-get/ein/lisp")
(require 'ein)

;; Remap disabled C-x C-u to undo
(define-key global-map "\C-x\C-u" 'undo)


;; Smex is a M-x enhancement
(require 'smex) ; Not needed if you use package.el
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                  ; when Smex is auto-initialized on its first run.
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Suppress the startup message
(setq inhibit-startup-screen t)

;; UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; y or n instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; set the fill-column to 80 characters and prevent tab characters
(setq-default fill-column 80)
(setq-default indent-tabs-mode nil)

;; hide the mouse while typing
(setq make-pointer-invisible t)

;; require a newline at the end of files
(setq require-final-newline t)

;; highlight matching parentheses
(show-paren-mode 1)

;; Org mode
;; syntax highlighting for code blocks
(setq org-src-fontify-natively t)
(setq org-startup-indented t)
(global-visual-line-mode 1)
;; (setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cc" 'org-capture)


;; load babel supported languages
(org-babel-do-load-languages
 'org-babel-load-languages
  '((python . t)
     (emacs-lisp . t)))


;; C-x k: kill current buffer without asking
(defun kill-this-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'kill-this-buffer)



;; Use squid proxy
;(setq url-proxy-services '(("http" . "10.2.129.209:3128")))

;; Run autopep8 when saving .py files
(require 'py-autopep8)
(add-hook 'before-save-hook 'py-autopep8-before-save)
(setq py-autopep8-options '("--max-line-length=80"))

;; image viewer
;; (require 'eiv)

;; clocktable format avoid converting 24 hr to day
(setq org-time-clocksum-format (quote (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)))
(setq org-log-into-drawer 3)
;; Increase the size of latex equation rendering
(setq org-format-latex-options '(:foreground default :background default :scale 1.4 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
              ("begin" "$1" "$" "$$" "\\(" "\\[")))

;; Doc-view
;; Navigate pages with M-[ or M-] without being in the doc-view buffer.
(fset 'doc-prev "\C-xo\C-x[\C-xo")
(fset 'doc-next "\C-xo\C-x]\C-xo")
(global-set-key (kbd "M-[") 'doc-prev)
(global-set-key (kbd "M-]") 'doc-next)
(setq org-return-follows-link t)

;; If switch to buffer that's already open in another frame,
;; don't switch to that window, just open it again in current window
(setq switch-to-buffer-preserve-window-point 'already-displayed)

(setq doc-view-continuous t)

;; press F8 on keypad to lookup dictionary definition
(global-set-key (kbd "<f8>") 'dictionary-lookup-definition)

;; Stop a currently running clock on killing emacs
(defun org-clock-out-maybe ()
     (org-clock-out nil t)
     (org-save-all-org-buffers))
(add-hook 'kill-emacs-hook 'org-clock-out-maybe)


;; (add-to-list 'load-path "~/.emacs.d/elpa/auctex-11.88")
;; (load "auctex.el" nil t t)
;; (load "preview.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; select text objects incrementally
(require 'expand-region)
(global-set-key (kbd "M-m") 'er/expand-region)

;; language syntax checking
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; A quick major mode help with discover-my-major
(global-unset-key (kbd "C-h h"))        ; original "C-h h" displays "hello world" in different languages
(define-key 'help-command (kbd "h m") 'discover-my-major)

(setq org-directory "~/org")
(setq org-agenda-files '("~/org/notes.org"))
(setq org-mobile-files '("~/org"))
(setq org-mobile-directory "~/Dropbox/MobileOrg")

(add-to-list 'load-path "~/.emacs.d/relap-mode")
(require 'relap-mode)
(require 'r5out-mode)

(add-hook 'TeX-mode-hook
          (lambda () (TeX-fold-mode 1))); automatically activate TeX-fold-mode
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)

;; folding for auctex
(add-hook 'outline-minor-mode-hook
            (lambda () (local-set-key "\C-c\C-a"
                                      outline-mode-prefix-map)))
