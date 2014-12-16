;; -------------------------------
;; Emacs initialization file
;; -------------------------------
;;
(server-start)

;; Package manager for ver 24
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives 
               '("melpa" . "http://melpa.milkbox.net/packages/"))
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (package-refresh-contents)
  )

;; install necessary packages
(defvar install-packages '(magit git-gutter smex switch-window jedi ein smartparens undo-tree fill-column-indicator py-autopep8))
(dolist (pack install-packages)
  (unless (package-installed-p pack)
    (package-install pack)))

;; Solarized color theme
(add-to-list 'load-path "~/.emacs.d/colors/emacs-color-theme-solarized")
(require 'color-theme-solarized)
  (eval-after-load "color-theme-solarized"
   '(progn
      (color-theme-initialize)
      (color-theme-solarized-dark)))

(add-hook 'shell-mode-hook 
          'ansi-color-for-comint-mode-on)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-drill-optimal-factor-matrix (quote ((1 (2.5 . 4.0) (1.7000000000000002 . 3.44) (2.36 . 3.86) (2.1799999999999997 . 3.72) (1.96 . 3.58) (2.6 . 4.14)))))
 '(org-modules (quote (org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m org-drill org-learn)))
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

(require 'magit)

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
(menu-bar-mode -1)
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


;; Standard el-get setup
;; (See also: https://github.com/dimitri/el-get#basic-setup)
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(el-get 'sync)


;; Python
;; Standard Jedi.el setting
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

(add-hook 'python-mode-hook 'fci-mode)

;; Python IDE
(setq py-install-directory "/home/smed/Downloads/python-mode.el-6.1.3")
(add-to-list 'load-path py-install-directory)
(require 'python-mode)

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


(set-frame-font "-xos4-terminus-medium-r-normal--14-140-*-*-*-*-*-*" nil t)

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

;; git
(global-git-gutter-mode t)

;; Undo
(global-undo-tree-mode)

;; C-x k: kill current buffer without asking
(defun kill-this-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'kill-this-buffer)

(require 'org-drill)

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

;; ;; Org-mode allow relative file refiling
;; ;; any headline with level <= 2 is a target
;; (setq org-refile-targets '((nil :maxlevel . 2)
;;                                 ; all top-level headlines in the
;;                                 ; current buffer are used (first) as a
;;                                 ; refile target
;;                            (org-agenda-files :maxlevel . 2)))

;; ;; provide refile targets as paths, including the file name
;; ;; (without directory) as level 1 of the path
;; (setq org-refile-use-outline-path 'file)

;; ;; allow to create new nodes (must be confirmed by the user) as
;; ;; refile targets
;; (setq org-refile-allow-creating-parent-nodes 'confirm)

;; ;; refile only within the current buffer
;; (defun my/org-refile-within-current-buffer ()
;;   "Move the entry at point to another heading in the current buffer."
;;   (interactive)
;;   (let ((org-refile-targets '((nil :maxlevel . 5))))
;;     (org-refile)))

;; Stop a currently running clock on killing emacs
(defun org-clock-out-maybe ()
     (org-clock-out nil t)
     (org-save-all-org-buffers))
(add-hook 'kill-emacs-hook 'org-clock-out-maybe)

