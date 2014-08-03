;; -------------------------------
;; Emacs initialization file
;; -------------------------------
;;
(add-to-list 'load-path "~/.emacs.d/")


;; Package manager for ver 24
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives 
               '("melpa" . "http://melpa.milkbox.net/packages/"))
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-refresh-contents)
  )


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

;; enable visual feedback on selections
;(setq transient-mark-mode t)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; default to unified diffs
(setq diff-switches "-u")

;; always end a file with a newline
;(setq require-final-newline 'query)

;; Enable colors
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Enables text highlighting
(transient-mark-mode 1)

;; Associate Fortran mode with *.s files
(setq auto-mode-alist (cons '("\\.s$" . fortran-mode) auto-mode-alist))

;; Associate C-mode with *.ino files
(setq auto-mode-alist (cons '("\\.ino$" . c-mode) auto-mode-alist))

;; Show column number
(setq column-number-mode t)

;; Set mark region color to yellow
;;(set-face-background 'region "yellow")

;; automatically save eshell history and stop annoying me
(load "em-hist") ; So the history vars are defined
(if (boundp 'eshell-save-history-on-exit)
    (setq eshell-save-history-on-exit t)) ; Don't ask, just save
;(message "eshell-ask-to-save-history is %s" eshell-ask-to-save-history)
(if (boundp 'eshell-ask-to-save-history)
    (setq eshell-ask-to-save-history 'always)) ; For older(?) version
;(message "eshell-ask-to-save-history is %s" eshell-ask-to-save-history)

(add-to-list 'load-path "~/.emacs.d/magit-1.2.0")
(require 'magit)

;; Flyspell
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(autoload 'flyspell-delay-command "flyspell" "Delay on command." t) 
(autoload 'tex-mode-flyspell-verify "flyspell" "" t) 
;; Enable flyspell automatically in Latex mode
(add-hook 'latex-mode-hook 'flyspell-mode)
(add-hook 'tex-mode-hook 'flyspell-mode)

;; Erc Nick Colors (version 24)
(if (>= (string-to-number emacs-version) 24)                        ; this is the test, the "if"
    (require 'erc-hl-nicks)
  (ding)                                              ; From here on is the "else"
    (message "Time to upgrade, don't you think?"))


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

(add-to-list 'load-path "~/.emacs.d/switch-window-master")
(require 'switch-window)

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
;; (setenv "PYMACS_PYTHON" "python2")
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

;; load babel supported languages
(org-babel-do-load-languages
 'org-babel-load-languages
  '((python . t)
     (emacs-lisp . t)))

;; git
;; (global-git-gutter-mode t)


;; W3M web browser
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
 ;; optional keyboard short-cut
(global-set-key "\C-xm" 'browse-url-at-point)

;;cookies
(setq w3m-use-cookies t)

;; Undo
(global-undo-tree-mode)


