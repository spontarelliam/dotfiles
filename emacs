(add-to-list 'load-path "~/.emacs.d/")
(require 'color-theme)

;; Load the 256 color hack for Emacs ver 21
(load "emacs21-256color-hack.el")

;; Solarized color theme
(add-to-list 'load-path "~/.emacs.d/colors/emacs-color-theme-solarized-master")
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

;; (add-to-list 'load-path "~/.emacs.d")
;; (require 'i3)
;; (require 'i3-integration)

;; Save all backup files to /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;;SERVER
;; turn on font-lock mode
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

;; Show column number
(setq column-number-mode t)

;;(require 'git-gutter)

;; Set mark region color to yellow
(set-face-background 'region "yellow")

