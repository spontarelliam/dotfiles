;; -------------------------------
;; Emacs initialization file
;; -------------------------------
;;
(add-to-list 'load-path "~/.emacs.d/")
;(require 'color-theme)

;; Load the 256 color hack for Emacs ver 21
;(load "emacs21-256color-hack.el")

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

;; Disable the toolbar and menubar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

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

(add-to-list 'load-path "~/.emacs.d/switch-window-master")
(require 'switch-window)

;; This forces ediff to split windows vertically. Yes, the nomenclature
;; backwards.
(setq ediff-split-window-function 'split-window-horizontally)
