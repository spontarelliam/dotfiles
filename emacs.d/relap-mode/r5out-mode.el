;; r5out-mode.el --- Major mode for reading RELAP5 output files.

;; Author: Adam Spontarelli <spontareliam@gmail.com>

;; Usage:
;; 1. load this file
;; 2. M-x r5out-mode

;; C-c C-n next major exit
;; C-c C-p previous major edit

(setq relap-cards 
      `(
	("0MAJOR EDIT.*" . font-lock-builtin-face) ;; output file major edits
        ("0Trip number.*" . font-lock-reference-face) ;; Trip results
        ("[0-9]   Vol.*" . font-lock-type-face) ;; volumes
        ("[0-9]   Jun.*" . font-lock-builtin-face) ;; junctions
        ("str\\.no.*" . font-lock-builtin-face) ;; heat structures
        ("^       .*(.*" . font-lock-builtin-face) ;; units
	))

(setq myKeywords (append relap-cards))

(defun next-error ()
  (interactive)
  (search-forward "0********"))
(global-set-key (kbd "C-c C-e") 'next-error)

(defun next-majout ()
  (interactive)
  (search-forward "0MAJOR EDIT"))
(global-set-key (kbd "C-c C-n") 'next-majout)

(defun prev-majout ()
  (interactive)
  (search-backward "0MAJOR EDIT"))
(global-set-key (kbd "C-c C-p") 'prev-majout)
          
;; define the mode
(define-derived-mode r5out-mode text-mode
  "RELAP"
  "Major mode for RELAP input files"

  ;; code for syntax highlighting
  (setq font-lock-defaults '(myKeywords nil t))

  )

(provide 'r5out-mode)

;; if first line of file matches, activate relap-mode
;; looking for equals line or comment line followed by equals line
(add-to-list 'magic-mode-alist '("^1  NRELAP5/Ver.*" . r5out-mode))

