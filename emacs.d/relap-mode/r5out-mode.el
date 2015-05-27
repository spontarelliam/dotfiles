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


(defun Vol1 ()
  "Provide popup tips describing RELAP output columns."
  (interactive)
  (cond
   ((and (<= 0 (current-column)) (>= 11 (current-column))) (popup-tip "Vol.No."))
   ((and (< 11 (current-column)) (>= 20 (current-column))) (popup-tip "Pressure"))
   ((and (< 20 (current-column)) (>= 33 (current-column))) (popup-tip "Voidg"))
   ((and (< 33 (current-column)) (>= 46 (current-column))) (popup-tip "Tempf"))
   ((and (< 46 (current-column)) (>= 59 (current-column))) (popup-tip "Tempg"))
   ((and (< 59 (current-column)) (>= 72 (current-column))) (popup-tip "Sat Temp"))
   ((and (< 72 (current-column)) (>= 85 (current-column))) (popup-tip "Noncond Vapor Qual"))
   ((and (< 85 (current-column)) (>= 98 (current-column))) (popup-tip "Boron Dens"))
   ((and (< 98 (current-column)) (>= 111 (current-column))) (popup-tip "UF"))
   ((and (< 111 (current-column)) (>= 124 (current-column))) (popup-tip "UG"))
   ((and (< 124 (current-column)) (>= 133 (current-column))) (popup-tip "Vol Flag"))
   ))

(defun Vol2 ()
  "Provide popup tips describing RELAP output columns."
  (interactive)
  (cond
   ((and (<= 0 (current-column)) (>= 11 (current-column))) (popup-tip "Vol.No."))
   ((and (< 11 (current-column)) (>= 20 (current-column))) (popup-tip "rhof"))
   ((and (< 20 (current-column)) (>= 33 (current-column))) (popup-tip "velf"))
   ((and (< 33 (current-column)) (>= 46 (current-column))) (popup-tip "velg"))
   ((and (< 46 (current-column)) (>= 59 (current-column))) (popup-tip "sounde"))
   ((and (< 59 (current-column)) (>= 72 (current-column))) (popup-tip "equil"))
   ((and (< 72 (current-column)) (>= 85 (current-column))) (popup-tip "qual"))
   ((and (< 85 (current-column)) (>= 98 (current-column))) (popup-tip "tot HT inp"))
   ((and (< 98 (current-column)) (>= 111 (current-column))) (popup-tip "vap HT inp"))
   ((and (< 111 (current-column)) (>= 124 (current-column))) (popup-tip "vapor gen"))
   ))

(defun Vol3 ()
  "Provide popup tips describing RELAP output columns."
  (interactive)
  (cond
   ((and (<= 0 (current-column)) (>= 11 (current-column))) (popup-tip "Vol.No."))
   ((and (< 11 (current-column)) (>= 20 (current-column))) (popup-tip "lrgst mass err"))
   ((and (< 20 (current-column)) (>= 33 (current-column))) (popup-tip "reduce-quality"))
   ((and (< 33 (current-column)) (>= 46 (current-column))) (popup-tip "reduce-extrap"))
   ((and (< 46 (current-column)) (>= 59 (current-column))) (popup-tip "reduce-mass"))
   ((and (< 59 (current-column)) (>= 72 (current-column))) (popup-tip "reduce-propty"))
   ((and (< 72 (current-column)) (>= 85 (current-column))) (popup-tip "min.courant"))
   ((and (< 85 (current-column)) (>= 98 (current-column))) (popup-tip "reduce-courant"))
   ))

(defun Jun1 ()
  "Provide popup tips describing RELAP output columns."
  (interactive)
  (cond
   ((and (<= 0 (current-column)) (>= 11 (current-column))) (popup-tip "Jun.No."))
   ((and (< 11 (current-column)) (>= 20 (current-column))) (popup-tip "From Vol"))
   ((and (< 20 (current-column)) (>= 33 (current-column))) (popup-tip "To Vol"))
   ((and (< 33 (current-column)) (>= 46 (current-column))) (popup-tip "velfj"))
   ((and (< 46 (current-column)) (>= 59 (current-column))) (popup-tip "velgj"))
   ((and (< 59 (current-column)) (>= 72 (current-column))) (popup-tip "mass flow"))
   ((and (< 72 (current-column)) (>= 85 (current-column))) (popup-tip "jun. area"))
   ((and (< 85 (current-column)) (>= 98 (current-column))) (popup-tip "throat ratio"))
   ((and (< 98 (current-column)) (>= 111 (current-column))) (popup-tip "junction flags"))
   ((and (< 111 (current-column)) (>= 117 (current-column))) (popup-tip "choke flag"))
   ((and (< 117 (current-column)) (>= 126 (current-column))) (popup-tip "no. adv edits"))
   ((and (< 126 (current-column)) (>= 132 (current-column))) (popup-tip "choked total"))
   ))

(defun Jun2 ()
  "Provide popup tips describing RELAP output columns."
  (interactive)
  (cond
   ((and (<= 0 (current-column)) (>= 11 (current-column))) (popup-tip "Jun.No."))
   ((and (< 11 (current-column)) (>= 20 (current-column))) (popup-tip "voidfj"))
   ((and (< 20 (current-column)) (>= 33 (current-column))) (popup-tip "voidgj"))
   ((and (< 33 (current-column)) (>= 46 (current-column))) (popup-tip "from"))
   ((and (< 46 (current-column)) (>= 59 (current-column))) (popup-tip "to"))
   ((and (< 59 (current-column)) (>= 72 (current-column))) (popup-tip "fij"))
   ((and (< 72 (current-column)) (>= 85 (current-column))) (popup-tip "fwalfj"))
   ((and (< 85 (current-column)) (>= 98 (current-column))) (popup-tip "fwalgj"))
   ((and (< 98 (current-column)) (>= 111 (current-column))) (popup-tip "fjunf"))
   ((and (< 111 (current-column)) (>= 117 (current-column))) (popup-tip "fjunr"))
   ((and (< 117 (current-column)) (>= 126 (current-column))) (popup-tip "formfj"))
   ((and (< 126 (current-column)) (>= 132 (current-column))) (popup-tip "formgj"))
   ))

(defun Str1 ()
  "Provide popup tips describing RELAP output columns."
  (interactive)
  (cond
   ((and (<= 0 (current-column)) (>= 11 (current-column))) (popup-tip "Str.No."))
   ((and (< 11 (current-column)) (>= 20 (current-column))) (popup-tip "side"))
   ((and (< 20 (current-column)) (>= 33 (current-column))) (popup-tip "bdry vol"))
   ((and (< 33 (current-column)) (>= 46 (current-column))) (popup-tip "surface temp"))
   ((and (< 46 (current-column)) (>= 59 (current-column))) (popup-tip "heat tr rate"))
   ((and (< 59 (current-column)) (>= 72 (current-column))) (popup-tip "heat flux"))
   ((and (< 72 (current-column)) (>= 85 (current-column))) (popup-tip "crit heat flux"))
   ((and (< 85 (current-column)) (>= 98 (current-column))) (popup-tip "mode"))
   ((and (< 98 (current-column)) (>= 111 (current-column))) (popup-tip "heat tr coef"))
   ((and (< 111 (current-column)) (>= 117 (current-column))) (popup-tip "int heat source"))
   ((and (< 117 (current-column)) (>= 126 (current-column))) (popup-tip "net heat loss"))
   ((and (< 126 (current-column)) (>= 132 (current-column))) (popup-tip "vol ave temp"))
   ))

(defun define-value ()
  (interactive)
  (save-excursion (search-backward-regexp "^0\s*.*\\.NO\\..*"))
  (if (string-match "PRESSURE" (match-string 0 nil))
      (Vol1)())
  (if (string-match "RHOF" (match-string 0 nil))
      (Vol2)())
  (if (string-match "FROM" (match-string 0 nil))
      (Vol3)())
  (if (string-match "FROM" (match-string 0 nil))
      (Jun1)())
  (if (string-match "VOIDFJ" (match-string 0 nil))
      (Jun2)())
  (if (string-match "BDRY" (match-string 0 nil))
      (Str1)())
  )
(global-set-key (kbd "C-c C-d") 'define-value)

(defun get-units ()
  (interactive)
  (save-excursion (search-backward-regexp "^\s*\(.*"))
  (popup-tip (match-string 0 nil))
  )
(global-set-key (kbd "C-c C-u") 'get-units)

(defun time-step-info ()
  (interactive)
  (if (search-backward-regexp "^ *[0-9]+ *0*20[0-9] *[0-9]+\\." p1)
      (message "found")
      (message "not found"))
  )
(global-set-key (kbd "C-c C-t") 'time-step-info)
          
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

