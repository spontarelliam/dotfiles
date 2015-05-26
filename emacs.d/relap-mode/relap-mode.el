;; relap-mode.el --- Major mode for editing RELAP5 input files.

;; Author: Adam Spontarelli <spontareliam@gmail.com>

;; Usage:
;; 1. load this file
;; 2. M-x relap-mode

;; C-c C-l to count lines longer than 80 characters

(setq non-condensables '("argon" "helium" "hydrogen" "nitrogen" "xenon" "krypton" "air" "oxygen" "h2o" "h2oold" "d2o" "water"))

(setq specials '("new" "restart" "reset" "reedit" "strip" "cmpcoms" "stdy-st" "transnt" "binary" "fmtout" "pwrlb" "pwrsb" "pwrnl" "bwrnl" "pwrlbrv2" "none" "run" "inp-chk" "british" "si"))

;; controls
(setq controls '("sum" "mult" "div" "differeni" "diffrend" "integral" "function" "stdfnctn" "delay" "tripunit" "tripdlay" "poweri" "powerr" "powerx" "prop-int" "lag" "lead-lag" "shaft" "constant" "expanded" "min" "max"))

;; attributes
(setq attributes '("time" "timeof" "cputime" "null" "tmass" "emass" "dt" "dtcrnt" "pmpvel" "pmphead" "pmptrq" "vlvarea" "vlvstem" "turpow" "turtrq" "turvel" "tureff" "rho" "rhof" "rhog" "uf" "ug" "voidf" "voidg" "velf" "velg" "p" "quals" "quala" "quale" "q" "qwg" "tempf" "tempg" "sattemp" "satpps" "sounde" "vapgen" "boron" "floreg" "mfluxf" "mfluxg" "velfj" "velgj" "voidgj" "qualaj" "rhofj" "rhogj" "hfj" "hgj" "mflowj" "ajun" "mnfluxfj" "lvol" "htmode" "rktpow" "rkreac" "tclada" "htvat" "tfuela" "httemp" "httmps" "cntrlvar" "acvliq" "volmer" "hif" "tsatt" "htrnr" "sysmer" "sysmerf" "sysmerg" "sysmerb" "errmax" "flenth" "volmas"))

;; components
(setq components '("snglvol" "tmdpvol" "sngljun" "tmdpjun" "pipe" "annulus" "prizer" "canchan" "branch" "separatr" "jetmixer" "turbine" "eccmix" "valve" "pump" "mtpjun" "accum" "delete" "chkvlv" "trpvlv" "inrvlv" "mtrvlv" "srvvlv" "rlfvlv" "posvlv" "twodee" "multjun" "twodee-a" "twodee-b" "snglfw"))

;; trips
(setq trips '("gt" "lt" "l" "n" "and" "or" "null" "ge" "le"))

;; tables
(setq tables '("power" "htrnrate" "hct-t" "htc-temp" "temp" "reac-t" "normarea"))

(setq specials-regexp (regexp-opt specials 'words))
(setq components-regexp (regexp-opt components 'words))
(setq controls-regexp (regexp-opt controls 'words))
(setq trips-regexp (regexp-opt trips 'words))
(setq tables-regexp (regexp-opt tables 'words))
(setq attributes-regexp (regexp-opt attributes 'words))
(setq non-condensables-regexp (regexp-opt non-condensables 'words))

(setq relap-keywords
      `(
        (,components-regexp . font-lock-builtin-face)
        (,controls-regexp . font-lock-function-name-face)
        (,trips-regexp . font-lock-negation-char-face)
        (,tables-regexp . font-lock-builtin-face)
        (,attributes-regexp . font-lock-type-face)
        (,specials-regexp . font-lock-builtin-face)
        (,non-condensables-regexp . font-lock-type-face)
        ))

(setq relap-cards 
      `(
        ("^[^\\*
]\\{81,\\}" . font-lock-warning-face) ;; >80 char lines
	("^ *205[0-9]\\{5\\}" . font-lock-builtin-face) ;; Control variables
	("^ *206[0-9]\\{5\\} " . font-lock-preprocessor-face) ;; Trips expanded
        ("^ *[0]*[4-7][0-9]\\{2\\} " . font-lock-preprocessor-face) ;; Trips
        ("^ *300[0-9]\\{5\\} " . font-lock-function-name-face) ;; Reactivity
	("^ *[0-9]\\{5,7\\} " . font-lock-constant-face) ;; Components
        ("^ *[0-9]\\{3\\} " . font-lock-constant-face) ;; time step
	("^ *1[0-9]\\{7\\} " . font-lock-type-face) ;; Heat Structures
        ("^ *201[0-9]\\{5\\} " . font-lock-type-face) ;; HS thermal properties
        ("^ *202[0-9]\\{5\\} " . font-lock-reference-face) ;; General Table
        ("^ *3[0-9]\\{2\\} " . font-lock-builtin-face) ;; Minor Edits
        ("^ *208[0-9]\\{5\\} " . font-lock-builtin-face) ;; Extended Minor Edits
	("\\*.*" . font-lock-comment-face) ;; comments
        ("^=.*" . font-lock-function-name-face) ;; title card
        ("^\\..*" . font-lock-warning-face) ;; end card)
	))

(setq myKeywords (append relap-cards relap-keywords))

;; command to comment/uncomment text
(defun relap-comment-dwim (arg)
  "Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'."
  (interactive "*P")
  (require 'newcomment)
  (let (
        (comment-start "*") (comment-end "")
        )
    (comment-dwim arg)))

(defun count-bad-lines ()
  (interactive)
  (message "%d lines longer than 80 characters." (how-many "^[^\\*
]\\{81,\\}" 0 (point-max))))
(global-set-key (kbd "C-c C-l") 'count-bad-lines)
          
;; define the mode
(define-derived-mode relap-mode text-mode
  "RELAP"
  "Major mode for RELAP input files"

  ;; code for syntax highlighting
  (setq font-lock-defaults '(myKeywords nil t))

  ;; modify the keymap
  (define-key relap-mode-map [remap comment-dwim] 'relap-comment-dwim)

  ;; clear memory
  (setq components-regexp nil)
  (setq trips-regexp nil)
  (setq tables-regexp nil)
  (setq attributes-regexp nil)
  (setq specials-regexp nil)
  (setq controls-regexp nil)
  (setq non-condensables-regexp nil)
  )

(provide 'relap-mode)
;; (define-minor-mode foo-mode
;;   (provide 'relap-mode))
  
;; if first line of file matches, activate relap-mode
;; looking for equals line or comment line followed by equals line
(add-to-list 'magic-mode-alist '("^=.*" . relap-mode))
(add-to-list 'magic-mode-alist '("^\\*.*\n=.*" . relap-mode))


