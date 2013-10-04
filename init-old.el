
;; Nrepl key bindings


;; Ac-nrepl (Auto-completion for nrepl)



;; Global bindings

;; Org


;; Captures, e.g. for Bookmarks







;; Org-Mobile
;; Set to the location of your Org files on your local system
;; Set to the name of the file where new notes will be stored

;; Org-Babel




;; Haskell

;; Flymake


;; Sometimes Org agenda just doesnt work

;; Org-attach should provide a link functionality




(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-warning-face ((t (:inherit nil :foreground "red" :background nil))))
 '(linum-highlight-face ((t (:inherit default :background "color-238" :foreground "white"))))
 '(show-paren-match ((((class color) (background dark)) (:inherit nil :foreground "red")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("9f443833deb3412a34d2d2c912247349d4bd1b09e0f5eaba11a3ea7872892000" default)))
 '(org-drill-optimal-factor-matrix (quote ((3 (1.8000000000000003 . 2.254) (2.6 . 2.579) (2.2800000000000002 . 2.436) (2.7 . 2.66) (2.32 . 2.383) (2.1799999999999997 . 2.343) (2.04 . 2.211) (2.46 . 2.46)) (2 (2.04 . 2.268) (1.8199999999999998 . 2.192) (2.06 . 2.335) (2.22 . 2.22) (1.8000000000000003 . 2.254) (1.96 . 2.264) (2.2800000000000002 . 2.417) (2.7 . 2.66) (2.2199999999999998 . 2.336) (2.1799999999999997 . 2.343) (1.56 . 2.116) (2.5 . 2.5) (1.7000000000000002 . 2.185) (2.6 . 2.579) (2.36 . 2.421) (2.46 . 2.498)) (1 (2.2800000000000002 . 3.866) (2.46 . 3.996) (1.8199999999999998 . 3.508) (1.96 . 3.622) (2.2199999999999998 . 3.752) (2.04 . 3.63) (2.7 . 4.256) (2.1799999999999997 . 3.748) (2.5 . 4.0) (2.36 . 3.874) (2.6 . 4.126) (1.7000000000000002 . 3.496)))))
 '(org-modules (quote (org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m org-drill)))
 '(safe-local-variable-values (quote ((nrepl-buffer-ns . "darts180.core") (whitespace-line-column . 80) (lexical-binding . t)))))
