(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(add-to-list 'load-path "/home/greg/.emacs.d/custom/org-mode/lisp")
(require 'org)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(starter-kit
                      starter-kit-bindings
                      starter-kit-js
                      starter-kit-lisp
                      ;; Clojure & friends
                      cider
                      rainbow-delimiters
                      ac-nrepl
                      paredit
                      ;; Project navigation
                      projectile
                      ack-and-a-half
                      ido-better-flex
                      ;; Misc.
                      markdown-mode
                      twilight-theme
                      evil
                      evil-matchit
                      evil-leader
                      evil-numbers
                      evil-paredit
                      evil-nerd-commenter
                      flymake-hlint
                      haskell-mode
                      flymake-haskell-multi
                      hlinum
                      auto-complete
                      ace-jump-mode)
  "A list of packages to ensure are installed at launch.")

;; Automaticaly install any missing packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(load-file "~/.emacs.d/custom/org-dotemacs/org-dotemacs.el")
(setq org-dotemacs-default-file (concat user-emacs-directory "configuration.org"))
(org-dotemacs-load-default)

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
 '(inferior-haskell-web-docs-base "http://hackage.haskell.org/packages/archive/" t)
 '(org-drill-learn-fraction 0.45)
 '(org-drill-optimal-factor-matrix (quote ((5 (1.52 . 2.028)) (4 (1.52 . 2.083) (1.7600000000000002 . 2.237) (1.8000000000000003 . 2.272) (1.6600000000000001 . 2.15) (2.7 . 2.682) (2.8000000000000003 . 2.766) (2.32 . 2.383) (1.9999999999999998 . 2.174)) (3 (1.48 . 1.993) (2.3200000000000003 . 2.29) (1.9400000000000002 . 2.014) (2.56 . 2.576) (2.1399999999999997 . 2.305) (2.36 . 2.36) (1.8000000000000003 . 2.254) (2.6 . 2.579) (2.2800000000000002 . 2.436) (2.7 . 2.66) (2.32 . 2.383) (2.1799999999999997 . 2.343) (2.04 . 2.211) (2.46 . 2.46)) (2 (2.08 . 2.08) (1.8 . 1.879) (2.04 . 2.268) (1.8199999999999998 . 2.192) (2.06 . 2.335) (2.22 . 2.22) (1.8000000000000003 . 2.254) (1.96 . 2.264) (2.2800000000000002 . 2.249) (2.7 . 2.66) (2.2199999999999998 . 2.336) (2.1799999999999997 . 2.343) (1.56 . 2.116) (2.5 . 2.5) (1.7000000000000002 . 2.185) (2.6 . 2.579) (2.36 . 2.421) (2.46 . 2.498)) (1 (2.2800000000000002 . 3.866) (2.46 . 3.996) (1.8199999999999998 . 3.508) (1.96 . 3.622) (2.2199999999999998 . 3.752) (2.04 . 3.63) (2.7 . 4.256) (2.1799999999999997 . 3.874) (2.5 . 4.0) (2.36 . 3.874) (2.6 . 4.126) (1.7000000000000002 . 3.496)))))
 '(org-latex-with-hyperref nil)
 '(safe-local-variable-values (quote ((org-latex-with-hyperref) (eval setq org-latex-with-hyperref nil) (eval org-latex-with-hyperref nil) (whitespace-line-column . 80) (lexical-binding . t)))))
