(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

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
                      guide-key
                      markdown-mode
                      graphviz-dot-mode
                      twilight-theme
                      auto-complete
                      ace-jump-mode
                      drag-stuff
                      jabber
                      js-comint
                      js2-mode
                      js2-refactor
                      skewer-mode
                      twittering-mode
                      ;; Evil
                      evil
                      evil-matchit
                      evil-leader
                      evil-numbers
                      evil-paredit
                      evil-jumper
                      evil-nerd-commenter
                      flx-ido
                      flymake flymake-cursor
                      flymake-hlint
                      ;; haskell-mode
                      flymake-haskell-multi
                      hlinum)
  "A list of packages to ensure are installed at launch.")

;; Automaticaly install any missing packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(load-file "~/.emacs.d/custom/org-dotemacs/org-dotemacs.el")
;;(setq org-dotemacs-error-handling nil)
(setq org-dotemacs-default-file (concat user-emacs-directory "configuration.org"))
(setq org-dotemacs-tag-match "-skip")
(org-dotemacs-load-default )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(font-lock-warning-face ((t (:inherit nil :foreground "red" :background nil))))
 '(linum-highlight-face ((t (:inherit default :background "color-238" :foreground "white"))) t)
 '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))))
 '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon" :strike-through t))))
 '(show-paren-match ((((class color) (background dark)) (:inherit nil :foreground "red")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("6cfe5b2f818c7b52723f3e121d1157cf9d95ed8923dbc1b47f392da80ef7495d" "9a9e75c15d4017c81a2fe7f83af304ff52acfadd7dde3cb57595919ef2e8d736" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" "9f443833deb3412a34d2d2c912247349d4bd1b09e0f5eaba11a3ea7872892000" default)))
 '(inferior-haskell-web-docs-base "http://hackage.haskell.org/packages/archive/")
 '(mu4e-headers-date-format "%d-%m-%y")
 '(org-agenda-files (quote ("~/org/home.org")))
 '(org-drill-learn-fraction 0.45)
 '(org-drill-optimal-factor-matrix (quote ((5 (1.52 . 2.028)) (4 (2.1399999999999997 . 2.305) (2.4799999999999995 . 2.624) (2.38 . 2.513) (1.52 . 2.083) (1.7600000000000002 . 2.237) (1.8000000000000003 . 2.272) (1.6600000000000001 . 2.15) (2.7 . 2.682) (2.8000000000000003 . 2.766) (2.32 . 2.383) (1.9999999999999998 . 2.174)) (3 (3.0 . 2.991) (3.1 . 3.094) (2.66 . 2.657) (2.6599999999999997 . 2.712) (2.34 . 2.492) (2.8 . 2.8) (2.8000000000000003 . 2.744) (1.48 . 1.993) (2.3200000000000003 . 2.29) (1.9400000000000002 . 2.014) (2.56 . 2.576) (2.1399999999999997 . 2.305) (2.36 . 2.36) (1.8000000000000003 . 2.254) (2.6 . 2.579) (2.2800000000000002 . 2.436) (2.7 . 2.66) (2.32 . 2.383) (2.1799999999999997 . 2.343) (2.04 . 2.211) (2.46 . 2.434)) (2 (2.34 . 2.475) (2.9 . 2.888) (3.0 . 2.991) (2.56 . 2.577) (2.24 . 2.399) (2.66 . 2.641) (2.8000000000000003 . 2.744) (2.32 . 2.419) (2.08 . 2.08) (1.8 . 1.879) (2.04 . 2.268) (1.8199999999999998 . 2.192) (2.06 . 2.335) (2.22 . 2.22) (1.8000000000000003 . 2.254) (1.96 . 2.264) (2.2800000000000002 . 2.417) (2.7 . 2.66) (2.2199999999999998 . 2.336) (2.1799999999999997 . 2.343) (1.56 . 2.116) (2.5 . 2.5) (1.7000000000000002 . 2.185) (2.6 . 2.579) (2.36 . 2.421) (2.46 . 2.498)) (1 (2.66 . 4.252) (2.8000000000000003 . 4.39) (2.9 . 4.126) (1.56 . 3.386) (2.56 . 4.122) (2.2800000000000002 . 3.866) (2.46 . 3.996) (1.8199999999999998 . 3.508) (1.96 . 3.622) (2.2199999999999998 . 3.752) (2.04 . 3.63) (2.7 . 4.256) (2.1799999999999997 . 3.748) (2.5 . 4.0) (2.36 . 3.874) (2.6 . 4.126) (1.7000000000000002 . 3.496)))))
 '(org-export-backends (quote (ascii html icalendar latex md octopress)))
 '(org-latex-with-hyperref nil)
 '(safe-local-variable-values (quote ((require-final-newline) (reftex-plug-into-AUCTeX . t) (TeX-auto-save . t) (TeX-parse-self . t) (TeX-debug-bad-boxes . t) (zotero-collection . #("12" 0 2 (name "Diplomarbeit"))) (haskell-program-name . "ghci") (haskell-program-name . ghci) (haskell-process-args-ghci) (org-latex-with-hyperref) (eval setq org-latex-with-hyperref nil) (eval org-latex-with-hyperref nil) (whitespace-line-column . 80) (lexical-binding . t)))))
