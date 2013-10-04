(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(starter-kit
                      starter-kit-bindings
                      starter-kit-js
                      starter-kit-lisp
                      ;; Clojure & friends
                      clojure-mode
                      nrepl
                      rainbow-delimiters
                      paredit
                      ;; Project navigation
                      projectile
                      ack-and-a-half
                      ido-better-flex
                      ;; Misc.
                      magit
                      markdown-mode
                      twilight-theme
                      evil
                      evil-leader
                      evil-numbers
                      evil-paredit
                      evil-nerd-commenter
                      flymake-hlint
                      haskell-mode
                      flymake-haskell-multi
                      hlinum
                      auto-complete
                      ac-nrepl
                      ace-jump-mode)
  "A list of packages to ensure are installed at launch.")

;; Automaticaly install any missing packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; (load-file "~/.emacs.d/elpa/org-dotemacs-0.2/org-dotemacs.el")
(setq org-dotemacs-default-file (concat user-emacs-directory "configuration.org"))
(org-dotemacs-load-file)

(custom-set-faces
	;; custom-set-faces was added by Custom.
	;; If you edit it by hand, you could mess it up, so be careful.
	;; Your init file should contain only one such instance.
	;; If there is more than one, they won't work right.
	'(font-lock-warning-face ((t (:inherit nil :foreground "red" :background nil))))
	'(linum-highlight-face ((t (:inherit default :background "color-238" :foreground "white"))))
	'(show-paren-match ((((class color) (background dark)) (:inherit nil :foreground "red"))))))
(custom-set-variables
	;; custom-set-variables was added by Custom.
	;; If you edit it by hand, you could mess it up, so be careful.
	;; Your init file should contain only one such instance.
	;; If there is more than one, they won't work right.
	'(custom-safe-themes (quote ("9f443833deb3412a34d2d2c912247349d4bd1b09e0f5eaba11a3ea7872892000" default)))
	;; '(flymake-gui-warnings-enabled nil)
	;; '(haskell-hoogle-command "")
	;; '(haskell-package-conf-file "/home/greg/.ghc/x86_64-linux-7.4.1/package.conf")
	;; '(haskell-process-path-cabal-dev "/usr/bin/cabal-dev")
	;; '(haskell-process-path-ghci "ghci")
	;; '(haskell-process-prompt-restart-on-cabal-change nil)
	;; '(haskell-process-suggest-language-pragmas t)
	;; '(haskell-process-type (quote cabal-dev))
	;; '(haskell-program-name "cabal-dev ghci")
	;; '(haskell-stylish-on-save t)
	'(haskell-tags-on-save t)
	'(inferior-haskell-web-docs-base "http://hackage.haskell.org/packages/archive/")
	;; '(initial-buffer-choice "~/org/home.org")
	;; '(org-M-RET-may-split-line (quote ((default))))
	;; '(org-drill-learn-fraction 0.45)
	'(org-drill-optimal-factor-matrix (quote ((3 (1.8000000000000003 . 2.254) (2.6 . 2.579) (2.2800000000000002 . 2.436) (2.7 . 2.66) (2.32 . 2.383) (2.1799999999999997 . 2.343) (2.04 . 2.211) (2.46 . 2.46)) (2 (2.04 . 2.268) (1.8199999999999998 . 2.192) (2.06 . 2.335) (2.22 . 2.22) (1.8000000000000003 . 2.254) (1.96 . 2.264) (2.2800000000000002 . 2.417) (2.7 . 2.66) (2.2199999999999998 . 2.336) (2.1799999999999997 . 2.343) (1.56 . 2.116) (2.5 . 2.5) (1.7000000000000002 . 2.185) (2.6 . 2.579) (2.36 . 2.421) (2.46 . 2.498)) (1 (2.2800000000000002 . 3.866) (2.46 . 3.996) (1.8199999999999998 . 3.508) (1.96 . 3.622) (2.2199999999999998 . 3.752) (2.04 . 3.63) (2.7 . 4.256) (2.1799999999999997 . 3.748) (2.5 . 4.0) (2.36 . 3.874) (2.6 . 4.126) (1.7000000000000002 . 3.496)))))
	'(safe-local-variable-values (quote ((nrepl-buffer-ns . "darts180.core") (whitespace-line-column . 80) (lexical-binding . t)))))
