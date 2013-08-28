(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)
(server-start)

;; Captures, e.g. for Bookmarks
(require 'org-protocol)
(setq org-protocol-default-template-key "l")
(setq org-capture-templates
 '(("t" "Todo" entry (file+headline "~/org/notes.org" "Tasks")
        "* TODO %?\n  %i\n  %a")
   ("l" "Link" entry (file+olp "~/org/bookmarks.org" "Bookmarks")
        "* %a\n %?\n %i")
   ("j" "Journal" entry (file+datetree "~/org/notes.org")
        "* %?\nEntered on %U\n  %i\n  %a")))

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
                      ;; Project navigation
                      projectile
                      ack-and-a-half
                      ;; Misc.
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

;; Don't fire up another backtrace when an error happens in debug mode
(setq eval-expression-debug-on-error nil)

;; Load the provided Clojure start kit configurations
(load (concat user-emacs-directory "clojure-starter-kit.el"))

;; Evil
(evil-mode 1)

;; Escape insert mode with jj
(defun evil-insert-jj-for-normal-mode ()
  (interactive)
  (insert "j")
  (let ((event (read-event nil)))
    (if (= event ?j)
      (progn
        (backward-delete-char 1)
        (evil-normal-state))
      (push event unread-command-events))))
(define-key evil-insert-state-map "j" 'evil-insert-jj-for-normal-mode)

;; Evil-Leader
(global-evil-leader-mode)
(evil-leader/set-leader ",")

;; Evil-Leader custom configuration
(evil-leader/set-key
  "e" 'find-file
  "b" 'switch-to-buffer
  "o" 'other-window
  "k" 'kill-buffer)

;; Evil-Nerd-Commenter
(evil-leader/set-key
  "ci" 'evilnc-comment-or-uncomment-lines
  "cl" 'evilnc-comment-or-uncomment-to-the-line
  )

;; Markdown
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Nrepl key bindings
(global-set-key (kbd "C-;") 'nrepl-jump-back)
(global-set-key (kbd  "C-:") 'nrepl-jump)


;; Ac-nrepl
(require 'ac-nrepl)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)
(define-key nrepl-interaction-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)
(global-auto-complete-mode t)

;; Global bindings
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "C-c f") 'anything)
(global-set-key (kbd "C-ä") 'delete-other-windows)
(global-set-key (kbd "C-Ä") 'delete-window)
(global-set-key (kbd "<C-S-iso-lefttab>") 'previous-buffer)
(global-set-key (kbd "<C-tab>") 'next-buffer)
(global-set-key (kbd "C-ö") 'keyboard-quit)
(global-set-key (kbd "C-S-u") 'evil-scroll-up)
(global-set-key (kbd "C-S-o") 'other-window)
(global-set-key (kbd "ö") 'other-window)
(global-set-key (kbd "M-l") 'forward-word)
(global-set-key (kbd "M-h") 'backward-word)
(global-set-key (kbd "M-a") 'find-tag)

;; Paredit
(global-set-key (kbd "C-M-h") 'paredit-backward)
(global-set-key (kbd "C-M-l") 'paredit-forward)
(global-set-key (kbd "C-c h") 'paredit-backward-slurp-sexp)
(global-set-key (kbd "C-c j") 'paredit-backward-barf-sexp)
(global-set-key (kbd "C-c k") 'paredit-forward-barf-sexp)
(global-set-key (kbd "C-c l") 'paredit-forward-slurp-sexp)
(global-set-key (kbd "C-M-j") 'paredit-splice-sexp-killing-forward)
(global-set-key (kbd "C-M-k") 'paredit-splice-sexp-killing-backward)
(global-set-key (kbd "C-c C-s") 'paredit-split-sexp)
(global-set-key (kbd "C-c C-j") 'paredit-join-sexps)
(global-set-key (kbd "C-c C-r") 'paredit-raise-sexp)
;; (global-set-key (kbd "C-c s") 'paredit-open-bracket)
(global-set-key (kbd "C-c c") 'paredit-open-curly)
;; (global-set-key (kbd "C-c a") 'paredit-forward-down)
;; (global-set-key (kbd "C-c A") 'paredit-forward-up)
;; (fset 'enter-new-line
;;       (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([5 return] 0 "%d")) arg)))
;; (global-set-key (kbd "C-#") 'enter-new-line)


;; Org
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

   
(setq org-log-done t)

(setq org-agenda-files (list "~/org/home.org"
                             "~/org/uni.org"))
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; Org-drill, Org-learn, Org-annotate-file
(load-file "~/.emacs.d/custom/org-drill.el")
(load-file "~/.emacs.d/custom/org-learn.el")
(load-file "~/.emacs.d/custom/org-annotate-file.el")
(require 'org-annotate-file)
(require 'org-drill)
(require 'org-learn)
(setq org-annotate-file-storage-file "~/org/annotated.org")
(global-set-key (kbd "C-c d") '(lambda () (interactive) (org-annotate-file (current-buffer))))

(setq org-return-follows-link t)

;; Open pdfs mit envince
(delete '("\\.pdf\\'" . default) org-file-apps)
(add-to-list 'org-file-apps '("\\.pdf\\'" . "evince \"%s\""))
(add-to-list 'org-file-apps '("\\.pdf::\\([0-9]+\\)\\'" . "evince \"%s\" -p %1"))

;; Org-Mobile
;; Set to the location of your Org files on your local system
(setq org-directory "~/org")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/org/flagged.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

;; Org-Babel
(add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))

(defvar org-babel-default-header-args:clojure 
  '((:results . "silent")))

(defun org-babel-execute:clojure (body params)
  "Execute a block of Clojure code with Babel."
  (nrepl-interactive-eval body))

(add-hook 'org-src-mode-hook
          '(lambda ()
             (set (make-local-variable 'nrepl-buffer-ns) 
                  (with-current-buffer 
                      (overlay-buffer org-edit-src-overlay)
                    nrepl-buffer-ns))))
(setq org-src-fontify-natively t)
(setq org-confirm-babel-evaluate nil)
(setq org-src-window-setup 'current-window)
(provide 'ob-clojure)
(eval-after-load 'org
       '(add-to-list 'org-structure-template-alist
                    '("c" "#+begin_src clojure :tangle src/\n?\n#+end_src", "<src lang='clojure'>\n?\n</src>")))

(load-file "~/.emacs.d/custom/ob-haskell.el")


;; Haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'flymake-haskell-multi-load)
(defun haskell-hook ()
  (define-key evil-normal-state-map (kbd "M-.") 'find-tag)
  (define-key haskell-mode-map (kbd "C-#") 'haskell-interactive-bring)
  )
(defun haskell-cabal-hook ()
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
  (define-key haskell-cabal-mode-map (kbd "C-#") 'haskell-interactive-bring)
  (define-key haskell-cabal-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch))
(add-hook 'haskell-cabal-mode-hook 'haskell-cabal-hook)
(add-hook 'haskell-mode-hook 'haskell-hook)

;; Flymake
(global-set-key (kbd "C-c e") 'flymake-display-err-menu-for-current-line)
(global-set-key (kbd "C-c C-n") 'flymake-goto-next-error)
(global-set-key (kbd "C-c C-p") 'flymake-goto-next-error)

;; PATH
(setenv "PATH" (concat (getenv "PATH") ":/home/greg/.cabal/bin"))


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
 '(flymake-gui-warnings-enabled nil)
 '(haskell-hoogle-command "")
 '(haskell-package-conf-file "/home/greg/.ghc/x86_64-linux-7.4.1/package.conf")
 '(haskell-process-path-cabal-dev "/usr/bin/cabal-dev")
 '(haskell-process-path-ghci "ghci")
 '(haskell-process-prompt-restart-on-cabal-change nil)
 '(haskell-process-suggest-language-pragmas t)
 '(haskell-process-type (quote cabal-dev))
 '(haskell-program-name "cabal-dev ghci")
 '(haskell-stylish-on-save t)
 '(haskell-tags-on-save t)
 '(inferior-haskell-web-docs-base "http://hackage.haskell.org/packages/archive/")
 '(org-agenda-files (quote ("~/org/bookmarks.org" "~/org/home.org")))
 '(org-drill-optimal-factor-matrix (quote ((1 (2.1799999999999997 . 3.72) (2.5 . 4.0) (2.36 . 3.86) (2.6 . 4.14) (1.7000000000000002 . 3.44)))))
 '(org-modules (quote (org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m org-drill)))
 '(org-todo-keywords (quote ((sequence "TOIMPLEMENT" "IMPLEMENTED") (sequence "TODO" "DONE"))))
 '(safe-local-variable-values (quote ((nrepl-buffer-ns . "darts180.core") (whitespace-line-column . 80) (lexical-binding . t)))))
