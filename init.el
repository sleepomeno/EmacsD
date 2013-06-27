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
                      ace-jump-mode)
  "A list of packages to ensure are installed at launch.")

;; Automaticaly install any missing packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

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
(fset 'enter-new-line
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([5 return] 0 "%d")) arg)))
(global-set-key (kbd "C-#") 'enter-new-line)


;; Org
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(setq org-agenda-files (list "~/org/home.org"
                             "~/org/uni.org"))

;; Haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'flymake-haskell-multi-load)
(defun haskell-hook ()
  (define-key evil-normal-state-map (kbd "M-.") 'find-tag))
(add-hook 'haskell-mode-hook 'haskell-hook)

;; Flymake
(global-set-key (kbd "C-c d") 'flymake-display-err-menu-for-current-line)
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
 '(flymake-gui-warnings-enabled nil)
 '(haskell-hoogle-command "")
 '(haskell-package-conf-file "/home/greg/.ghc/x86_64-linux-7.4.1/package.conf")
 '(haskell-process-path-ghci "ghci")
 '(haskell-process-prompt-restart-on-cabal-change nil)
 '(haskell-process-suggest-language-pragmas t)
 '(haskell-program-name "ghci")
 '(haskell-stylish-on-save t)
 '(haskell-tags-on-save t)
 '(haskell-process-type 'cabal-dev)
 '(inferior-haskell-web-docs-base "http://hackage.haskell.org/packages/archive/"))
