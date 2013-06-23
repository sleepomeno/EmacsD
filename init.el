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
                      hlinum)
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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-warning-face ((t (:inherit nil :foreground "red" :background nil))))
 '(linum-highlight-face ((t (:inherit default :background "color-238" :foreground "white"))))
 '(show-paren-match ((((class color) (background dark)) (:inherit nil :foreground "red")))))
