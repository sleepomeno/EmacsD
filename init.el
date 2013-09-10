(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)
(server-start)


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

;; Always indent on newline
(global-set-key (kbd "RET") 'newline-and-indent)

;; Don't check spelling in every text-mode buffer
(remove-hook 'text-mode-hook 'turn-on-flyspell)

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
  "o" 'org-iswitchb
  "w" 'save-buffer
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

;; Org-caldav-sync bindings
(define-key evil-normal-state-map (kbd "C-p") 'org-caldav-sync)
;; Input settings (M-x insert-kbd-macro)
(global-set-key (kbd "C-ü")
   '(lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217848 111 114 103 45 99 97 108 100 97 118 45 115 121 110 99 return 111 114 46 114 105 101 103 108 101 114 64 103 109 97 105 108 46 99 111 109 return 119 97 97 114 115 110 118 116 102 120 102 120 121 112 118 106 return] 0 "%d")) arg)))

;; Ac-nrepl (Auto-completion for nrepl)
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
(global-set-key (kbd "M-o") 'imenu)


;; Paredit
(global-set-key (kbd "C-M-h") 'paredit-backward)
(global-set-key (kbd "C-M-l") 'paredit-forward)
(global-set-key (kbd "C-c h") 'paredit-backward-slurp-sexp)
(global-set-key (kbd "C-c j") 'paredit-backward-barf-sexp)
(global-set-key (kbd "C-c k") 'paredit-forward-barf-sexp)
(global-set-key (kbd "C-c l") 'paredit-forward-slurp-sexp)
(global-set-key (kbd "C-M-j") 'paredit-splice-sexp-killing-forward)
(global-set-key (kbd "C-M-k") 'paredit-splice-sexp-killing-backward)
(global-set-key (kbd "C-c C-s") 'paredit-split-sexp )
(global-set-key (kbd "C-c C-j") 'paredit-join-sexps)
(global-set-key (kbd "C-c C-r") 'paredit-raise-sexp)
(global-set-key (kbd "C-c c") 'paredit-open-curly)
(global-set-key (kbd "C-s-l j") 'paredit-forward-down)
(global-set-key (kbd "C-s-l k") 'paredit-forward-up)
(global-set-key (kbd "C-s-h j") 'paredit-backward-down)
(global-set-key (kbd "C-s-h k") 'paredit-backward-up)
(global-set-key (kbd "C-c (") 'paredit-wrap-round)
(global-set-key (kbd "C-c {") 'paredit-wrap-curly)
(global-set-key (kbd "C-c [") 'paredit-wrap-square)

;; Org

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

(load-file "~/.emacs.d/custom/org-caldav.el")
(load-file "~/.emacs.d/custom/org-import-calendar.el")
(load-file "~/.emacs.d/custom/org-drill.el")
(load-file "~/.emacs.d/custom/org-learn.el")
(load-file "~/.emacs.d/custom/org-annotate-file.el")

(require 'org-caldav)
(require 'org-annotate-file)
(require 'org-drill)
(require 'org-learn)

(setq org-goto-interface 'outline org-goto-max-level 10)

(setq org-caldav-calendar-id "vpvsjgj9avredjnv58kt85lklo@group.calendar.google.com")
(setq org-icalendar-timezone "UTC")
(setq org-caldav-inbox "~/org/cal.org")
(setq org-caldav-files (list "~/org/home.org" "~/org/uni.org"))
(setq org-caldav-sync-changes-to-org 'title-only)

;; Not needed for the time being
;; (require 'org-import-icalendar)
;; (setq org-import-icalendar-filename "~/org/cal.org")

(setq org-icalendar-include-todo nil)
(setq org-icalendar-store-UID t)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cL" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
   
(setq org-log-done 'time)

(setq org-agenda-files (list "~/org/test.org" "~/org/home.org"
                             "~/org/uni.org"))
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; Org-drill, Org-learn, Org-annotate-file
(setq org-annotate-file-storage-file "~/org/annotated.org")
;; (global-set-key (kbd "C-c d") '(lambda () (interactive) (org-annotate-file (current-buffer))))

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

(defun save-macro (name)                  
  "save a macro. Take a name as argument
     and save the last defined macro under 
     this name at the end of your .emacs"
  (interactive "SName of the macro :")  ; ask for the name of the macro    
  (kmacro-name-last-macro name)         ; use this name for the macro    
  (find-file user-init-file)            ; open ~/.emacs or other user init file 
  (goto-char (point-max))               ; go to the end of the .emacs
  (newline)                             ; insert a newline
  (insert-kbd-macro name)               ; copy the macro 
  (newline)                             ; insert a newline
  (switch-to-buffer nil))               ; return to the initial buffer

;; refile only within the current buffer
(defun my/org-refile-within-current-buffer ()
  "Move the entry at point to another heading in the current buffer."
  (interactive)
  (let ((org-refile-targets '((nil :maxlevel . 5))))
    (org-refile)))

(global-set-key (kbd "C-c C-S-w") 'my/org-refile-within-current-buffer)




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
 '(org-M-RET-may-split-line (quote ((default))))
 '(org-agenda-files (quote ("~/org/projects.org" "~/org/bookmarks.org" "~/org/home.org")))
 '(org-drill-learn-fraction 0.45)
 '(org-drill-optimal-factor-matrix (quote ((2 (1.8000000000000003 . 2.254) (1.96 . 2.264) (2.2800000000000002 . 2.417) (2.7 . 2.66) (2.2199999999999998 . 2.336) (2.1799999999999997 . 2.343) (1.56 . 2.074) (2.5 . 2.5) (1.7000000000000002 . 2.185) (2.6 . 2.579) (2.36 . 2.421) (2.46 . 2.497)) (1 (2.7 . 4.27) (2.1799999999999997 . 3.72) (2.5 . 4.0) (2.36 . 3.874) (2.6 . 4.126) (1.7000000000000002 . 3.44)))))
 '(org-icalendar-exclude-tags (quote ("training")))
 '(org-icalendar-include-body nil)
 '(org-modules (quote (org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m org-drill)))
 '(org-refile-targets (quote ((org-agenda-files :maxlevel . 2))))
 '(org-todo-keywords (quote ((sequence "TOREAD" "READ") (sequence "TODO" "DONE"))))
 '(safe-local-variable-values (quote ((nrepl-buffer-ns . "darts180.core") (whitespace-line-column . 80) (lexical-binding . t)))))



(fset 'fla
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217848 111 114 103 45 99 97 108 100 97 118 45 115 121 110 99 return 111 114 46 114 105 101 103 108 101 114 64 103 109 97 105 108 46 99 111 109 return 119 97 97 114 115 110 118 116 102 120 102 120 121 112 118 106 return] 0 "%d")) arg)))

