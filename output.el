
(setq user-full-name "Gregor Riegler"
      user-mail-address "gregor.riegler@gmail.com")

(add-to-list 'load-path (concat user-emacs-directory "custom"))
(add-to-list 'load-path (concat user-emacs-directory "custom/use-package"))

(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(starter-kit starter-kit-bindings
                                  starter-kit-js starter-kit-lisp
                                  ;; Clojure & friends
                                  clojure-mode nrepl rainbow-delimiters paredit
                                  ;; Project navigation
                                  projectile magit
                                  ;; Evil
                                  evil evil-leader evil-numbers evil-paredit evil-nerd-commenter
                                  ;; Misc.
                                  ack-and-a-half auto-complete markdown-mode twilight-theme
                                  ac-nrepl ace-jump-mode
                                  ;; Haskell
                                  flymake-hlint haskell-mode flymake-haskell-multi hlinum
                                  )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'use-package)
(require 'bind-key)

(load (concat user-emacs-directory "clojure-starter-kit.el"))

(setq initial-buffer-choice "~/org/home.org")

(setenv "PATH" (concat (getenv "PATH") ":/home/greg/.cabal/bin"))

(setq eval-expression-debug-on-error nil)

(remove-hook 'text-mode-hook 'turn-on-flyspell)

(global-set-key (kbd "M-l") 'forward-word)
(global-set-key (kbd "M-h") 'backward-word)
(global-set-key (kbd "M-a") 'find-tag)

(global-set-key (kbd "RET") 'newline-and-indent)

(global-set-key (kbd "C-ä") 'delete-other-windows)
(global-set-key (kbd "C-Ä") 'delete-window)
(global-set-key (kbd "ö") 'other-window)

(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; (global-auto-complete-mode f)

(delete '("\\.pdf\\'" . default) org-file-apps)
(add-to-list 'org-file-apps '("\\.pdf\\'" . "evince \"%s\""))
(add-to-list 'org-file-apps '("\\.pdf::\\([0-9]+\\)\\'" . "evince \"%s\" -p %1"))

(use-package evil
             :config (progn
                       (evil-mode 1))
             :init
             (progn
              (define-key evil-insert-state-map "j"
                '(lambda ()
                   (interactive)
                   (insert "j")
                   (let ((event (read-event nil)))
                     (if (= event ?j)
                         (progn
                           (backward-delete-char 1)
                           (evil-normal-state))
                       (push event unread-command-events)))))
              (global-set-key (kbd "C-S-u") 'evil-scroll-up)
              (global-set-key (kbd "C-S-o") 'evil-execute-in-emacs-state)))

;; (defun evil-insert-jj-for-normal-mode ()
;;   (interactive)
;;   (insert "j")
;;   (let ((event (read-event nil)))
;;     (if (= event ?j)
;;       (progn
;;         (backward-delete-char 1)
;;         (evil-normal-state))
;;       (push event unread-command-events))))

;; (define-key evil-insert-state-map "j" 'evil-insert-jj-for-normal-mode)

(global-evil-leader-mode)
               (evil-leader/set-leader ",")

               (evil-leader/set-key
                 "e" 'find-file
                 "b" 'switch-to-buffer
                 "o" 'org-iswitchb
                 "w" 'save-buffer
                 "l" 'ace-jump-line-mode
                 "k" 'kill-buffer)

               (evil-leader/set-key
                 "ci" 'evilnc-comment-or-uncomment-lines
                 "cl" 'evilnc-comment-or-uncomment-to-the-line)

;; (global-evil-leader-mode)
;; (evil-leader/set-leader ",")

;; (evil-leader/set-key
;;   "e" 'find-file
;;   "b" 'switch-to-buffer
;;   "o" 'org-iswitchb
;;   "w" 'save-buffer
;;   "l" 'ace-jump-line-mode
;;   "k" 'kill-buffer)

;; (evil-leader/set-key
;;   "ci" 'evilnc-comment-or-uncomment-lines
;;   "cl" 'evilnc-comment-or-uncomment-to-the-line)

;; (global-set-key (kbd "C-S-u") 'evil-scroll-up)
;; (global-set-key (kbd "C-S-o") 'evil-execute-in-emacs-state)

(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-line-mode))

(use-package paredit
        ;;     :commands paredit-mode
             :init
             (progn
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
               (global-set-key (kbd "C-c x") 'paredit-open-curly)
               (global-set-key (kbd "C-s-l j") 'paredit-forward-down)
               (global-set-key (kbd "C-s-l k") 'paredit-forward-up)
               (global-set-key (kbd "C-s-h j") 'paredit-backward-down)
               (global-set-key (kbd "C-s-h k") 'paredit-backward-up)
               (global-set-key (kbd "C-c (") 'paredit-wrap-round)
               (global-set-key (kbd "C-c {") 'paredit-wrap-curly)
               (global-set-key (kbd "C-c [") 'paredit-wrap-square)
               ))

(setq org-directory "~/org")

(defun my/org-refile-within-current-buffer ()
  "Move the entry at point to another heading in the current buffer."
  (interactive)
  (let ((org-refile-targets '((nil :maxlevel . 5))))
    (org-refile)))

(global-set-key (kbd "C-c C-S-w") 'my/org-refile-within-current-buffer)

(setq org-todo-keywords (quote ((sequence "TOREAD" "READ") (sequence "TODO" "DONE"))))
(setq org-todo-keyword-faces
      '(
        ("UTODO"  . (:foreground "#b70101" :weight bold :slant italic))
        ("UTOLEARN"  . (:foreground "#b70101" :weight bold :slant italic))
        ("UTOIMPLEMENT"  . (:foreground "#b70101" :weight bold :slant italic))
        ;; ("STARTED"  . (:foreground "#b70101" :weight bold))
        ;; ("APPT"  . (:foreground "sienna" :weight bold))
        ;; ("PROJ"  . (:foreground "blue" :weight bold))
        ;; ("ZKTO"  . (:foreground "orange" :weight bold))
        ;; ("WAITING"  . (:foreground "orange" :weight bold))
        ;; ("DONE"  . (:foreground "forestgreen" :weight bold))
        ;; ("DELEGATED"  . (:foreground "forestgreen" :weight bold))
        ;; ("CANCELED"  . shadow)
        ))

(use-package org-protocol
             :init (progn
                     (setq org-protocol-default-template-key "l")
                     (setq org-capture-templates
                           '(("t" "Todo" entry (file+datetree "~/org/journal.org")
                              "* TODO %?")
                             ("w" "TOTWEET" entry (file+datetree "~/org/journal.org")
                              "* TOTWEET %?")
                             ("b" "starting with b...")
                             ("bu" "Tobuy" entry (file+datetree "~/org/journal.org")
                              "* TOBUY %?")
                             ("bl" "TOBLOG" entry (file+olp "~/org/home.org" "Blog")
                              "* TOBLOG %^{Heading}\n\t%?")
                             ("l" "starting with l... ")
                             ("li" "Link" entry (file+olp "~/org/bookmarks.org" "Bookmarks")
                              "* %a\n %?\n %i")
                             ("lb" "TOBLOG from Browser" entry (file+olp "~/org/home.org" "Blog")
                              "* TOBLOG %?\n\t%a")
                             ("lo" "TOLOOKAT" entry (file+datetree "~/org/journal.org")
                              "* TOLOOKAT %?")
                             ("lu" "TOLOOKAT from Browser" entry (file+datetree "~/org/journal.org")
                              "* TOLOOKAT %?\n\t%a")
                             ("lt" "TODO from Browser" entry (file+datetree "~/org/journal.org")
                              "* TODO %?\n\t%a")
                             ("p" "Project" entry (file+olp "~/org/projects.org" "Programming")
                              "* %^{Heading}\n\t%?")
                             ("r" "TOREAD" entry (file+olp "~/org/home.org" "Bücher")
                              "* TOREAD %^{Heading}\n\t%?")
                             ("y" "Journal prompted" item (file+datetree+prompt "~/org/journal.org")
                              "%?")
                             ("j" "Journal" item (file+datetree "~/org/journal.org")
                              "%?")))
                     (define-key global-map "\C-cc" 'org-capture)))

(setq org-agenda-files (list "~/org/cal.org" "~/org/bookmarks.org" "~/org/journal.org" "~/org/projects.org" "~/org/home.org" "~/org/uni.org"))
(setq org-agenda-custom-commands
      '(("u" todo "UTODO|UTOLEARN|UTOIMPLEMENT")
        ("l" todo "TOLOOKAT")
        ("d" todo "TODO")))

(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)

(setq org-agenda-archives-mode nil)
(setq org-agenda-skip-comment-trees nil)
(setq org-agenda-skip-function nil)

(load-file "~/.emacs.d/custom/org-caldav.el")
(load-file "~/.emacs.d/custom/org-import-calendar.el")
(use-package org-caldav
             :init (progn 
                     (setq org-icalendar-exclude-tags (quote ("training")))
                     (setq org-icalendar-include-body nil)
                     (setq org-icalendar-use-scheduled (quote nil))
                     (define-key evil-normal-state-map (kbd "C-p") 'org-caldav-sync)
                     
                     (global-set-key (kbd "C-ü")
                                     '(lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217848 111 114 103 45 99 97 108 100 97 118 45 115 121 110 99 return 111 114 46 114 105 101 103 108 101 114 64 103 109 97 105 108 46 99 111 109 return 119 97 97 114 115 110 118 116 102 120 102 120 121 112 118 106 return] 0 "%d")) arg)))
                     (setq org-caldav-calendar-id "vpvsjgj9avredjnv58kt85lklo@group.calendar.google.com")
                     (setq org-icalendar-timezone "UTC")
                     (setq org-caldav-inbox "~/org/cal.org")
                     (setq org-caldav-files (list "~/org/home.org" "~/org/uni.org"))
                     (setq org-caldav-sync-changes-to-org 'title-only)
                     (setq org-icalendar-include-todo nil)
                     (setq org-icalendar-store-UID t)
                     ))

(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 2))))
(setq org-M-RET-may-split-line (quote ((default))))
(setq org-goto-interface 'outline org-goto-max-level 10)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cL" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key (kbd "M-o") 'imenu)
(setq org-log-done 'time)
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-return-follows-link t)

(load-file "~/.emacs.d/custom/org-drill.el")
(use-package org-drill
             :init (progn (setq org-drill-learn-fraction 0.45)))

(load-file "~/.emacs.d/custom/org-learn.el")
(require 'org-learn)

(setq org-mobile-inbox-for-pull "~/org/notes.org")
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

(setq org-src-fontify-natively t)
(setq org-confirm-babel-evaluate nil)
(setq org-src-window-setup 'current-window)

(eval-after-load 'org
       '(add-to-list 'org-structure-template-alist
                    '("x" "#+begin_src emacs-lisp \n?\n#+end_src", "<src lang='emacs-lisp'>\n?\n</src>")))

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
(provide 'ob-clojure)
(eval-after-load 'org
       '(add-to-list 'org-structure-template-alist
                    '("c" "#+begin_src clojure :tangle src/\n?\n#+end_src", "<src lang='clojure'>\n?\n</src>")))

(load-file "~/.emacs.d/custom/ob-haskell.el")

(require 'org-attach)
(org-add-link-type "att" 'org-attach-open-link)
(defun org-attach-open-link (file)
  (org-open-file (org-attach-expand file)))
(set-variable 'org-attach-store-link-p t)

(setq org-dotemacs-default-file (concat user-emacs-directory "configuration.org"))

(setq haskell-hoogle-command "") 
(setq haskell-package-conf-file "/home/greg/.ghc/x86_64-linux-7.4.1/package.conf")
(setq haskell-process-path-cabal-dev "/usr/bin/cabal-dev")
(setq haskell-process-path-ghci "ghci")
(setq haskell-process-prompt-restart-on-cabal-change nil)
(setq haskell-process-suggest-language-pragmas t)
(setq haskell-process-type (quote cabal-dev))
(setq haskell-program-name "cabal-dev ghci")
(setq haskell-stylish-on-save t)
(setq haskell-tags-on-save t)
(setq inferior-haskell-web-docs-base "http://hackage.haskell.org/packages/archive/")

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

(setq flymake-gui-warnings-enabled nil)
(global-set-key (kbd "C-c e") 'flymake-display-err-menu-for-current-line)
(global-set-key (kbd "C-c C-n") 'flymake-goto-next-error)
(global-set-key (kbd "C-c C-p") 'flymake-goto-next-error)

(setq projectile-use-native-indexing t)

(use-package ac-nrepl
                    :commands nrepl-mode
                    :init (progn
                            (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
                            (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
                            (eval-after-load "auto-complete"
                              '(add-to-list 'ac-modes 'nrepl-mode))
                            (add-hook 'auto-complete-mode-hook '(lambda () (setq completion-at-point-functions '(auto-complete))))
                            (add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
                            (add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)
                ;;            (define-key nrepl-interaction-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)
))

;; (defun set-auto-complete-as-completion-at-point-function ()
;;   (setq completion-at-point-functions '(auto-complete)))
;; (global-set-key (kbd "C-;") 'nrepl-jump-back)
;; (global-set-key (kbd  "C-:") 'nrepl-jump)

;; (require 'ac-nrepl)
;; (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
;; (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
;; (eval-after-load "auto-complete"
;;   '(add-to-list 'ac-modes 'nrepl-mode))
;; (add-hook 'auto-complete-mode-hook '(setq completion-at-point-functions '(auto-complete)))
;; (add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
;; (add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)
;; (define-key nrepl-interaction-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)