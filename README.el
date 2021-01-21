(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(unless (package-installed-p 'exec-path-from-shell)
  (package-install 'exec-path-from-shell))

(require 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(require 'use-package)
(setq use-package-always-ensure t)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

(setq default-directory "~/")

;; Default font
(set-face-attribute 'default nil :font "Iosevka Term Light" :height 150)

;; Pitched fonts
(set-face-attribute 'fixed-pitch nil :font "Iosevka Term Light" :height 150)

;; Variable pitched fonts
(set-face-attribute 'variable-pitch nil :font "Iosevka Term Light" :height 150)

(show-paren-mode 1)

(electric-pair-mode 1)
(add-hook 'org-mode-hook (lambda () (electric-pair-mode -1)))

(setq column-numbers-mode t)
(global-display-line-numbers-mode)
(add-hook 'org-mode-hook (global-display-line-numbers-mode -1))

(global-hl-line-mode 1)
(add-hook 'org-mode-hook (global-hl-line-mode -1))

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Keep transient cruft out of ~/.emacs.d/
(setq user-emacs-directory "~/.cache/emacs/"
      backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      url-history-file (expand-file-name "url/history" user-emacs-directory)
      auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" user-emacs-directory)
      projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" user-emacs-directory))

;; Keep customization settings in a temporary file
(setq custom-file
      (if (boundp 'server-socket-dir)
	  (expand-file-name "custom.el" server-socket-dir)
	(expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

(define-key org-mode-map (kbd "M-p") #'outline-previous-visible-heading)
(define-key org-mode-map (kbd "M-n") #'outline-next-visible-heading)
(define-key org-mode-map (kbd "M-h") #'outline-promote)
(define-key org-mode-map (kbd "M-l") #'outline-demote)
(define-key org-mode-map (kbd "M-j") #'org-down-element)
(define-key org-mode-map (kbd "M-k") #'org-up-element)

(global-set-key (kbd "C-c a") #'org-agenda-list)
(global-set-key (kbd "s-a") #'org-agenda-list)

(defun kill-and-close ()
  "Kill buffer and close window"
  (interactive)
  (if (one-window-p)
      (kill-current-buffer)
    (progn
      (kill-current-buffer)
      (delete-window))))

(global-set-key (kbd "s-k") #'kill-and-close)
(global-set-key (kbd "s-o") #'other-window)
(global-set-key (kbd "s-b") #'switch-to-buffer-other-window)
(global-set-key (kbd "s-1") #'delete-other-windows)
(global-set-key (kbd "s-2") #'split-window-below)
(global-set-key (kbd "s-3") #'split-window-right)
(global-set-key (kbd "s-0") #'delete-window)

(defun cherry/find-files ()
  "Use either find-file or projectile-find-file depending if is on a project"
  (interactive)
  (if (projectile-project-p)
      (projectile-find-file-other-window)
    (find-file-other-window (read-file-name "File: "))))


(global-set-key (kbd "s-f") #'cherry/find-files)

(global-set-key (kbd "C-/") #'undo)
(global-set-key (kbd "C-_") #'redo)

(define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 10)
  )

(use-package crux
  :config
  (global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
  (global-set-key [(shift return)] #'crux-smart-open-line)
  (global-set-key (kbd "C-<backspace>") #'crux-kill-line-backwards)
  (global-set-key [remap kill-whole-line] #'crux-kill-whole-line)
  (global-set-key (kbd "C-j") #'crux-top-join-line)
  (global-set-key (kbd "C-c d") #'crux-duplicate-current-line-or-region))

(use-package modus-vivendi-theme
  :config
  (load-theme 'modus-vivendi t))
(use-package modus-operandi-theme)

(use-package bufler
  :bind (("C-x C-b" . bufler))
  :config
  (setf bufler-groups
	(bufler-defgroups
	  ;; Subgroup collecting all named workspaces.
	  (group (auto-workspace))
	  ;; Subgroup collecting buffers in a projectile project.
	  (group (auto-projectile))
	  ;; Grouping browser windows
	  (group
	   ;; Subgroup collecting all `help-mode' and `info-mode' buffers.
	   (group-or "Help/Info"
		     (mode-match "*Help*" (rx bos (or "help-" "helpful-")))
		     ;; (mode-match "*Helpful*" (rx bos "helpful-"))
		     (mode-match "*Info*" (rx bos "info-"))))
	  (group
	   ;; Subgroup collecting all special buffers (i.e. ones that are not
	   ;; file-backed), except `magit-status-mode' buffers (which are allowed to fall
	   ;; through to other groups, so they end up grouped with their project buffers).
	   (group-and "*Special*"
		      (name-match "**Special**"
				  (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace" "Pinentry") "*"))
		      (lambda (buffer)
			(unless (or (funcall (mode-match "Magit" (rx bos "magit-status"))
					     buffer)
				    (funcall (mode-match "Dired" (rx bos "dired"))
					     buffer)
				    (funcall (auto-file) buffer))
			  "*Special*"))))
	  ;; Group remaining buffers by major mode.
	  (auto-mode))))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :config
  (global-set-key (kbd "C-c d") 'dired-jump)
  )

(use-package dired-single)
(use-package all-the-icons-dired
  :after (all-the-icons dired)
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  )

(use-package helm
  :init (helm-mode 1)
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)))

(use-package helm-swoop
  :bind (("C-s" . helm-swoop)))

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

(defun cherry/lsp-mode-setup ()
  (setq lsp-headerline-breadcumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((lsp-mode . cherry/lsp-mode-setup)
         (web-mode . lsp-mode)
         (python-mode . lsp-mode)
         (typescript-mode . lsp-mode))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  )

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-jedi
  :after lsp-mode 
  :config
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-disabled-clients 'pyls)
    (add-to-list 'lsp-enabled-clients 'jedi)))

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (define-key company-active-map (kbd "\C-n") 'company-select-next)
  (define-key company-active-map (kbd "\C-p") 'company-select-previous)
  (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
  (setq company-selection-wrap-around t)
  (company-tng-configure-default)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

(use-package web-mode
  :mode "\\.tsx\\'"
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode)))

(use-package prettier-js
  :config
  (add-hook 'web-mode-hook #'prettier-js-mode)
  (add-hook 'typescript-mode-hook #'prettier-js-mode)
  )

(use-package flycheck
  :init (global-flycheck-mode))

(setq exec-path (append exec-path '("/usr/local/bin" "/Library/Python/3.9/lib/python/site-packages/")))
(cond ((eq system-type 'darwin)
   (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin" ":/Library/Python/3.9/lib/python/site-packages/"))
   (setq exec-path (append exec-path '("/usr/local/bin" "/Library/Python/3.9/lib/python/site-packages/")))))
(use-package add-node-modules-path
  :init (add-node-modules-path))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'helm))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package magit
  :bind (("C-x g" . magit)
         ("s-g" . magit)))

(use-package tree-sitter)
(use-package tree-sitter-langs)

(use-package avy
  :config
  (global-set-key (kbd "M-s") 'avy-goto-char))

(use-package ace-jump-mode
  :config
  (autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
  (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
  (autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
  (eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
  (define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark))

(use-package guru-mode
  :config
  (guru-global-mode +1))

(use-package yasnippet
  :diminish
  :config (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package eyebrowse
  :diminish eyebrowse-mode
  :config (progn
                        (define-key eyebrowse-mode-map (kbd "C-c 1") 'eyebrowse-switch-to-window-config-1)
            (define-key eyebrowse-mode-map (kbd "C-c 2") 'eyebrowse-switch-to-window-config-2)
            (define-key eyebrowse-mode-map (kbd "C-c 3") 'eyebrowse-switch-to-window-config-3)
            (define-key eyebrowse-mode-map (kbd "C-c 4") 'eyebrowse-switch-to-window-config-4)
            (define-key eyebrowse-mode-map (kbd "C-c 5") 'eyebrowse-switch-to-window-config-5)
            (define-key eyebrowse-mode-map (kbd "C-c 6") 'eyebrowse-switch-to-window-config-6)
            (define-key eyebrowse-mode-map (kbd "C-c 7") 'eyebrowse-switch-to-window-config-7)
            (define-key eyebrowse-mode-map (kbd "C-c 8") 'eyebrowse-switch-to-window-config-8)
            (define-key eyebrowse-mode-map (kbd "C-c 9") 'eyebrowse-switch-to-window-config-9)
            (eyebrowse-mode t)
            (setq eyebrowse-new-workspace t)))

(use-package multiple-cursors
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package undo-tree
  :init (global-undo-tree-mode))

(use-package paredit
  :bind (("C-M-l" . paredit-forward-slurp-sexp)
         ("C-M-h" . paredit-forward-barf-sexp))
  :config
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode))

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))

(use-package circe
  :config
  (setq circe-network-options
      '(("Freenode"
         :tls t
         :nick "cherryramatis"
         :sasl-username "cherryramatis"
         :sasl-password (lambda (x) (read-passwd "SASL password: "))
         :channels ("#emacs-circe" "#emacs-beginners" "#emacs" "#emacs-offtopic")
         ))))

(use-package slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
  :name "hagens"
  :default t
  :client-id "fabio.ramatis@hagens.com.br"
  :client-secret "A45b84c99@"
  :token "xoxs-188549451731-1300081492802-1636536881728-dad11d8f228cdd8a1785c5f798fd2be36120106bfe42feff17f5964f1717ed69"
  :subscribed-channels '(general slackbot))
  (global-set-key (kbd "s-s") #'slack-im-select)
  )

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))

(use-package telega
  :bind (("s-t" . telega))
  :commands (telega)
  :defer t
  :config
  (add-hook 'telega-load-hook
        (lambda ()
          (define-key global-map (kbd "C-x t") telega-prefix-map))))

(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode))
  :config
  (add-hook 'clojure-mode-map 'paredit-mode)
  )

(use-package cider
  :mode (("\\.clj\\'" . cider-mode)
         ("\\.edn\\'" . cider-mode)))

(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))

(auto-fill-mode)
(setq-default fill-column 80)

(defun cherry/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(add-hook 'org-mode-hook 'cherry/org-mode-setup)
(setq org-ellipsis " ▾")
(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-agenda-files '("~/Dropbox/org/todo.org"))
(setq org-refile-targets
      '(("archive.org" :maxlevel . 1)))

(setq org-capture-templates
        '(("t" "Todo" entry (file+olp "~/Dropbox/org/todo.org" "Inbox")
           "* TODO %?\nSCHEDULED: %^t\n" :empty-lines 1)
          ("n" "Note" entry (file+olp "~/Dropbox/org/todo.org" "Inbox")
           "* %?" :empty-lines 1)))

(define-key global-map (kbd "C-c oc") #'org-capture)
(define-key global-map (kbd "C-c oN") (lambda () (interactive) (find-file "~/todo/org/notes.org")))

(use-package org-bullets
 :hook (org-mode . (lambda () (org-bullets-mode 1))))

(defun cherry/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . cherry/org-mode-visual-fill))

;; TODO: Define a binding for this if needed

(defun cherry/org-archive-done-tasks ()
  "Archive all DONE subheadings"
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'file))

(defun cherry/open-term ()
  "Open eshell"
  (interactive)
  (if (projectile-project-p)
      (projectile-run-eshell)
    (eshell)))

(defun shell-other-window (buffer-name)
  "Open a `shell' in a new window."
  (interactive)
  (let ((buf (cherry/open-term)))
    (switch-to-buffer (other-buffer buf))
    (switch-to-buffer-other-window buf)
    (rename-buffer buffer-name)
    ))

(defun named-eshell ()
  "Open eshell with buffer name"
  (interactive)
  (shell-other-window (read-string "Enter the shell name: ")))

(global-set-key (kbd "C-c t") #'named-eshell)

(require 'eshell)

(defun eshell-lifecycle () 
  (when (not (one-window-p))
    (delete-window)))

(advice-add 'eshell-life-is-too-much :after 'eshell-lifecycle)
