(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

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

(linum-mode 1)

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

(define-key global-map (kbd "C-x k") 'kill-current-buffer)

(define-key org-mode-map (kbd "M-p") #'outline-previous-visible-heading)
(define-key org-mode-map (kbd "M-n") #'outline-next-visible-heading)

(global-set-key (kbd "C-c a") #'org-agenda-list)

(defun kill-and-close ()
  "Kill buffer and close window"
  (interactive)
  (kill-current-buffer)
  (delete-window)
  )

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

(use-package ivy
  :init (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  )

(use-package swiper
  :bind (("C-s" . swiper)))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-x b" . counsel-switch-buffer)
         ("C-c k" . counsel-ag))
  :config
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package org-roam
  :ensure t
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/Dropbox/roam/")
  :config
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

(defun cherry/lsp-mode-setup ()
  (setq lsp-headerline-breadcumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((lsp-mode . cherry/lsp-mode-setup)
         (web-mode . lsp-mode)
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

(use-package lsp-ivy)

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
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

(setq exec-path (append exec-path '("/usr/local/bin")))
(cond ((eq system-type 'darwin)
   (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
   (setq exec-path (append exec-path '("/usr/local/bin")))))
(use-package add-node-modules-path
  :init (add-node-modules-path))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :bind ("C-x g" . magit))

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

(auto-fill-mode)
(setq-default fill-column 80)

(defun cherry/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . cherry/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files '("~/Dropbox/org/todo.org"))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)
  (setq org-refile-targets
        '(("archive.org" :maxlevel . 1)))

  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-capture-templates
        '(
          ("t" "Today" entry (file+headline "~/Dropbox/org/todo.org" "Today")
           "* TODO %?\nSCHEDULED: %^t\n" :empty-lines 0)
          ("T" "Tomorrow" entry (file+headline "~/Dropbox/org/todo.org" "Tomorrow")
           "* TODO %?\nSCHEDULED: %^t\n" :empty-lines 0)
                  ("d" "No date" entry (file+headline "~/Dropbox/org/todo.org" "No Date")
           "* TODO %?\n" :empty-lines 0)
          ("n" "Note" entry (file+olp "~/Dropbox/org/notes.org" "Inbox")
           "* %?" :empty-lines 0)))

  (define-key global-map (kbd "C-c oc") (lambda () (interactive) (org-capture)))
  (define-key global-map (kbd "C-c oa") (lambda () (interactive) (org-agenda-list)))
  (define-key global-map (kbd "C-c oN") (lambda () (interactive) (find-file-other-window "~/Dropbox/org/todo.org")))
  )

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

(defun cherry/open-eshell ()
  "Open eshell"
  (interactive)
  (if (projectile-project-p)
      (projectile-run-eshell)
      (eshell)))

(defun shell-other-window (buffer-name)
  "Open a `shell' in a new window."
  (interactive)
  (let ((buf (cherry/open-eshell)))
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
