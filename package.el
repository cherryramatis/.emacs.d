(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Comment out if you've already loaded this package...
(require 'cl-lib)

(defvar my-packages
  '(exec-path-from-shell which-key doom-modeline crux modus-vivendi-theme modus-operandi-theme bufler helm helm-swoop lsp-mode lsp-ui helm-lsp lsp-jedi company company-box typescript-mode web-mode prettier-js flycheck add-node-modules-path projectile helm-projectile magit tree-sitter tree-sitter-langs avy ace-jump-mode guru-mode yasnippet yasnippet-snippets eyebrowse multiple-cursors undo-tree paredit circe slack telega clojure-mode cider pipenv org-roam)
  "A list of packages to ensure are installed at launch.")

(defun my-packages-installed-p ()
  (cl-loop for p in my-packages
           when (not (package-installed-p p)) do (cl-return nil)
           finally (cl-return t)))

(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (package-refresh-contents)
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))
