;;; package.el --- Installing packages

;;; Commentary:

;; Installing packages

;;; Code:

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(defun package! (package)
  "Install PACKAGE."
  (interactive "P")
  (if (not (package-installed-p package))
      (package-install package)))
  
(package! 'exec-path-from-shell)
(package! 'which-key)
(package! 'doom-modeline)
(package! 'crux)
(package! 'modus-vivendi-theme)
(package! 'modus-operandi-theme)
(package! 'bufler)
(package! 'helm)
(package! 'helm-swoop)
(package! 'eglot)
(package! 'company)
(package! 'company-box)
(package! 'typescript-mode)
(package! 'web-mode)
(package! 'prettier-js)
(package! 'flycheck)
(package! 'add-node-modules-path)
(package! 'projectile)
(package! 'helm-projectile)
(package! 'magit)
(package! 'tree-sitter)
(package! 'tree-sitter-langs)
(package! 'avy)
(package! 'ace-jump-mode)
(package! 'guru-mode)
(package! 'yasnippet)
(package! 'yasnippet-snippets)
(package! 'eyebrowse)
(package! 'multiple-cursors)
(package! 'undo-tree)
(package! 'paredit)
(package! 'circe)
(package! 'slack)
(package! 'telega)
(package! 'clojure-mode)
(package! 'cider)
(package! 'pipenv)
(package! 'org-roam)
(package! 'org-bullets)
(package! 'alert)
(package! 'eshell-syntax-highlighting)
(package! 'eshell-git-prompt)
(package! 'dired-single)

(provide 'package)
;;; package.el ends here
