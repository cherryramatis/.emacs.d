;;; init.el --- Init file that loads emacs

;;; Commentary:

;; Init file that loads Emacs
;; This is the last used setup
;; (require 'org)
;; (org-babel-load-file (expand-file-name "~/.emacs.d/README.org"))

;;; Code:

(defun load-package (package)
  "Load PACKAGE in an easy way."
  (load (format "~/.emacs.d/lisp/%s" package)))

(load "~/.emacs.d/secrets.el")
(load-package "general")
(load-package "keybindings")
(load-package "eshell-package")
(load-package "org-package")
(load-package "agenda-notifications")
(load-package "programming/lisp")
(load-package "packages/ace-jump-package")
(load-package "packages/alert-package")
(load-package "packages/avy-package")
(load-package "packages/bufler-package")
;; (load-package "packages/circe-package")
;; (load-package "packages/clojure-package")
(load-package "packages/company-package")
(load-package "packages/crux-package")
(load-package "packages/dired-package")
(load-package "packages/doom-modeline-package")
(load-package "packages/environment-macos")
(load-package "packages/eyebrowse-package")
(load-package "packages/flycheck-package")
(load-package "packages/guru-package")
(load-package "packages/helm-package")
(load-package "packages/web-mode-package")
(load-package "packages/typescript-package")
(load-package "packages/lsp-package")
(load-package "packages/magit-package")
(load-package "packages/modus-vivendi-package")
(load-package "packages/multiple-cursors-package")
(load-package "packages/paredit-package")
(load-package "packages/pipenv-package")
(load-package "packages/prettier-package")
(load-package "packages/projectile-package")
(load-package "packages/slack-package")
(load-package "packages/telega-package")
(load-package "packages/undo-tree-package")
(load-package "packages/which-key-package")
(load-package "packages/yas-package")
(load-package "packages/org-roam-package")

(provide 'init)
;;; init.el ends here
