;;; lsp-package.el --- Lsp package configuration

;;; Commentary:

;; Lsp package configuration

;;; Code:

(require 'lsp-mode)
(require 'lsp-ui)
(require 'lsp-jedi)

(defun cherry/lsp-mode-setup ()
  (setq lsp-headerline-breadcumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(add-hook 'lsp-mode-hook 'cherry/lsp-mode-setup)
(add-hook 'web-mode-hook 'lsp-mode)
(add-hook 'python-mode-hook 'lsp-mode)
(add-hook 'typescript-mode-hook 'lsp-mode)

(setq lsp-keymap-prefix "C-c l")

(lsp-enable-which-key-integration t)

(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(setq-default lsp-ui-doc-position 'bottom)

(with-eval-after-load "lsp-mode"
  (add-to-list 'lsp-disabled-clients 'pyls)
  (add-to-list 'lsp-enabled-clients 'jedi))

(provide 'lsp-package)
;;; lsp-package.el ends here
