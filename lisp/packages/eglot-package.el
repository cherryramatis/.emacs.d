;;; eglot-package.el --- Eglot package configuration

;;; Commentary:

;; Eglot package configuration

;;; Code:

(require 'eglot)

(add-to-list 'eglot-server-programs
             '((web-mode) "typescript-language-server" "--stdio"))

(add-hook 'web-mode-hook #'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)

(provide 'eglot-package)
;;; eglot-package.el ends here
