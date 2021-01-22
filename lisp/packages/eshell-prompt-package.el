;;; eshell-prompt-package.el --- Eshell prompt package configuration

;;; Commentary:

;; Eshell prompt package configuration

;;; Code:

(require 'eshell-git-prompt)

(eshell-git-prompt-use-theme 'powerline)

(provide 'eshell-prompt-package)
;;; eshell-prompt-package.el ends here
