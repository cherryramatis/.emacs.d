;;; prettier-package.el --- Prettier package configuration

;;; Commentary:

;; Prettier package configuration

;;; Code:

(require 'prettier-js)

(add-hook 'web-mode-hook #'prettier-js-mode)
(add-hook 'typescript-mode-hook #'prettier-js-mode)

(provide 'prettier-package)
;;; prettier-package.el ends here
