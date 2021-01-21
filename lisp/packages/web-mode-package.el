;;; web-mode-package.el --- Web mode package configuration

;;; Commentary:

;; Web mode package configuration

;;; Code:

(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

(provide 'web-mode-package)
;;; web-mode-package.el ends here
