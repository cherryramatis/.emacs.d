;;; magit-package.el --- Magit package configuration

;;; Commentary:

;; Magit package configuration

;;; Code:

(require 'magit)

(global-set-key (kbd "C-x g") 'magit)
(global-set-key (kbd "s-g") 'magit)

(provide 'magit-package)
;;; magit-package.el ends here
