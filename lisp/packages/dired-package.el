;;; dired-package.el --- Dired package configuration

;;; Commentary:

;; Dired package configuration

;;; Code:

(require 'dired)
(require 'dired-single)
(require 'all-the-icons-dired)

(global-set-key (kbd "C-c d") 'dired-jump)

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(provide 'dired-package)
;;; dired-package.el ends here
