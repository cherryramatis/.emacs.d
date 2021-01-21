;;; helm-package.el --- Helm package configuration

;;; Commentary:

;; Helm package configuration

;;; Code:

(require 'helm)
(require 'helm-swoop)

(helm-mode 1)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-s") 'helm-swoop)

(provide 'helm-package)
;;; helm-package.el ends here
