;;; pipenv-package.el --- Pipenv package configuration

;;; Commentary:

;; Pipenv package configuration

;;; Code:

(require 'pipenv)

(add-hook 'python-mode-hook 'pipenv-mode)

(setq
 pipenv-projectile-after-switch-function
 #'pipenv-projectile-after-switch-extended)

(provide 'pipenv-package)
;;; pipenv-package.el ends here
