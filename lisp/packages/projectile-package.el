;;; projectile-package.el --- Projectile package configuration

;;; Commentary:

;; Projectile package configuration

;;; Code:

(require 'projectile)

(projectile-mode)

(setq-default projectile-completion-system 'helm)

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(when (file-directory-p "~/projects")
  (setq projectile-project-search-path '("~/projects")))

(setq projectile-switch-project-action #'projectile-dired)

(provide 'projectile-package)
;;; projectile-package.el ends here
