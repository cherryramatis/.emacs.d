;;; org-roam-package.el --- Org roam package configuration

;;; Commentary:

;; Org roam package configuration

;;; Code:

(require 'org-roam)

(add-hook 'after-init-hook 'org-roam-mode)

(setq-default org-roam-directory "~/Dropbox/roam/")

(define-key org-roam-mode-map (kbd "C-c n l") 'org-roam)
(define-key org-roam-mode-map (kbd "C-c n f") 'org-roam-find-file)
(define-key org-mode-map (kbd "C-c n i") 'org-roam-insert)
(define-key org-mode-map (kbd "C-c n I") 'org-roam-insert-immediate)

(provide 'org-roam-package)
;;; org-roam-package.el ends here
