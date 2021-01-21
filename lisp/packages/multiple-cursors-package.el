;;; multiple-cursors-package.el --- Multiple cursors package configuration

;;; Commentary:

;; Multiple cursors package configuration

;;; Code:

(require 'multiple-cursors)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(provide 'multiple-cursors-package)
;;; multiple-cursors-package.el ends here
