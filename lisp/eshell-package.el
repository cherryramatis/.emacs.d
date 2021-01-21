;;; eshell-package.el --- Eshell configuration

;;; Commentary:

;; Eshell configuration

;;; Code:

(require 'eshell)

(defun eshell-lifecycle () 
  (when (not (one-window-p))
    (delete-window)))

(advice-add 'eshell-life-is-too-much :after 'eshell-lifecycle)

(provide 'eshell)
;;; eshell-package.el ends here
