;;; telega-package.el --- Telega package configuration

;;; Commentary:

;; Telega package configuration

;;; Code:

(require 'telega)

(global-set-key (kbd "s-t") 'telega)
(add-hook 'telega-load-hook
        (lambda ()
          (define-key global-map (kbd "C-x t") telega-prefix-map)))

(provide 'telega-package)
;;; telega-package.el ends here
