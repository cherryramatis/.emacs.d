;;; company-package.el --- Company package configuration

;;; Commentary:

;; Company package configuration

;;; Code:

(require 'company)
(require 'company-box)

(add-hook 'after-init-hook 'global-company-mode)
(define-key company-active-map (kbd "\C-n") 'company-select-next)
(define-key company-active-map (kbd "\C-p") 'company-select-previous)
(define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
(setq company-selection-wrap-around t)

(setq-default company-minimum-prefix-length 1)
(setq-default company-idle-delay 0.0)

(add-hook 'company-mode-hook 'company-box-mode)

(provide 'company-package)
;;; company-package.el ends here
