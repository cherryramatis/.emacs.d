;;; eyebrowse-package.el --- Eyebrowse package configuration

;;; Commentary:

;; Eyebrowse package configuration

;;; Code:

(require 'eyebrowse)

(define-key eyebrowse-mode-map (kbd "C-c 1") 'eyebrowse-switch-to-window-config-1)
(define-key eyebrowse-mode-map (kbd "C-c 2") 'eyebrowse-switch-to-window-config-2)
(define-key eyebrowse-mode-map (kbd "C-c 3") 'eyebrowse-switch-to-window-config-3)
(define-key eyebrowse-mode-map (kbd "C-c 4") 'eyebrowse-switch-to-window-config-4)
(define-key eyebrowse-mode-map (kbd "C-c 5") 'eyebrowse-switch-to-window-config-5)
(define-key eyebrowse-mode-map (kbd "C-c 6") 'eyebrowse-switch-to-window-config-6)
(define-key eyebrowse-mode-map (kbd "C-c 7") 'eyebrowse-switch-to-window-config-7)
(define-key eyebrowse-mode-map (kbd "C-c 8") 'eyebrowse-switch-to-window-config-8)
(define-key eyebrowse-mode-map (kbd "C-c 9") 'eyebrowse-switch-to-window-config-9)
(eyebrowse-mode t)
(setq eyebrowse-new-workspace t)

(provide 'eyebrowse-package)
;;; eyebrowse-package.el ends here
