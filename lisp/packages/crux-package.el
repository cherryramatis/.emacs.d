;;; crux-package.el --- Crux package configuration

;;; Commentary:

;; Crux package configuration

;;; Code:

(require 'crux)

(global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
(global-set-key [(shift return)] #'crux-smart-open-line)
(global-set-key (kbd "C-<backspace>") #'crux-kill-line-backwards)
(global-set-key [remap kill-whole-line] #'crux-kill-whole-line)
(global-set-key (kbd "C-j") #'crux-top-join-line)
(global-set-key (kbd "C-c d") #'crux-duplicate-current-line-or-region)

(provide 'crux-package)
;;; crux-package.el ends here
