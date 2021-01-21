;;; lisp.el --- Lisp language configuration

;;; Commentary:

;; Lisp general configuration(this includes emacs-lisp, clojure, etc...)

;;; Code:

(show-paren-mode 1)
(electric-pair-mode 1)

(setq column-number-mode t)
(add-hook 'prog-mode-hook (display-line-numbers-mode 1))

(define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer)

(provide 'lisp)
;;; lisp.el ends here
