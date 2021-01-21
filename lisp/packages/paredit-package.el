;;; paredit-package.el --- Paredit package configuration

;;; Commentary:

;; Paredit package configuration

;;; Code:

(require 'paredit)

(define-key emacs-lisp-mode-map (kbd "M-l")'paredit-forward-slurp-sexp)
(define-key emacs-lisp-mode-map (kbd "M-h") 'paredit-forward-barf-sexp)
;; (define-key clojure-mode-map (kbd "M-l") 'paredit-forward-slurp-sexp)
;; (define-key clojure-mode-map (kbd "M-h") 'paredit-forward-barf-sexp)

(provide 'paredit-package)
;;; paredit-package.el ends here
