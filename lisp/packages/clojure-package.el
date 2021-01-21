;;; clojure-package.el --- Clojure package configuration

;;; Commentary:

;; Clojure package configuration

;;; Code:

(require 'clojure-mode)

(add-hook 'clojure-mode-hook 'paredit-mode)

(provide 'clojure-package)
;;; clojure-package.el ends here
