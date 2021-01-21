;;; circe-package.el --- Circe package configuration

;;; Commentary:

;; Circe package configuration

;;; Code:

(require 'circe)

(setq circe-network-options
      '(("Freenode"
         :tls t
         :nick "cherryramatis"
         :sasl-username "cherryramatis"
         :sasl-password (lambda (x) (read-passwd "SASL password: "))
         :channels ("#emacs-circe" "#emacs-beginners" "#emacs" "#emacs-offtopic")
         )))

(provide 'circe-package)
;;; circe-package.el ends here
