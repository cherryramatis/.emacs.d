;;; environment-macos.el --- Environment variables reads on MacOS

;;; Commentary:

;; Environment variables reads on MacOS

;;; Code:

(require 'add-node-modules-path)

(setq exec-path (append exec-path '("/usr/local/bin" "/Library/Python/3.9/lib/python/site-packages/")))
(cond ((eq system-type 'darwin)
   (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin" ":/Library/Python/3.9/lib/python/site-packages/"))
   (setq exec-path (append exec-path '("/usr/local/bin" "/Library/Python/3.9/lib/python/site-packages/")))))

(add-node-modules-path)

(provide 'environment-macos)
;;; environment-macos.el ends here
