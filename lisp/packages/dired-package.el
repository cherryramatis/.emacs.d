;;; dired-package.el --- Dired package configuration

;;; Commentary:

;; Dired package configuration

;;; Code:

(require 'dired)
(require 'dired-single)
(require 'all-the-icons-dired)

(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's loaded."
  ;; <add other stuff here>
  (define-key dired-mode-map [remap dired-find-file]
    'dired-single-buffer)
  (define-key dired-mode-map [remap dired-mouse-find-file-other-window]
    'dired-single-buffer-mouse)
  (define-key dired-mode-map [remap dired-up-directory]
    'dired-single-up-directory))

;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; we're good to go; just add our bindings
    (my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-init))


(define-key dired-mode-map (kbd "f") 'helm-find-files)
(define-key dired-mode-map (kbd "DEL") 'dired-up-directory)
(global-set-key (kbd "C-c d") 'dired-jump)

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(provide 'dired-package)
;;; dired-package.el ends here
