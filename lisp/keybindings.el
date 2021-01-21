;;; keybindings.el --- Keybinding for custom or native functions

;;; Commentary:

;; Keybinding for custom or native functions

;;; Code:

(defun kill-and-close ()
  "Kill buffer and close window"
  (interactive)
  (if (one-window-p)
      (kill-current-buffer)
    (progn
      (kill-current-buffer)
      (delete-window))))

(global-set-key (kbd "s-k") #'kill-and-close)
(global-set-key (kbd "s-o") #'other-window)
(global-set-key (kbd "s-b") #'switch-to-buffer-other-window)
(global-set-key (kbd "s-1") #'delete-other-windows)
(global-set-key (kbd "s-2") #'split-window-below)
(global-set-key (kbd "s-3") #'split-window-right)
(global-set-key (kbd "s-0") #'delete-window)

(defun cherry/find-files ()
  "Use either find-file or projectile-find-file depending if is on a project"
  (interactive)
  (if (projectile-project-p)
      (projectile-find-file-other-window)
    (find-file-other-window (read-file-name "File: "))))


(global-set-key (kbd "s-f") #'cherry/find-files)

(global-set-key (kbd "C-/") #'undo)
(global-set-key (kbd "C-_") #'redo)

(defun cherry/open-term ()
  "Open eshell"
  (interactive)
  (if (projectile-project-p)
      (projectile-run-eshell)
    (eshell)))

(defun shell-other-window (buffer-name)
  "Open a `shell' in a new window."
  (interactive)
  (let ((buf (cherry/open-term)))
    (switch-to-buffer (other-buffer buf))
    (switch-to-buffer-other-window buf)
    (rename-buffer buffer-name)
    ))

(defun named-eshell ()
  "Open eshell with buffer name"
  (interactive)
  (shell-other-window (read-string "Enter the shell name: ")))

(global-set-key (kbd "C-c t") #'named-eshell)

(provide 'keybindings)
;;; keybindings.el ends here
