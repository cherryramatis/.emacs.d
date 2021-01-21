;;; bufler-package.el --- Bufler package configuration

;;; Commentary:

;; Bufler package configuration

;;; Code:

(require 'bufler)

(global-set-key (kbd "C-x C-b") 'bufler)

(setf bufler-groups
      (bufler-defgroups
	;; Subgroup collecting all named workspaces.
	(group (auto-workspace))
	;; Subgroup collecting buffers in a projectile project.
	(group (auto-projectile))
	;; Grouping browser windows
	(group
	 ;; Subgroup collecting all `help-mode' and `info-mode' buffers.
	 (group-or "Help/Info"
		   (mode-match "*Help*" (rx bos (or "help-" "helpful-")))
		   ;; (mode-match "*Helpful*" (rx bos "helpful-"))
		   (mode-match "*Info*" (rx bos "info-"))))
	(group
	 ;; Subgroup collecting all special buffers (i.e. ones that are not
	 ;; file-backed), except `magit-status-mode' buffers (which are allowed to fall
	 ;; through to other groups, so they end up grouped with their project buffers).
	 (group-and "*Special*"
		    (name-match "**Special**"
				(rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace" "Pinentry") "*"))
		    (lambda (buffer)
		      (unless (or (funcall (mode-match "Magit" (rx bos "magit-status"))
					   buffer)
				  (funcall (mode-match "Dired" (rx bos "dired"))
					   buffer)
				  (funcall (auto-file) buffer))
			"*Special*"))))
	;; Group remaining buffers by major mode.
	(auto-mode)))

(provide 'bufler-package)
;;; bufler-package.el ends here
