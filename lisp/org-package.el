;;; org-package.el --- Org mode configuration

;;; Commentary:

;; Org mode configuration

;;; Code:

(require 'org-bullets)
(require 'visual-fill-column)

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(define-key org-mode-map (kbd "M-p") #'outline-previous-visible-heading)
(define-key org-mode-map (kbd "M-n") #'outline-next-visible-heading)
(define-key org-mode-map (kbd "M-h") #'outline-promote)
(define-key org-mode-map (kbd "M-l") #'outline-demote)
(define-key org-mode-map (kbd "M-j") #'org-down-element)
(define-key org-mode-map (kbd "M-k") #'org-up-element)

(global-set-key (kbd "C-c a") #'org-agenda-list)
(global-set-key (kbd "s-a") #'org-agenda-list)

(auto-fill-mode)
(setq-default fill-column 80)

(defun cherry/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(add-hook 'org-mode-hook 'cherry/org-mode-setup)
(setq org-ellipsis " ▾")
(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-agenda-files '("~/Dropbox/org/todo.org"))
(setq org-refile-targets
      '(("archive.org" :maxlevel . 1)))

(setq org-capture-templates
        '(("t" "Todo" entry (file+olp "~/Dropbox/org/todo.org" "Inbox")
           "* TODO %?\nSCHEDULED: %^t\n" :empty-lines 1)
          ("n" "Note" entry (file+olp "~/Dropbox/org/todo.org" "Inbox")
           "* %?" :empty-lines 1)))

(define-key global-map (kbd "C-c oc") #'org-capture)
(define-key global-map (kbd "C-c on") (lambda () (interactive) (find-file-other-window
								"~/Dropbox/org/todo.org")))

;; (global-set-key (kbd "C-c n") (lambda () (interactive) (find-file-other-window "~/Dropbox/org/wiki/index.org")))

(defun cherry/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(add-hook 'org-mode-hook 'cherry/org-mode-visual-fill)

(defun cherry/org-archive-done-tasks ()
  "Archive all DONE subheadings"
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'file))

(provide 'org-package)
;;; org-package.el ends here
