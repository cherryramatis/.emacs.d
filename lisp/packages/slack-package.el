;;; slack-package.el --- Slack package configuration

;;; Commentary:

;; Slack package configuration

;;; Code:

(require 'slack)

(setq slack-buffer-emojify t)
(setq slack-prefer-current-team t)

(slack-register-team
  :name "hagens"
  :default t
  :client-id slack-email
  :client-secret slack-pass
  :token slack-token
  :subscribed-channels '(general slackbot))

(global-set-key (kbd "s-s") #'slack-im-select)

(provide 'slack-package)
;;; slack-package.el ends here
