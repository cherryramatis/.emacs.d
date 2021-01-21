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
  :client-id "fabio.ramatis@hagens.com.br"
  :client-secret "A45b84c99@"
  :token "xoxs-188549451731-1300081492802-1636536881728-dad11d8f228cdd8a1785c5f798fd2be36120106bfe42feff17f5964f1717ed69"
  :subscribed-channels '(general slackbot))

(global-set-key (kbd "s-s") #'slack-im-select)

(provide 'slack-package)
;;; slack-package.el ends here
