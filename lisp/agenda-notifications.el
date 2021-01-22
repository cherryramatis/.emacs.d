;;; agenda-notifications.el --- Agenda notifications

;;; Commentary:

;; Use alert.el to have notifications for TODO itens on agenda.

;;; Code:

(require 'appt)
(require 'alert)

(defun cherry/appt-display-native (min-to-app new-time msg)
  (alert (format "%s" msg) :title (format "Appointment in %s minutes" min-to-app)))

;; Clear existing appt list
(setq appt-time-msg-list nil)

;; Warn every 5 minutes from t - appt-message-warning-time
(setq appt-display-interval '5)

;; Send first warning 10 minutes before appointment
(setq appt-message-warning-time '10)

;; Don't show in the modeline
(setq appt-display-mode-line nil)

;; Pass warnings to the designed window function
(setq appt-display-format 'window)

(setq appt-disp-window-function (function cherry/appt-display-native))

(org-agenda-to-appt)

;; Update appt list hourly
(run-at-time "24:01" 3600 'org-agenda-to-appt)

;; update appt list on agenda view
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

(provide 'agenda-notifications)
;;; agenda-notifications.el ends here
