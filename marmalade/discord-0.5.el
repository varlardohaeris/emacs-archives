;;; discord.el --- Discordian dates for calendar

;;; Author: Dave Pearson <davep@davep.org>
;;; Version: 0.5

(eval-when-compile
  (require 'cl))
(require 'calendar)

;;;###cal-autoload
(defun calendar-discordian-print-date ()
  "Show the Discordian date under the cursor"
  (interactive)
  (message "Discordian date: %s"
           (calendar-discordian-date-string (calendar-cursor-to-date t))))

;;;###cal-autoload
(defun* calendar-discordian-date-string (&optional (date (calendar-current-date)))
  "Convert DATE to discordian format."
  (let* ((days      ["Sweetmorn" "Boomtime" "Pungenday" "Prickle-Prickle" "Setting Orange"])
         (months    ["Chaos" "Discord" "Confusion" "Bureaucracy" "Aftermath"])
         (day-count [0 31 59 90 120 151 181 212 243 273 304 334])
         (year      (- (calendar-extract-year date) 1900))
         (month     (1- (calendar-extract-month date)))
         (day       (1- (calendar-extract-day date)))
         (julian    (+ (aref day-count month) day)))
    (if (and (= month 1) (= day 28))
        (format "St. Tib's Day, %d" (+ year 3066))
      (format "%s, Day %d of the season of %s, Anno Mung %d"
              (aref days (mod julian 5))
              (1+ (mod julian 73))
              (aref months (floor (/ julian 73)))
              (+ year 3066)))))

(provide 'discord)
;;; discord.el ends here
