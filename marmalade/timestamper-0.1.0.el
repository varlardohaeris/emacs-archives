;;; timestamper.el --- A minor mode for easy timelogging

;; Copyright 2013 Zhao Shenyang

;; Author: Zhao Shenyang
;; Version: 0.1.0

(defcustom timestamper-default-stopwatch nil
  "When the timestamper is enable, should it default to stop watch mode.")

(defcustom timestamper-normal-time-format "[%Y-%m-%d %H:%M:%S] "
  "The timestamp format"
  :type 'string
  :group 'timestamper)

(defcustom timestamper-stopwatch-time-format "<%HS:%MS:%SS %HL:%ML:%SL> "
  "The timestamp format for stopwatch time tag
%HS, %MS and %SS for total hours, minutes and seconds
%HL, %ML and %SL for lap hours, minutes and seconds"
  :type 'string
  :group 'timestamper)

(defvar timestamper--tag-mode 'normal
  "timestamp mode, currently only two mode: normal and stopwatch")

(defvar timestamper--stopwatch-init-time nil)

(defvar timestamper--stopwatch-last-time nil)

(defvar timestamper--stopwatch-time-term-total '("%HS" "%MS" "%SS"))

(defvar timestamper--stopwatch-time-term-lap '("%HL" "%ML" "%SL"))

(defvar timestamper--gen-func-alist
  '((normal . timestamper--gen-normal-time-tag)
    (stopwatch . timestamper--gen-stopwatch-time-tag)))

(defun timestamper--gen-normal-time-tag ()
  "generate normal time tag"
  (format-time-string timestamper-normal-time-format (current-time)))

(defun timestamper--gen-stopwatch-time-tag ()
  "generate stopwatch time tag"
  (let* ((now (float-time))
	 (total-delta (- now timestamper--stopwatch-init-time))
	 (total-hms (timestamper--seconds-to-hms total-delta))
	 (lap-delta (- now timestamper--stopwatch-last-time))
	 (lap-hms (timestamper--seconds-to-hms lap-delta))
	 (first-past (timestamper--stopwatch-replace-terms
		      timestamper-stopwatch-time-format
		      total-hms
		      timestamper--stopwatch-time-term-total))
	 (second-past (timestamper--stopwatch-replace-terms
		       first-past
		       lap-hms
		       timestamper--stopwatch-time-term-lap)))
    second-past))

(defun timestamper--seconds-to-hms (seconds)
  "convert a time interval in seconds to a (hh mm ss) string list"
  (let* ((seconds-i (round seconds))
	 (ss (% seconds-i 60))
	 (rest-m (/ seconds-i 60))
	 (mm (% rest-m 60))
	 (hh (/ rest-m 60)))
      (mapcar (lambda (i) (format "%02d" i)) (list hh mm ss))))

(defun timestamper--stopwatch-replace-terms (template hms-list term-list)
  (if (and hms-list term-list)
      (timestamper--stopwatch-replace-terms
       (replace-regexp-in-string (car term-list)
				 (car hms-list)
				 template)
       (cdr hms-list)
       (cdr term-list))
    template))

(defun timestamper-start-stopwatch ()
  (let ((now (float-time)))
   (setq timestamper--tag-mode 'stopwatch
	 timestamper--stopwatch-init-time now
	 timestamper--stopwatch-last-time now)))

(defun timestamper-stop-stopwatch ()
  (when (eq timestamper--tag-mode 'stopwatch)
    (timestamper-newline)
    (insert "Stop!")
    (setq timestamper--tag-mode 'normal)))

(defun timestamper-toggle-stopwatch-ss ()
  "toggle start/stop stopwatch"
  (interactive)
  (if (eq timestamper--tag-mode 'stopwatch)
      (progn
	(timestamper-stop-stopwatch)
	(message "Stopwatch stopped!"))
    (progn
     (timestamper-start-stopwatch)
     (message "Stopwatch started!"))))

(defun timestamper-newline ()
  (interactive)
  (newline)
  (insert
   (funcall (cdr (assoc timestamper--tag-mode
			timestamper--gen-func-alist))))
  (when (eq timestamper--tag-mode 'stopwatch)
    (setq timestamper--stopwatch-last-time (float-time))))

(define-minor-mode timestamper-mode
  "Time stamp each line!"
  :lighter " ts"
  :global nil
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "RET") 'timestamper-newline)
	    (define-key map (kbd "C-c .") 'timestamper-toggle-stopwatch-ss)
	    map)
  (make-local-variable 'timestamper--tag-mode)
  (make-local-variable 'timestamper--stopwatch-initn-time)
  (make-local-variable 'timestamper--stopwatch-last-time))

(provide 'timestamper)

;;; timestamper.el ends here
