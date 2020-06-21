;;; org-taskforecast.el --- Manage closed task list and forecast time flow with org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Hiroki YAMAKAWA

;; Author:  Hiroki YAMAKAWA <s06139@gmail.com>
;; URL: https://github.com/HKey/org-taskforecast
;; Package-Version: 0.1.0
;; Package-Commit: 1dc75f26c27f752a00731e843c784ab4d10b7b56
;; Keywords: convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (dash "2.16.0") (dash-functional "2.16.0") (s "1.12.0") (org-ql "0.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Time management tool for today's tasks showing them with estimated
;; start/end time.

;; This package is based on the time management method TaskChute
;; (https://taskchute.net/).  However this package implements only a
;; few features, not all of the original TaskChute.

;; Make a list of tasks for today and show the list with todo state,
;; estimated start and end time of task.  You can see what task you're
;; working on, what tasks you've done today with time log and rest
;; tasks for today with estimated start and end time.  And you can
;; also manipulate tasks in the list like org-agenda.

;; This package uses org-id to find an org heading.  So ID property
;; of an org heading is set as needed.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'org)
(require 'org-clock)
(require 'org-element)
(require 'org-id)
(require 'dash)
(require 'dash-functional)
(require 's)
(require 'org-ql)

;;;; Custom

(defgroup org-taskforecast nil
  "Manage closed task list and forecast time flow with org-mode."
  :group 'org
  :prefix "org-taskforecast")

(defcustom org-taskforecast-dailylist-file "~/org-taskforecast/%Y/%Y-%m/%Y-%m-%d.org"
  "A file name which indicates the location to store daily task list.
This string is expanded by `format-time-string'."
  :type 'string
  :group 'org-taskforecast
  :package-version '(org-taskforecast . "0.1.0"))

(defcustom org-taskforecast-day-start 0000
  "A start time of a day.
It is an integer as HHMM.
The value is less than zero, which means yesterday or older.
The value is over than 2359, which means tomorrow or more future.

Example of a range of today:
   0300 => 03:00 ~ 27:00 (03:00 of tomorrow)
  -0100 => 23:00 of yesterday ~ 23:00 of today"
  :type 'integer
  :group 'org-taskforecast
  :package-version '(org-taskforecast . "0.1.0"))

(defcustom org-taskforecast-list-task-link-formatters
  (list #'org-taskforecast-list-tlfmt-default-section-id
        #'org-taskforecast-list-tlfmt-scheduled-time
        #'org-taskforecast-list-tlfmt-start
        #'org-taskforecast-list-tlfmt-end
        #'org-taskforecast-list-tlfmt-effort
        #'org-taskforecast-list-tlfmt-clock
        #'org-taskforecast-list-tlfmt-todo
        #'org-taskforecast-list-tlfmt-title)
  "Function list for formatting a task link.
The results of the functions are joined with \" \" and
empty strings are ignored..
The functions should have no parameter.
The functions are obtained information as global variables below:
- `org-taskforecast-list-info-task-link' is an instance of
  `org-taskforecast--tlink'
- `org-taskforecast-list-info-today' is an encoded time as a date of today
- `org-taskforecast-list-info-now' as an encoded time as the current time
- `org-taskforecast-list-info-task-link-start-end-time' is an instance of
  `org-taskforecast--eclock'
- `org-taskforecast-list-info-sections' is a list of instances of
  `org-taskforecast--section'

Other global variable is also set for formatting:
- `org-taskforecast-day-start'"
  :type '(repeat function)
  :group 'org-taskforecast
  :package-version '(org-taskforecast . "0.1.0"))

(defcustom org-taskforecast-list-section-formatter
  #'org-taskforecast-list-secfmt-section
  "Function for formatting a section in `org-taskforecast-list'.
The function should have no parameter.
The function is obtained information as global variables below:
- `org-taskforecast-list-info-section' is an instance of
  `org-taskforecast--section'
- `org-taskforecast-list-info-today' is an encoded time as a date of today
- `org-taskforecast-list-info-now' as an encoded time as the current time"
  :type 'function
  :group 'org-taskforecast
  :package-version '(org-taskforecast . "0.1.0"))

(defcustom org-taskforecast-enable-interruption t
  "Non-nil means enable interruption representation."
  :type 'boolean
  :group 'org-taskforecast
  :package-version '(org-taskforecast . "0.1.0"))

(defcustom org-taskforecast-sorting-storategy
  (list #'org-taskforecast-ss-time-up)
  "A list of functions to sort task links.
Each function takes two task links like A and B.
The function returns:
- +1  if A > B
- -1  if A < B
- nil if A == B"
  :type '(repeat function)
  :group 'org-taskforecast
  :package-version '(org-taskforecast . "0.1.0"))

(defcustom org-taskforecast-sections
  nil
  "A list of section definitions.
Each element is a list like (ID START-TIME &optional DESCRIPTION).

- ID is a string which must be unique in this variable.
  It is used as an section ID of task's default section.
- START-TIME is start time integer like `org-taskforecast-day-start'.
- DESCRIPTION is a description string of a section.

Example:
    (setq org-taskforecast-sections
          '((\"morning\"   0600)
            (\"daytime-a\" 0900)
            (\"noon\"      1200 \"Lunch and Nap\")
            (\"daytime-b\" 1300)
            (\"evening\"   1700)
            (\"night\"     2000 \"Reading books\")))"
  :type '(repeat list)
  :group 'org-taskforecast
  :package-version '(org-taskforecast . "0.1.0"))

;;;; Compatibility

;; TODO: when dropping support of emacs 26 and older, remove these functions
;;       and use functions that emacs provides.

;; these functions are introduces since emacs 27
(defalias 'org-taskforecast--decoded-time-hour
  (if (fboundp 'decoded-time-hour)
      (symbol-function 'decoded-time-hour)
    (lambda (time) (nth 2 time))))
(gv-define-setter org-taskforecast--decoded-time-hour (val time)
  `(setf (nth 2 ,time) ,val))

(defalias 'org-taskforecast--decoded-time-minute
  (if (fboundp 'decoded-time-minute)
      (symbol-function 'decoded-time-minute)
    (lambda (time) (nth 1 time))))
(gv-define-setter org-taskforecast--decoded-time-minute (val time)
  `(setf (nth 1 ,time) ,val))

(defalias 'org-taskforecast--decoded-time-second
  (if (fboundp 'decoded-time-second)
      (symbol-function 'decoded-time-second)
    (lambda (time) (nth 0 time))))
(gv-define-setter org-taskforecast--decoded-time-second (val time)
  `(setf (nth 0 ,time) ,val))

(defalias 'org-taskforecast--time-equal-p
  (if (fboundp 'time-equal-p)
      (symbol-function 'time-equal-p)
    (lambda (a b)
      (let ((now (current-time)))
        ;; nil means the current time
        (equal (or a now) (or b now))))))

;; since emacs 27, `encode-time' accepts float value as "second" parameter,
;; but emacs 26 and older, the function does not accept that.
(defalias 'org-taskforecast--encode-time
  (if (version<= "27.0.0" emacs-version)
      (symbol-function 'encode-time)
    (lambda (second &rest rest)
      (apply #'encode-time (truncate second) rest))))

;;;; Type

(defun org-taskforecast--encoded-time-p (x)
  "Non-nil means X is an encoded time.
Encoded time is a type of a returned value of `encode-time'."
  (or
   ;; `time-add' possibly returns an integer
   (integerp x)
   (and (listp x)
        (member (length x) '(2 3 4))
        (-all-p #'integerp x))))

(cl-deftype org-taskforecast--encoded-time ()
  '(satisfies org-taskforecast--encoded-time-p))

;;;; Time

(defun org-taskforecast--split-hhmm (hhmm)
  "Split HHMM integer into a list like (HOUR MINUTE)."
  (list (/ hhmm 100)
        (% hhmm 100)))

(defun org-taskforecast--hhmm-to-second (hhmm)
  "Convert HHMM integer into an integer in second counted from 00:00."
  (-let (((hh mm) (org-taskforecast--split-hhmm hhmm)))
    (+ (* 60 60 hh) (* 60 mm))))

(defun org-taskforecast--encode-hhmm (hhmm day)
  "Return an encoded time from HHMM as a time of DAY.
HHMM is an integer like `org-taskforecast-day-start'.
DAY is an encoded time."
  (-let (((hour minute) (org-taskforecast--split-hhmm hhmm))
         (time (decode-time day)))
    (setf (org-taskforecast--decoded-time-hour time) hour
          (org-taskforecast--decoded-time-minute time) minute
          (org-taskforecast--decoded-time-second time) 0)
    (apply #'org-taskforecast--encode-time time)))

(defun org-taskforecast--format-second-to-hhmm (second)
  "Format SECOND to HH:MM style string."
  (let ((second (floor second)))
    (format "%d:%02d" (/ second 3600) (/ (% second 3600) 60))))

(defun org-taskforecast--effort-to-second (effort-str)
  "Convert string of effort property to second.
EFFORT-STR is a string of a value of an effort property.
If EFFORT-STR is invalid, this function returns nil."
  (-let* ((re (rx bos (? (group (+ num)) ":") (group (+ num)) eos))
          (((_ h m)) (s-match-strings-all re effort-str)))
    ;; Effort property sometimes has no colon format like "0".
    (when m
      (+ (* 60 60 (if h (string-to-number h) 0))
         (* 60 (string-to-number m))))))

(defun org-taskforecast--time-as-date (time)
  "Set hour, minute and second of TIME to zero.
TIME is an encoded time.
A returned value is an encoded time."
  (let ((decoded (decode-time time)))
    (setf (org-taskforecast--decoded-time-hour decoded) 0
          (org-taskforecast--decoded-time-minute decoded) 0
          (org-taskforecast--decoded-time-second decoded) 0)
    (apply #'org-taskforecast--encode-time decoded)))

(defun org-taskforecast--date-of-time (time day-start)
  "Get the date of TIME when the day starts at DAY-START.
TIME is an encoded time.
DAY-START is an integer like `org-taskforecast-day-start'.
This function returns an encoded time as a date of today."
  (let* ((decoded (decode-time time))
         (start-time (org-taskforecast--encode-hhmm day-start time))
         (dsec (time-to-seconds (time-subtract time start-time))))
    (setf (org-taskforecast--decoded-time-hour decoded) 0
          (org-taskforecast--decoded-time-minute decoded) 0
          (org-taskforecast--decoded-time-second decoded) dsec)
    (org-taskforecast--time-as-date
     (apply #'org-taskforecast--encode-time decoded))))

(defun org-taskforecast--time-to-hhmm (time today)
  "Convert TIME to hour and minute as time of TODAY.
A returned value is a list like (hour minute)."
  (let* ((today (org-taskforecast--time-as-date today))
         (dsec (floor (time-to-seconds (time-subtract time today))))
         (dmin (/ dsec 60))
         (hour (/ dmin 60))
         (minute (% dmin 60)))
    (cl-assert (<= 0 dsec))
    (list hour minute)))

(defun org-taskforecast--today-p (time today day-start)
  "Return non-nil if TIME is in range of TODAY.
- TIME is an encoded time
- TODAY is an encoded time
- DAY-START is an integer, see `org-taskforecast-day-start'"
  (let ((start (org-taskforecast--encode-hhmm day-start today))
        (end (org-taskforecast--encode-hhmm (+ day-start 2400) today)))
    (and (time-less-p time end)
         (or (org-taskforecast--time-equal-p start time)
             (time-less-p start time)))))

;;;; List

(cl-defun org-taskforecast--sort (seq predicate &key (key #'identity))
  "Like `cl-sort' but do not modify SEQ.
About SEQ, PREDICATE and KEY, see `cl-sort'."
  (cl-sort (copy-sequence seq) predicate :key key))

;;;; Memoize

;; Parsing org-mode text consumes many time.
;; To reduce that time, introduce memoize utility to cache parsing results
;; of a task heading.
;; The cached values are linked to id of org-id of the heading to drop
;; the values when the heading is modified.

(defvar org-taskforecast--memoize-cache nil
  "A hash table for `org-taskforecast--memoize'.
Do not use this variable directory.
Use API functons below:
- `org-taskforecast--memoize-exists-p'
- `org-taskforecast--memoize-get'
- `org-taskforecast--memoize-set'
- `org-taskforecast--memoize-drop'
- `org-taskforecast--memoize-clear'

The value is nil or a hash table created by
`org-taskforecast--memoize-make-cache-table'.
If the value is nil, which means that do not use cache.")

(defun org-taskforecast--memoize-make-cache-table ()
  "Create a hash table for `org-taskforecast--memoize'.
Key is a string as an id of org-id."
  (make-hash-table :test #'equal))

(defmacro org-taskforecast--memoize (id &rest body)
  "Memoize the result of BODY and associates it to ID.
ID is an id of org-id.
The result is stored in `org-taskforecast--memoize-cache'.
If the cached value is not found or `org-taskforecast--memoize-cache' is nil,
this macro evaluates BODY.

This macro generates a unique key to a place where this macro expanded.
The cached value is managed by ID and the unique key.
So the cached value is independent for each expression which uses this macro."
  (declare (debug t) (indent 1))
  (let ((idsym (cl-gensym "id-"))
        (valsym (cl-gensym "value-"))
        ;; When evaluated a function using this macro by `eval-defun',
        ;; the macro is expanded every time in calling the function.
        ;; This macro uses an auto generated unique key to find value
        ;; from cache table and the key is generated by `cl-gensym'.
        ;; In that situation `cl-gensym' is called every time when
        ;; the function called and also the key symbol is re-generated.
        ;; So the cached value never hit.
        ;; To prevent that, use body expression as a key object instead of
        ;; a gensym symbol when the function evaluated by `eval-defun' .
        (key (if (or load-in-progress byte-compile-current-file)
                 (cl-gensym "key-")
               body)))
    `(let ((,idsym ,id))
       (if (org-taskforecast--memoize-exists-p ,idsym ',key)
           (org-taskforecast--memoize-get ,idsym ',key)
         (let ((,valsym (progn ,@body)))
           (org-taskforecast--memoize-set ,idsym ',key ,valsym)
           ,valsym)))))

(defun org-taskforecast--memoize-exists-p (id key)
  "Non-nil means cached value corresponds to ID and KEY is found.
This is an utility function for `org-taskforecast--memoize'.
This function finds the value from `org-taskforecast--memoize-cache'.

- ID is an id of org-id
- KEY is a unique key for an expression.
  See `org-taskforecast--memoize' for more detail."
  (-some--> org-taskforecast--memoize-cache
    (gethash id it)
    (and (assoc key it) t)))

(defun org-taskforecast--memoize-get (id key)
  "Get cached value corresponds to ID and KEY.
When the value is not found, this function returns nil.
This is an utility function for `org-taskforecast--memoize'.
This function finds the value from `org-taskforecast--memoize-cache'.

- ID is an id of org-id
- KEY is a unique key for an expression.
  See `org-taskforecast--memoize' for more detail."
  (-some--> org-taskforecast--memoize-cache
    (gethash id it)
    (assoc key it)
    (cdr it)))

(defun org-taskforecast--memoize-set (id key value)
  "Store VALUE as a value corresponds ID and KEY.
The returned value is undefined.
This is an utility function for `org-taskforecast--memoize'.
This function stores the value in `org-taskforecast--memoize-cache'.

- ID is an id of org-id
- KEY is a unique key for an expression.
  See `org-taskforecast--memoize' for more detail."
  (when org-taskforecast--memoize-cache
    (--> (gethash id org-taskforecast--memoize-cache)
         ;; update alist
         (-if-let (cell (assoc key it))
             (progn (setcdr cell value) it)
           `((,key . ,value) ,@it))
         (puthash id it org-taskforecast--memoize-cache))))

(defun org-taskforecast--memoize-drop (id)
  "Drop all cached values correspond to ID.
The returned value is undefined.
This function drops values from `org-taskforecast--memoize-cache'.

ID is an id of org-id."
  (-some--> org-taskforecast--memoize-cache
    (remhash id it)))

(defun org-taskforecast--memoize-clear ()
  "Clear all cached values.
The returned value is undefined.
This function clears the hash table `org-taskforecast--memoize-cache'."
  (-some--> org-taskforecast--memoize-cache
    (clrhash it)))

;;;; Org-mode

;;;;; Internal utility

(defmacro org-taskforecast--at-id (id &rest body)
  "Eval BODY at a heading of ID.
BODY is evaluated in widened buffer to go to appropriate point of
the buffer whether the buffer narrowed."
  (declare (indent 1) (debug t))
  (let ((id-sym (cl-gensym "id-")))
    `(let ((,id-sym ,id))
       (-if-let ((file . pos) (org-id-find ,id-sym))
           (with-current-buffer (find-file-noselect file)
             (save-excursion
               (save-restriction
                 (widen)
                 (goto-char pos)
                 ;; To parse with org element api properly
                 ;; even when the heading is folded and invisible.
                 (org-show-context)
                 ,@body)))
         (error "The location of org heading is not found, ID: %s" ,id-sym)))))

(defun org-taskforecast--normalize-title (title)
  "Normalize a TITLE of a heading."
  (s-replace-all '(("[" . "{") ("]" . "}"))
                 (org-link-display-format title)))

(defun org-taskforecast--parse-heading-without-subtree ()
  "Parse heading at point without subtree by org element api."
  (save-excursion
    (save-restriction
      (widen)
      (narrow-to-region
       (progn (outline-back-to-heading) (point))
       (progn (outline-next-heading) (point)))
      (org-element-parse-buffer))))

(defun org-taskforecast--get-link-id (str)
  "Get a link id from STR.
STR is a org-id link string like \"[[id:1234][foo]]\".
If STR is not a org-id link string, this function returns nil."
  (let ((re (rx bos "[[id:" (group (+ (not (any "]")))) "]["
                (+ (not (any "]"))) "]]" eos)))
    (-when-let (((_ id)) (s-match-strings-all re str))
      id)))

;;;;; Clock class

(defclass org-taskforecast--clock ()
  ((start
    :initarg :start
    :reader org-taskforecast-clock-start
    :type org-taskforecast--encoded-time
    :documentation
    "A start time of a clock of a task as an encoded time.")
   (end
    :initarg :end
    :reader org-taskforecast-clock-end
    :type (or null org-taskforecast--encoded-time)
    :documentation
    "An end time of a clock of a task as an encoded time."))
  :documentation
  "A clock data.")

(defun org-taskforecast--get-clock-from-element (element)
  "Get a clock from ELEMENT.
ELEMENT is a clock element of org element api."
  (let* ((timestamp (org-element-property :value element))
         (runnigp (eq 'running (org-element-property :status element)))
         (start (org-taskforecast--encode-timestamp-start-time timestamp))
         (end (and (not runnigp)
                   (org-taskforecast--encode-timestamp-end-time timestamp))))
    (org-taskforecast--clock :start start :end end)))

(defun org-taskforecast-clock-duration (clock)
  "Duration of CLOCK as an encoded time.
CLOCK is an instance of `org-taskforecast--clock'."
  ;; When the end of CLOCK is nil, it will be used as the current time
  ;; by `time-subtract'.
  (time-subtract (org-taskforecast-clock-end clock)
                 (org-taskforecast-clock-start clock)))

(defun org-taskforecast-clock-start-less-p (a b)
  "Compare start-times of A and B by `time-less-p'.
A and B are instances of `org-taskforecast--clock'."
  (time-less-p (org-taskforecast-clock-start a)
               (org-taskforecast-clock-start b)))

(cl-defun org-taskforecast--encode-timestamp-start-time (timestamp &optional (hour 0) (minute 0) (second 0))
  "Get an encoded time of the start time of TIMESTAMP.
TIMESTAMP is an element of timestamp of org element api.
HOUR, MINUTE and SECOND are the default values if TIMESTAMP doesn't
have those part."
  (org-taskforecast--encode-time
   second
   (or (org-element-property :minute-start timestamp) minute)
   (or (org-element-property :hour-start timestamp) hour)
   (org-element-property :day-start timestamp)
   (org-element-property :month-start timestamp)
   (org-element-property :year-start timestamp)))

(defun org-taskforecast--encode-timestamp-end-time (timestamp)
  "Get an encoded time of the end time of TIMESTAMP.
TIMESTAMP is an element of timestamp of org element api.
The second part of a returned time is set to zero.
If hour and minute part do not exist, they are set to zero."
  (org-taskforecast--encode-time
   0
   (or (org-element-property :minute-end timestamp) 0)
   (or (org-element-property :hour-end timestamp) 0)
   (org-element-property :day-end timestamp)
   (org-element-property :month-end timestamp)
   (org-element-property :year-end timestamp)))

(defun org-taskforecast--get-clocks-in-range (clocks start end)
  "Get clocks between START and END from CLOCKS.
- CLOCKS is a list of instances of `org-taskforecast--clock'
- START is an encoded time
- END is an encoded time"
  (--filter
   (let ((cstart (org-taskforecast-clock-start it)))
     (and (not (time-less-p cstart start))
          (time-less-p cstart end)))
   clocks))

;;;;; Timestamp class

(defclass org-taskforecast--timestamp ()
  ((ts-list
    :initarg :ts-list
    :type list
    :documentation
    "Timestamp object of org element api"))
  :documentation
  "A timestamp.")

(defun org-taskforecast--get-timestamp-from-timestamp (timestamp)
  "Get a timestamp information from TIMESTAMP.
TIMESTAMP is a timestamp element of org element api.
This function returns an instance of `org-taskforecast--timestamp'."
  (org-taskforecast--timestamp
   :ts-list timestamp))

(cl-defun org-taskforecast-timestamp-start-time (timestamp &optional (hour 0) (minute 0) (second 0))
  "An encoded time of the start time of TIMESTAMP.
TIMESTAMP is an instance of `org-taskforecast--timestamp'.
HOUR, MINUTE and SECOND are the default values if TIMESTAMP doesn't
have those part."
  (with-slots (ts-list) timestamp
    (org-taskforecast--encode-timestamp-start-time
     ts-list hour minute second)))

(defun org-taskforecast-timestamp-start-date-only-p (timestamp)
  "Non-nil means start time of TIMESTAMP has no hour and minute sections.
TIMESTAMP is an instance of `org-taskforecast--timestamp'."
  (with-slots (ts-list) timestamp
    (not (or (org-element-property :hour-start ts-list)
             (org-element-property :minute-start ts-list)))))

(defun org-taskforecast-timestamp-repeat-p (timestamp)
  "Non-nil means the TIMESTAMP has a repeater.
TIMESTAMP is an instance of `org-taskforecast--timestamp'."
  (with-slots (ts-list) timestamp
    (and (org-element-property :repeater-type ts-list) t)))

(defun org-taskforecast-timestamp-start-date (timestamp day-start)
  "Get the date of the start time of TIMESTAMP when the day starts at DAY-START.
If the start time of TIMESTAMP has no hour and minute sections, this function
ignores DAY-START.

- TIMESTAMP is an instance of `org-taskforecast--timestamp'
- DAY-START is an integer, see `org-taskforecast-day-start'"
  (let ((day-start
         (if (org-taskforecast-timestamp-start-date-only-p timestamp)
             ;; do not consider the day start time
             0000
           day-start)))
    (org-taskforecast--date-of-time
     (org-taskforecast-timestamp-start-time timestamp)
     day-start)))

(defun org-taskforecast-timestamp-start-earlier-p (a b day-start)
  "Non-nil if the start time of A is earlier than one of B.
- A and B is an instance of `org-taskforecast--timestamp'
- DAY-START is an integer, see `org-taskforecast-day-start'"
  (let ((a-date (org-taskforecast-timestamp-start-date a day-start))
        (b-date (org-taskforecast-timestamp-start-date b day-start))
        (a-date-only-p (org-taskforecast-timestamp-start-date-only-p a))
        (b-date-only-p (org-taskforecast-timestamp-start-date-only-p b))
        (a-time (org-taskforecast-timestamp-start-time a))
        (b-time (org-taskforecast-timestamp-start-time b)))
    (or
     ;; compare date
     (time-less-p a-date b-date)
     ;; for same date
     (and (org-taskforecast--time-equal-p a-date b-date)
          (or
           ;; compare date-only-p
           (and (not a-date-only-p) b-date-only-p)
           ;; compare time
           (and (not a-date-only-p)
                (not b-date-only-p)
                (time-less-p a-time b-time)))))))

(defun org-taskforecast-timestamp-start-equal-p (a b)
  "Non-nil if the start time of A equals to one of B.
- A and B is an instance of `org-taskforecast--timestamp'
- DAY-START is an integer, see `org-taskforecast-day-start'"
  (let ((a-date-only-p (org-taskforecast-timestamp-start-date-only-p a))
        (b-date-only-p (org-taskforecast-timestamp-start-date-only-p b))
        (a-time (org-taskforecast-timestamp-start-time a))
        (b-time (org-taskforecast-timestamp-start-time b)))
    (and (eq a-date-only-p b-date-only-p)
         (org-taskforecast--time-equal-p a-time b-time))))

;;;;; Task class

(defconst org-taskforecast--task-default-section-id-prop-name
  "ORG_TASKFORECAST_TASK_DEFAULT_SECTION_ID"
  "Property name of a default section ID of a task.")

(defclass org-taskforecast--task ()
  ((id
    :initarg :id
    :reader org-taskforecast-task-id
    :type string
    :documentation
    "An ID of org-id.")
   (title
    :initarg :title
    :reader org-taskforecast-task-title
    :type string
    :documentation
    "A heading title.")
   (effort
    :initarg :effort
    :reader org-taskforecast-task-effort
    :type (or null string)
    :documentation
    "A value of Effort property.")
   (todo-type
    :initarg :todo-type
    :reader org-taskforecast-task-todo-type
    :type (or null symbol)
    :documentation
    "A type of todo as a symbol of todo or done.")
   (scheduled
    :initarg :scheduled
    :reader org-taskforecast-task-scheduled
    :type (or null org-taskforecast--timestamp)
    :documentation
    "A schedule infomaton.")
   (deadline
    :initarg :deadline
    :reader org-taskforecast-task-deadline
    :type (or null org-taskforecast--timestamp)
    :documentation
    "A deadline information.")
   (default-section-id
     :initarg :default-section-id
     :reader org-taskforecast-task-default-section-id
     :type (or null string)
     :documentation
     "A default section ID string."))
  :documentation
  "A task heading data.")

(defun org-taskforecast--get-task ()
  "Get a task at the current point.
A returned value is an instance of `org-taskforecast--task'.
If the heading is not a task this function returns nil."
  (save-excursion
    ;; go to heading line for `org-element-at-point' to get a headline element
    (org-back-to-heading)
    ;; task must have todo state
    (-when-let* ((element (org-element-at-point))
                 (todo-type (org-element-property :todo-type element)))
      (let ((id (org-id-get-create))
            (title (substring-no-properties
                    (org-element-property :title element)))
            (effort (org-entry-get nil org-effort-property))
            (scheduled (-some--> (org-element-property :scheduled element)
                         (org-taskforecast--get-timestamp-from-timestamp it)))
            (deadline (-some--> (org-element-property :deadline element)
                        (org-taskforecast--get-timestamp-from-timestamp it)))
            (default-section-id (org-entry-get
                                 nil
                                 org-taskforecast--task-default-section-id-prop-name)))
        (org-taskforecast--task
         :id id
         :title title
         :effort effort
         :todo-type todo-type
         :scheduled scheduled
         :deadline deadline
         :default-section-id default-section-id)))))

(defun org-taskforecast--get-task-by-id (id)
  "Get a task by ID.
A returned value is an instance of `org-taskforecast--task'.
If the heading of ID is not a task, this function throws an error."
  (--> (org-taskforecast--memoize id
         (org-taskforecast--at-id id
           (org-taskforecast--get-task)))
       (if (null it)
           (error "Not a task headnig, ID: %s" id)
         it)))

(defun org-taskforecast--is-task-id (id)
  "Non-nil means ID is an ID of a task heading."
  (and (org-taskforecast--memoize id
         (org-taskforecast--at-id id
           (org-taskforecast--get-task)))
       t))

(defun org-taskforecast-task-clocks (task)
  "A list of clock data of TASK.
Each element is an instance of `org-taskforecast--clock'."
  (let ((id (org-taskforecast-task-id task)))
    (org-taskforecast--memoize id
      (org-taskforecast--at-id id
        (--> (org-taskforecast--parse-heading-without-subtree)
             (org-element-map it 'clock
               #'org-taskforecast--get-clock-from-element))))))

(defun org-taskforecast-task-repeat-p (task)
  "Non-nil means TASK is a repeat task.
TASK is an instance of `org-taskforecast--task'."
  (org-taskforecast--memoize (org-taskforecast-task-id task)
    (or (-some--> (org-taskforecast-task-scheduled task)
          (org-taskforecast-timestamp-repeat-p it))
        (-some--> (org-taskforecast-task-deadline task)
          (org-taskforecast-timestamp-repeat-p it)))))

(defun org-taskforecast-task-last-repeat (task)
  "Get the value of LAST_REPEAT of TASK.
This function returns an encoded time.
If TASK has no property, this function returns nil."
  (let ((id (org-taskforecast-task-id task)))
    (org-taskforecast--memoize id
      (org-taskforecast--at-id id
        (-some--> (org-entry-get nil "LAST_REPEAT")
          (org-parse-time-string it)
          (apply #'org-taskforecast--encode-time it))))))

(defun org-taskforecast-task-todo-state-for-today (task date day-start)
  "Get todo state of TASK for today.
This function returns a symbol, todo or done.
- TASK is an instance of `org-taskforecast--task'
- DATE is an encoded time as a date of today
- DAY-START is an integer like `org-taskforecast-day-start'"
  (let* ((todo-type (org-taskforecast-task-todo-type task))
         (scheduled (org-taskforecast-task-scheduled task))
         (deadline (org-taskforecast-task-deadline task))
         (stime (when scheduled
                  (let ((time (org-taskforecast-timestamp-start-time
                               scheduled)))
                    (if (org-taskforecast-timestamp-start-date-only-p
                         scheduled)
                        (org-taskforecast--encode-hhmm day-start time)
                      time))))
         (dtime (when deadline
                  (let ((time (org-taskforecast-timestamp-start-time
                               deadline)))
                    (if (org-taskforecast-timestamp-start-date-only-p
                         deadline)
                        (org-taskforecast--encode-hhmm day-start time)
                      time))))
         (repeatp (org-taskforecast-task-repeat-p task))
         (last-repeat (org-taskforecast-task-last-repeat task))
         (times (-non-nil (list (and scheduled stime) (and deadline dtime))))
         (today-start
          (org-taskforecast--encode-hhmm day-start date))
         (next-day-start
          (org-taskforecast--encode-hhmm (+ day-start 2400) date)))
    (unless todo-type
      (error "Task is not a todo heading"))
    (cond ((eq todo-type 'done) 'done)
          ;; todo-type is 'todo
          ((not repeatp) 'todo)
          ;; repeat task
          ((--some (time-less-p it next-day-start) times) 'todo)
          ;; next-day-start =< scheduled/deadline
          ((not last-repeat) 'todo)
          ((time-less-p last-repeat today-start) 'todo)
          ;; day-start =< last-repeat
          (t 'done))))

;;;;; Eclock class (entry clock)

(defclass org-taskforecast--eclock ()
  ((start
    :initarg :start
    :reader org-taskforecast-eclock-start
    :type org-taskforecast--encoded-time
    :documentation
    "an encoded time that indicates the start time of the task of today.
If the start time is not found, the value will be an estimated time.")
   (end
    :initarg :end
    :reader org-taskforecast-eclock-end
    :type org-taskforecast--encoded-time
    :documentation
    "An encoded time that indicates the end time of the task of today.
If the end time is not found, the value will be an estimated time.")
   (start-estimated-p
    :initarg :start-estimated-p
    :reader org-taskforecast-eclock-start-estimated-p
    :type boolean
    :documentation
    "Non-nil means the start time is estimated.")
   (end-estimated-p
    :initarg :end-estimated-p
    :reader org-taskforecast-eclock-end-estimated-p
    :type boolean
    :documentation
    "Non-nil means the end time is estimated.")
   (overrunp
    :initarg :overrunp
    :reader org-taskforecast-eclock-overrun-p
    :type boolean
    :documentation
    "Non-nil means the end time is over the time of the start time plus effort."))
  :documentation
  "An information of start and end time of a task link.")

;;;;; Entry interface

;; Entry interface is the interface for items shown in
;; `org-taskforecast-list' buffer.

(cl-defgeneric org-taskforecast-entry-title (entry)
  "Title of ENTRY.")

(cl-defgeneric org-taskforecast-entry-id (entry)
  "An org-id ID of ENTRY's heading.")

(cl-defgeneric org-taskforecast-entry-effective-effort (entry date day-start)
  "Get effort value of ENTRY.
A returned value is an effort second.

- DATE is an encoded time as a date of today
- DAY-START is an integer like `org-taskforecast-day-start'")

(cl-defgeneric org-taskforecast-entry-effective-clocks (entry date day-start)
  "Effective clocks of ENTRY.
An effective clock is a clock information that clocked today.
If the entry has effective start/end time, an effective clock satisfies
the following conditions:
- the clock was started after the effective start time of ENTRY
- the clock was ended before the effective end time of ENTRY

- DATE is an encoded time as a date of today
- DAY-START is an integer like `org-taskforecast-day-start'")

(cl-defgeneric org-taskforecast-entry-todo-state-for-today (entry date day-start)
  "Get todo state of ENTRY for today.
This function returns a symbol, todo or done.
- DATE is an encoded time as a date of today
- DAY-START is an integer like `org-taskforecast-day-start'")

(cl-defgeneric org-taskforecast-entry-scheduled (entry)
  "Schedule information of ENTRY.
If ENTRY has scheduled, this returns an instance of
`org-taskforecast--timestamp'.
If not, this returns nil.")

(cl-defgeneric org-taskforecast-entry-deadline (entry)
  "Deadline information of ENTRY.
If ENTRY has deadline, this returns an instance of
`org-taskforecast--timestamp'.
If not, this returns nil.")

(cl-defgeneric org-taskforecast-entry-effective-start-time (entry)
  "An encoded time when ENTRY is effective after.
If ENTRY has no effective start time, this returns nil.")

(cl-defgeneric org-taskforecast-entry-effective-end-time (entry)
  "An encoded time when ENTRY is effective before.
If ENTRY has no effective end time, this returns nil.")

(cl-defgeneric org-taskforecast-entry-default-section-id (entry)
  "Default section ID of ENTRY.
If ENTRY has default section, this returns the section ID string.
If not, this returns nil.")

(cl-defgeneric org-taskforecast-entry-is-task-link (_entry)
  "Non-nil if ENTRY is an instance of `org-taskforecast--tlink'."
  nil)

(cl-defgeneric org-taskforecast-entry-is-section (_entry)
  "Non-nil if ENTRY is an instance of `org-taskforecast--section'."
  nil)

(defun org-taskforecast-entry-has-effective-clock (entry date day-start)
  "Non-nil means ENTRY has some effective clocks.
See `org-taskforecast-entry-effective-clocks' about effective clock.

- DATE is an encoded time as a date of today
- DAY-START is an integer like `org-taskforecast-day-start'"
  (not (null (org-taskforecast-entry-effective-clocks entry date day-start))))

(defun org-taskforecast-entry-early-planning (entry day-start)
  "A timestamp of earlier of scheduled and deadline of ENTRY.
This function returns nil if ENTRY has no scheduled and deadline.
DAY-START is an integer, see `org-taskforecast-day-start'."
  (-some--> (list (org-taskforecast-entry-scheduled entry)
                  (org-taskforecast-entry-deadline entry))
    (-non-nil it)
    (-min-by (-flip (-rpartial #'org-taskforecast-timestamp-start-earlier-p
                               day-start))
             it)))

(defun org-taskforecast-entry-derive-default-section (entry sections date day-start)
  "Derive default section from SECTIONS with ENTRY's scheduled and deadline.
This function tries to derive a default section using timestamp obtained by
`org-taskforecast-entry-early-planning'.
If a latest section whose start time is less than or equal to the timestamp
above is found, the section is the derived section.
If the timestamp has no hour and minute section, this function doesn't derive
the default section.

If the derived section is found, this function returns it.
If not, this function returns nil.

- ENTRY is an entry instance
- SECTIONS is a list of instances of `org-taskforecast--section'
- DATE is an encoded time as a date of today
- DAY-START is an integer, see `org-taskforecast-day-start'"
  (-when-let* ((planning
                (org-taskforecast-entry-early-planning entry day-start))
               (planning-time
                (and
                 (not (org-taskforecast-timestamp-start-date-only-p planning))
                 (org-taskforecast-timestamp-start-time planning)))
               ;; when the entry is a repeat task and it has already been done
               ;; on the date, derive the default section on the future day.
               (date
                (if (and (org-taskforecast-entry-is-task-link entry)
                         (org-taskforecast-task-repeat-p
                          (org-taskforecast-tlink-task entry))
                         (eq (org-taskforecast-entry-todo-state-for-today
                              entry date day-start)
                             'done))
                    (org-taskforecast-timestamp-start-date planning day-start)
                  date)))
    (-some--> (org-taskforecast--sort
               sections #'> :key #'org-taskforecast-section-start-time)
      (--first (let ((st (org-taskforecast--encode-hhmm
                          (org-taskforecast-section-start-time it)
                          date)))
                 (or (time-less-p st planning-time)
                     (org-taskforecast--time-equal-p st planning-time)))
               it))))

;;;;; Tlink class

(defconst org-taskforecast--task-link-effective-start-time-prop-name
  "ORG_TASKFORECAST_TASK_LINK_EFFECTIVE_START_TIME"
  "Property name of an effective start time of a task link.")

(defconst org-taskforecast--task-link-effective-end-time-prop-name
  "ORG_TASKFORECAST_TASK_LINK_EFFECTIVE_END_TIME"
  "Property name of an effective end time of a task link.")

(defun org-taskforecast--get-task-link-effective-start-time ()
  "Get the task link's effective start time property from a heading.
A returned value is an encoded time."
  (-some-->
      (org-entry-get
       nil
       org-taskforecast--task-link-effective-start-time-prop-name)
    (org-parse-time-string it)
    (apply #'org-taskforecast--encode-time it)))

(defun org-taskforecast--get-task-link-effective-end-time ()
  "Get the task link's effective end time property from a heading.
A returned value is an encoded time."
  (-some-->
      (org-entry-get
       nil
       org-taskforecast--task-link-effective-end-time-prop-name)
    (org-parse-time-string it)
    (apply #'org-taskforecast--encode-time it)))

(defun org-taskforecast--set-task-link-effective-start-time (time)
  "Set the task link's effective start time property to TIME.
TIME is an encoded time."
  (org-entry-put nil
                 org-taskforecast--task-link-effective-start-time-prop-name
                 (format-time-string (org-time-stamp-format t t) time)))

(defun org-taskforecast--set-task-link-effective-end-time (time)
  "Set the task link's effective end time property to TIME.
TIME is an encoded time."
  (org-entry-put nil
                 org-taskforecast--task-link-effective-end-time-prop-name
                 (format-time-string (org-time-stamp-format t t) time)))

(defclass org-taskforecast--tlink ()
  ((id
    :initarg :id
    :reader org-taskforecast-tlink-id
    :type string
    :documentation
    "An ID of org-id.")
   (task-id
    :initarg :task-id
    :reader org-taskforecast-tlink-task-id
    :type string
    :documentation
    "An ID of a task where this links to.")
   (effective-start-time
    :initarg :effective-start-time
    :reader org-taskforecast-entry-effective-start-time
    :type (or null org-taskforecast--encoded-time)
    :documentation
    "An encoded time when the task link is effective after.")
   (effective-end-time
    :initarg :effective-end-time
    :reader org-taskforecast-entry-effective-end-time
    :type (or null org-taskforecast--encoded-time)
    :documentation
    "An encoded time when the task link is effective before."))
  :documentation
  "A task link data.")

(cl-defmethod org-taskforecast-entry-title ((task-link org-taskforecast--tlink))
  (org-taskforecast-task-title
   (org-taskforecast--get-task-by-id
    (org-taskforecast-tlink-task-id task-link))))

(cl-defmethod org-taskforecast-entry-id ((task-link org-taskforecast--tlink))
  (org-taskforecast-tlink-id task-link))

(cl-defmethod org-taskforecast-entry-todo-state-for-today ((task-link org-taskforecast--tlink) date day-start)
  (let ((task
         (org-taskforecast--get-task-by-id
          (org-taskforecast-tlink-task-id task-link)))
        (effective-end-time
         (org-taskforecast-entry-effective-end-time task-link)))
    (if effective-end-time
        ;; interrupted if efective-end-time exists
        'done
      (org-taskforecast-task-todo-state-for-today task date day-start))))

(cl-defmethod org-taskforecast-entry-scheduled ((task-link org-taskforecast--tlink))
  (org-taskforecast-task-scheduled
   (org-taskforecast--get-task-by-id
    (org-taskforecast-tlink-task-id task-link))))

(cl-defmethod org-taskforecast-entry-deadline ((task-link org-taskforecast--tlink))
  (org-taskforecast-task-deadline
   (org-taskforecast--get-task-by-id
    (org-taskforecast-tlink-task-id task-link))))

(defun org-taskforecast-tlink-task (task-link)
  "Get task linked from TASK-LINK."
  (org-taskforecast--get-task-by-id
   (org-taskforecast-tlink-task-id task-link)))

(defun org-taskforecast--tlink-start-end-time (task-link date day-start &optional start-after now)
  "Get the start and end time of a TASK-LINK.
This function returns an instance of `org-taskforecast--eclock'.

- TASK-LINK is an instance of `org-taskforecast--tlink'
- DATE is an encoded time as the date of today
- DAY-START is an integer, see `org-taskforecast-day-start'
- START-AFTER is an encoded time (optional).
  If it is set, ignore clocks whose start time is earlier than it.
- NOW is an encoded time (optional).
  If it is set, use it instead of an estimated time of start or end
  when the estimated time is earlier than it."
  (let* ((day-start-time
          (org-taskforecast--encode-hhmm day-start date))
         (next-day-start-time
          (org-taskforecast--encode-hhmm (+ day-start 2400) date))
         (effective-start-time
          (org-taskforecast-entry-effective-start-time task-link))
         (effective-end-time
          (org-taskforecast-entry-effective-end-time task-link))
         (todo
          (org-taskforecast-entry-todo-state-for-today
           task-link date day-start))
         (task
          (org-taskforecast--get-task-by-id
           (org-taskforecast-tlink-task-id task-link)))
         (effort-sec
          (or (org-taskforecast-entry-effective-effort task-link date day-start)
              0))
         (clock-start-greater-p
          (-flip #'org-taskforecast-clock-start-less-p))
         (time-greater-p
          (-flip #'time-less-p))
         (start-after
          (-max-by time-greater-p
                   (-non-nil
                    (list start-after day-start-time effective-start-time))))
         (end-before
          (-min-by time-greater-p
                   (-non-nil
                    (list next-day-start-time effective-end-time))))
         (target-clocks
          (org-taskforecast--get-clocks-in-range
           (org-taskforecast-task-clocks task)
           start-after
           end-before))
         (start-time
          (-some--> target-clocks
            (-min-by clock-start-greater-p it)
            (org-taskforecast-clock-start it)))
         (end-time
          (-some--> target-clocks
            (-max-by clock-start-greater-p it)
            (org-taskforecast-clock-end it)))
         (start-estimated-p
          (null start-time))
         (end-estimated-p
          (or (null end-time) (eq todo 'todo)))
         (start
          (cond ((and start-estimated-p (eq todo 'todo) now)
                 (-max-by time-greater-p (list start-after now)))
                (start-estimated-p start-after)
                (t start-time)))
         (start-plus-effort
          (time-add start (seconds-to-time effort-sec)))
         (end
          (cond ((and end-estimated-p (eq todo 'todo) now)
                 (-max-by time-greater-p (list start-plus-effort now)))
                ((and end-estimated-p (eq todo 'done)) start)
                (t end-time)))
         (overrunp
          (time-less-p start-plus-effort end)))
    (org-taskforecast--eclock
     :start start
     :end end
     :start-estimated-p start-estimated-p
     :end-estimated-p end-estimated-p
     :overrunp overrunp)))

(defun org-taskforecast--get-task-link ()
  "Get a task link at the current point.
A returned value is an instance of `org-taskforecast--tlink'.
If the heading is not a task link, this function returns nil."
  (save-excursion
    ;; Prevent error when there is no heading in a buffer.
    (unless (org-before-first-heading-p)
      ;; go to heading line for `org-element-at-point' to get a headline element
      (org-back-to-heading))
    (let* ((element (org-element-at-point))
           (title (org-element-property :title element))
           (task-id (org-taskforecast--get-link-id title))
           (effective-start-time
            (org-taskforecast--get-task-link-effective-start-time))
           (effective-end-time
            (org-taskforecast--get-task-link-effective-end-time)))
      ;; Ignore if the linked heading is not a task.
      (when (and task-id (org-taskforecast--is-task-id task-id))
        (org-taskforecast--tlink
         :id (org-id-get-create) ; Create id when this heading is a task link.
         :task-id task-id
         :effective-start-time effective-start-time
         :effective-end-time effective-end-time)))))

(defun org-taskforecast--get-task-link-by-id (id)
  "Get a task link by ID.
A returned value is an instance of `org-taskforecast--tlink'.
If the heading of ID is not a task link, this function throws an error."
  (--> (org-taskforecast--at-id id
         (org-taskforecast--get-task-link))
       (if (null it)
           (error "Not a task link headnig, ID: %s" id)
         it)))

(cl-defmethod org-taskforecast-entry-effective-clocks ((task-link org-taskforecast--tlink) date day-start)
  (let* ((task-id
          (org-taskforecast-tlink-task-id task-link))
         (effective-start-time
          (org-taskforecast-entry-effective-start-time task-link))
         (effective-end-time
          (org-taskforecast-entry-effective-end-time task-link))
         (clocks
          (org-taskforecast-task-clocks
           (org-taskforecast--get-task-by-id task-id)))
         (today-start (org-taskforecast--encode-hhmm day-start date))
         (next-day-start (org-taskforecast--encode-hhmm
                          (+ day-start 2400) date)))
    (--filter
     (let ((start (org-taskforecast-clock-start it)))
       (and (not (time-less-p start today-start))
            (time-less-p start next-day-start)
            (or (null effective-start-time)
                (not (time-less-p start effective-start-time)))
            (or (null effective-end-time)
                (time-less-p start effective-end-time))))
     clocks)))

(cl-defmethod org-taskforecast-entry-effective-effort ((task-link org-taskforecast--tlink) date day-start)
  (let* ((effective-start-time
          (org-taskforecast-entry-effective-start-time task-link))
         (effective-end-time
          (org-taskforecast-entry-effective-end-time task-link))
         (task
          (org-taskforecast--get-task-by-id
           (org-taskforecast-tlink-task-id task-link)))
         (clocks
          (org-taskforecast-task-clocks task))
         (effort-sec
          (org-taskforecast--effort-to-second
           (org-taskforecast-task-effort task)))
         (time-greater-p
          (-flip #'time-less-p))
         (today-start
          (org-taskforecast--encode-hhmm day-start date))
         (range-start
          (-max-by time-greater-p
                   (-non-nil (list today-start effective-start-time))))
         (range-end
          (-min-by time-greater-p
                   (-non-nil
                    (list effective-end-time
                          (org-taskforecast--encode-hhmm (+ day-start 2400)
                                                         date)))))
         (dsec
          (-compose #'time-to-seconds #'org-taskforecast-clock-duration))
         (used-sec-before
          (--> (org-taskforecast--get-clocks-in-range
                clocks today-start range-start)
               (-map dsec it)
               (-sum it)))
         (remaining-effort-sec
          (when effort-sec (max (- effort-sec used-sec-before) 0)))
         (used-sec
          (--> (org-taskforecast--get-clocks-in-range
                clocks range-start range-end)
               (-map dsec it)
               (-sum it))))
    (-some-->
        (cond ((null effort-sec) nil)
              (effective-end-time (min remaining-effort-sec used-sec))
              (t remaining-effort-sec))
      (floor it))))

(cl-defmethod org-taskforecast-entry-default-section-id ((task-link org-taskforecast--tlink))
  (--> (org-taskforecast-tlink-task task-link)
       (org-taskforecast-task-default-section-id it)))

(cl-defmethod org-taskforecast-entry-is-task-link ((_task-link org-taskforecast--tlink))
  t)

;;;;; Section class

(defclass org-taskforecast--section ()
  ((id
    :initarg :id
    :reader org-taskforecast-section-id
    :type string
    :documentation
    "An ID of org-id")
   (section-id
    :initarg :section-id
    :reader org-taskforecast-section-section-id
    :type string
    :documentation
    "A section ID string")
   (start-time
    :initarg :start-time
    :reader org-taskforecast-section-start-time
    :type integer
    :documentation
    "An integer of start time line `org-taskforecast-day-start'")
   (description
    :initarg :description
    :reader org-taskforecast-section-description
    :type string
    :documentation
    "Description")
   (effort
    :initarg :effort
    :reader org-taskforecast-section-effort
    :type integer
    :documentation
    "An integer of effort seconds.")
   (entries
    :initarg :entries
    :reader org-taskforecast-section-entries
    :type list
    :documentation
    "Entries")))

(cl-defmethod org-taskforecast-entry-title ((section org-taskforecast--section))
  (org-taskforecast-section-description section))

(cl-defmethod org-taskforecast-entry-id ((section org-taskforecast--section))
  (org-taskforecast-section-id section))

(cl-defmethod org-taskforecast-entry-effective-effort ((section org-taskforecast--section) date day-start)
  (--> (org-taskforecast-section-entries section)
       (--map (org-taskforecast-entry-effective-effort it date day-start) it)
       (-reduce-from #'+ 0 it)))

(cl-defmethod org-taskforecast-entry-effective-clocks ((section org-taskforecast--section) date day-start)
  (--> (org-taskforecast-section-entries section)
       (--map (org-taskforecast-entry-effective-clocks it date day-start) it)
       (apply #'append it)))

(cl-defmethod org-taskforecast-entry-todo-state-for-today ((_section org-taskforecast--section) _date _day-start)
  (error "Do not call `org-taskforecast-entry-todo-state-for-today' for `org-taskforecast--section'"))

(cl-defmethod org-taskforecast-entry-scheduled ((_section org-taskforecast--section))
  nil)

(cl-defmethod org-taskforecast-entry-deadline ((_section org-taskforecast--section))
  nil)

(cl-defmethod org-taskforecast-entry-effective-start-time ((_section org-taskforecast--section))
  ;; section should have no effective start time
  nil)

(cl-defmethod org-taskforecast-entry-effective-end-time ((_section org-taskforecast--section))
  ;; section should have no effective end time
  nil)

(cl-defmethod org-taskforecast-entry-default-section-id ((section org-taskforecast--section))
  (org-taskforecast-section-section-id section))

(cl-defmethod org-taskforecast-entry-is-section ((_section org-taskforecast--section))
  t)

;;;; File

;;;;; Internal utility

(defun org-taskforecast-get-dailylist-file (today)
  "Get the path of today's daily task list file for TODAY.
This function depends on:
- `org-taskforecast-dailylist-file' as a file format
- `org-taskforecast-day-start' to determine the date of today"
  (expand-file-name
   (format-time-string org-taskforecast-dailylist-file today)))

(defmacro org-taskforecast--with-existing-file (file &rest body)
  "Eval BODY in a buffer of FILE.
If FILE does not exist, save the buffer of FILE first.
The reason of that is to find org heading by org-id.
org-id searches for existing files only."
  (declare (debug t) (indent 1))
  (let ((file-sym (cl-gensym "file-")))
    `(let ((,file-sym ,file))
       (with-current-buffer (find-file-noselect ,file-sym)
         (unless (file-exists-p ,file-sym)
           (save-buffer))
         ,@body))))

(defun org-taskforecast--map-headings (fn)
  "Call FN on headings in the current buffer.
This function returns a list of results of FN.

Why use this function instead of `org-map-entries' is to avoid asking
about non-existent agenda file by `org-check-agenda-file' when
the file of the current buffer doesn't exist."
  (let* ((results nil)
         (f (lambda ()
              ;; The current point is not beginning of line when
              ;; a heading is the first heading on region.
              (org-back-to-heading t)
              (push (funcall fn) results))))
    (org-map-region f (point-min) (point-max))
    (nreverse results)))

(defun org-taskforecast--cut-heading-by-id (id)
  "Cut a heading by ID.
Return a string of the heading.
When this function failed, returns nil."
  (org-taskforecast--at-id id
    (save-excursion
      (-when-let* ((helement (org-element-at-point))
                   (begin (org-element-property :begin helement))
                   (end (org-element-property :end helement)))
        (prog1
            (buffer-substring begin end)
          (delete-region begin end))))))

;;;;; Entry

(defun org-taskforecast--set-section-slots (entries day-start)
  "Set entries and effort slot of `org-taskforecast--section' in ENTRIES.
This function updates the section instances in ENTRIES and returns ENTRIES.

DAY-START is an integer like `org-taskforecast-day-start'."
  (--> entries
       (-non-nil it)
       ;; drop non-section task links
       (-drop-while (-not #'org-taskforecast-entry-is-section) it)
       (-partition-before-pred #'org-taskforecast-entry-is-section it)
       ;; set entries slot
       (--map
        (-let (((section . entries) it))
          (setf (slot-value section 'entries) entries)
          section)
        it)
       ;; set effort slot
       ;; NOTE: leap second is not considered
       (let ((next-start (+ (* 60 60 24)
                            (org-taskforecast--hhmm-to-second day-start))))
         (--each-r it
           (let* ((s (org-taskforecast-section-start-time it))
                  (this-start (org-taskforecast--hhmm-to-second s))
                  (effort (max (- next-start this-start) 0)))
             (setf (slot-value it 'effort) effort
                   next-start this-start)))))
  entries)

(defun org-taskforecast--get-entry ()
  "Get an entry at the current point.
A returned value is an entry instance.
If the heading is not an entry, this function returns nil.

WARNING:
If the heading is a section heading,
this function does not set the effort and entries slot of the returned section.
So those slots are still nil.
If you want to get a section which contains them,
consider using `org-taskforecast--get-sections' instead."
  (or (org-taskforecast--get-task-link)
      (org-taskforecast--get-section)))

(defun org-taskforecast--get-entries (file day-start)
  "Get a list of entries from FILE.
DAY-START is an integer like `org-taskforecast-day-start'."
  (org-taskforecast--with-existing-file file
    (--> (org-taskforecast--map-headings #'org-taskforecast--get-entry)
         (-non-nil it)
         (org-taskforecast--set-section-slots it day-start))))

;;;;; Task link

(defun org-taskforecast--append-task-link (id file)
  "Append a task link for ID to the end of FILE.
This function returns an ID of the new task link."
  (let ((normalized-title
         (org-taskforecast--normalize-title
          (org-taskforecast-task-title
           (org-taskforecast--get-task-by-id id)))))
    (org-taskforecast--with-existing-file file
      (save-excursion
        (goto-char (point-max))
        (unless (bolp)
          (insert "\n"))
        (insert (concat "* [[id:" id "][" normalized-title "]]\n"))
        (org-id-get-create)))))

(defun org-taskforecast--append-task-link-maybe (id file date day-start)
  "Append a task link for ID to the end of FILE.
If a todo task link corresponding to ID already exists,
this function does nothing.
This function returns an ID of the task link which was appended or already
exists corresponding to the task.

- ID is a task id
- FILE is a today's daily task list file name
- DATE is an encoded time as a date of today
- DAY-START is an integer like `org-taskforecast-day-start'"
  (--> (org-taskforecast--get-task-links-for-task id file)
       (--filter
        (eq 'todo
            (org-taskforecast-entry-todo-state-for-today
             it date day-start))
        it)
       (-if-let* ((task-link (-first-item it))
                  (link-id (org-taskforecast-tlink-id task-link)))
           link-id
         (org-taskforecast--append-task-link id file))))

(defun org-taskforecast--get-first-todo-task-link (file date day-start now)
  "Get the first todo task link in FILE.
A returned value is an instance of `org-taskforecast--tlink'.
If a first todo task is not found, this function returns nil.

- FILE is a today's daily task list file name
- DATE is an encoded time as a date of today
- DAY-START is an integer like `org-taskforecast-day-start'
- NOW is an encoded time as the current time"
  (org-taskforecast--with-existing-file file
    (save-excursion
      (goto-char
       (org-taskforecast--get-todo-entry-head-pos file date day-start now))
      ;; `org-taskforecast--get-todo-entry-head-pos' returns the end
      ;; of buffer if there is no todo entries.
      ;; And it may return a position of a section heading.
      ;; So find a todo task heading from the returned position.
      (cl-loop until (eobp)
               for entry = (org-taskforecast--get-entry)
               if (and entry
                       (org-taskforecast-entry-is-task-link entry))
               return entry
               else do (forward-line)))))

(defun org-taskforecast--split-task-link (link-id time file)
  "Split a task link of LINK-ID on FILE as interrupted at TIME."
  (let* ((task-id (org-taskforecast-tlink-task-id
                   (org-taskforecast--get-task-link-by-id link-id)))
         (new-link-id (org-taskforecast--append-task-link task-id file))
         (new-link-heading (org-taskforecast--cut-heading-by-id new-link-id)))
    (org-taskforecast--at-id link-id
      (org-taskforecast--set-task-link-effective-end-time time)
      (outline-next-heading)
      (insert new-link-heading)
      ;; Now the corsor is at the next heading if it exists.
      ;; So move the cursor backward.
      (forward-char -1)
      (org-taskforecast--set-task-link-effective-start-time time))))

(defun org-taskforecast--push-task-link-maybe (id file date day-start now &optional allow-interruption)
  "Add a task link for ID to the head of todo task links in FILE.
If a task link corresponding to ID already exists, this function moves it.
If the existing task link is done, this function does not move it.
This function returns an ID of the task link corresponding to the task.

If the first todo task link of the task link list has effective clocks and
its task is not the pushed task, this function splits the first todo task
link as interrupted.
This feature is enabled while ALLOW-INTERRUPTION and
`org-taskforecast-enable-interruption' are non-nil.

- ID is a task id
- FILE is a today's daily task list file name
- DATE is an encoded time as a date of today
- DAY-START is an integer like `org-taskforecast-day-start'
- NOW is an encoded time as the current time"
  ;; interruption
  (when (and allow-interruption org-taskforecast-enable-interruption)
    (-when-let* ((first-todo-task-link
                  (org-taskforecast--get-first-todo-task-link
                   file date day-start now))
                 (task-id
                  (org-taskforecast-tlink-task-id first-todo-task-link))
                 (link-id
                  (org-taskforecast-tlink-id first-todo-task-link)))
      (when (and (not (equal id task-id))
                 (org-taskforecast-entry-has-effective-clock
                  first-todo-task-link
                  date
                  day-start))
        (org-taskforecast--split-task-link link-id now file))))
  (let ((link-id (org-taskforecast--append-task-link-maybe
                  id file date day-start)))
    (org-taskforecast--move-task-link-to-todo-head
     link-id file date day-start now)
    link-id))

(defun org-taskforecast--get-task-links (file)
  "Get a task link list from FILE."
  (-filter #'org-taskforecast-entry-is-task-link
           ;; day-start is not necessary to get task links,
           ;; it is only necessary for getting sectios.
           (org-taskforecast--get-entries file 0000)))

(defun org-taskforecast--get-task-links-for-task (task-id file)
  "Get task links for the task of TASK-ID in FILE.
- TASK-ID is a string
- FILE is a today's daily task list file name"
  (--filter
   (string= task-id (org-taskforecast-tlink-task-id it))
   (org-taskforecast--get-task-links file)))

(defun org-taskforecast--get-todo-entry-head-pos (file date day-start now)
  "Get the point of head of todo entries in FILE.
If there is no todo entry heading, this function returns the point of
the end of buffer.

- FILE is a today's daily task list file name
- DATE is an encoded time as a date of today
- DAY-START is an integer like `org-taskforecast-day-start'
- NOW is an encoded time as the current time"
  (let ((pos nil))
    (org-taskforecast--with-existing-file file
      (org-taskforecast--map-headings
       (lambda ()
         (unless pos
           (--> (org-taskforecast--get-entry)
                (cond ((org-taskforecast-entry-is-task-link it)
                       (org-taskforecast-entry-todo-state-for-today
                        it date day-start))
                      ;; pseudo todo state for section to put a task link
                      ;; heading before a section heading if the section is
                      ;; not started.
                      ((org-taskforecast-entry-is-section it)
                       (let* ((sthhmm (org-taskforecast-section-start-time it))
                              (st (org-taskforecast--encode-hhmm sthhmm date)))
                         (if (time-less-p now st)
                             'todo
                           'done))))
                (when (eq it 'todo)
                  (setq pos (point)))))))
      (or pos (point-max)))))

(defun org-taskforecast--move-task-link-to-todo-head (link-id file date day-start now)
  "Move a task link of LINK-ID to the head of todo task links of FILE.
- ID is an id of org-id of a task link
- FILE is a today's daily task list file name
- DATE is an encoded time as a date of today
- DAY-START is an integer like `org-taskforecast-day-start'
- NOW is an encoded time as the current time"
  (unless (org-taskforecast--at-id link-id (org-taskforecast--get-task-link))
    (error "Not a task link ID: %s" link-id))
  (let ((task-link (org-taskforecast--cut-heading-by-id link-id))
        (head (org-taskforecast--get-todo-entry-head-pos
               file date day-start now)))
    (org-taskforecast--with-existing-file file
      (save-excursion
        (goto-char head)
        (insert task-link)))))

;;;;; Section

(defconst org-taskforecast--section-id-prop-name
  "ORG_TASKFORECAST_SECTION_ID"
  "Property name of a section ID string of a section.")

(defconst org-taskforecast--section-start-time-prop-name
  "ORG_TASKFORECAST_SECTION_START_TIME"
  "Property name of a start time of a section.")

(defun org-taskforecast--get-section ()
  "Get a section at the current point.
A returned value is an instance of `org-taskforecast--section'.
If the heading is not a section, this function returns nil.

WARNING:
This function does not set the effort and entries slot of the returned section.
So those slots are still nil.
If you want to get a section which contains them,
consider using `org-taskforecast--get-sections' instead."
  (save-excursion
    ;; Prevent error when there is no heading in a buffer.
    (unless (org-before-first-heading-p)
      ;; go to heading line for `org-element-at-point' to get a headline element
      (org-back-to-heading))
    (-when-let* ((section-id (org-entry-get
                              nil org-taskforecast--section-id-prop-name))
                 (start-time (org-entry-get
                              nil org-taskforecast--section-start-time-prop-name))
                 (description (org-element-property
                               :title (org-element-at-point)))
                 (id (org-id-get-create)))
      (org-taskforecast--section
       :id id
       :section-id section-id
       :start-time (string-to-number start-time)
       :description description))))

(defun org-taskforecast--get-section-by-id (id)
  "Get a section by ID.
WARNING:
This function does not set the effort and entries slot of the returned section.
So those slots are still nil.
If you want to get a section which contains them,
consider using `org-taskforecast--get-sections' instead."
  (org-taskforecast--at-id id
    (org-taskforecast--get-section)))

(defun org-taskforecast--get-sections (file day-start)
  "Get a section list from FILE.
DAY-START is an integer like `org-taskforecast-day-start'."
  (-filter #'org-taskforecast-entry-is-section
           (org-taskforecast--get-entries file day-start)))

(defun org-taskforecast--append-section (section-id start-time description file)
  "Append a section heading to the end of FILE.
This function returns an ID of the section heading.

- SECTION-ID is a string of section ID
- START-TIME is an integer like `org-taskforecast-day-start'
- DESCRIPTION is a string of section description or nil"
  (let ((title (org-taskforecast--normalize-title
                (or description
                    (--> start-time
                         (org-taskforecast--hhmm-to-second it)
                         (org-taskforecast--format-second-to-hhmm it)
                         (format "SECTION: %s -" it))))))
    (org-taskforecast--with-existing-file file
      (save-excursion
        (goto-char (point-max))
        (unless (bolp)
          (insert "\n"))
        (insert (concat "* " title "\n"))
        (org-entry-put nil org-taskforecast--section-id-prop-name section-id)
        (org-entry-put nil
                       org-taskforecast--section-start-time-prop-name
                       (format "%s" start-time))
        (org-id-get-create)))))

(defun org-taskforecast--append-section-maybe (section-id start-time description file day-start)
  "Append a section heading to the end of FILE.
If a section heading corresponding to SECTION-ID already exists,
this function does nothing.
This function returns an ID of the section heading which was appended or
already exists corresponding to SECTION-ID.

- SECTION-ID is a string of section ID
- START-TIME is an integer like `org-taskforecast-day-start'
- DESCRIPTION is a string of section description or nil
- DAY-START is an integer like `org-taskforecast-day-start'"
  (--> (org-taskforecast--get-sections file day-start)
       (--filter (string= section-id (org-taskforecast-section-section-id it))
                 it)
       (progn (cl-assert (member (length it) '(0 1)))
              (cl-first it))
       (if it
           (org-taskforecast-section-id it)
         (org-taskforecast--append-section
          section-id start-time description file))))

;;;; Sort

(defvar org-taskforecast-sort-info-today nil
  "This variable is used to pass a date of today to sort comparators.
This value will be an encoded time.
Its hour, minute and second are set to zero.")

(defvar org-taskforecast-sort-info-sections nil
  "This variable is used to pass the section list to sort comparators.
This value will be a list of instances of `org-taskforecast--section'.")

(defun org-taskforecast--sort-compare (a b comparators sections date)
  "Compare A and B with COMPARATORS.
A returned value is:
- +1  if A > B
- -1  if A < B
- nil if A == B

- SECTIONS is a list of instances of `org-taskforecast--section'.
- DATE is an encoded time as a date of today"
  (let ((org-taskforecast-sort-info-sections sections)
        (org-taskforecast-sort-info-today date))
    (cl-loop for cmp in comparators
             for res = (funcall cmp a b)
             when (eql res +1) return +1
             when (eql res -1) return -1)))

(defun org-taskforecast--sort-entry-up (entry file comparators date day-start)
  "Sort ENTRY up in FILE.
Move ENTRY up while it > previous one in FILE, like bubble sort.
This function moves only ENTRY not all of entries in FILE.

- ENTRY is an entry instance
- FILE is a today's daily task list file name
- COMPARATORS is a list of camparators like `org-taskforecast-sorting-storategy'
- DATE is an encoded time as a date of today
- DAY-START is an integer like `org-taskforecast-day-start'"
  (let* ((entries (org-taskforecast--get-entries file day-start))
         (sections (-filter #'org-taskforecast-entry-is-section entries))
         (target-entry-p (lambda (a)
                           (string= (org-taskforecast-entry-id a)
                                    (org-taskforecast-entry-id entry))))
         (registeredp (-some target-entry-p entries)))
    (when registeredp
      (-let (((head _) (-split-when target-entry-p entries))
             (insert-before nil))
        (--each-r-while head
            (eql +1 (org-taskforecast--sort-compare
                     entry it comparators sections date))
          (setq insert-before it))
        (when insert-before
          (-let ((tree (org-taskforecast--cut-heading-by-id
                        (org-taskforecast-entry-id entry)))
                 ((_ . pos) (org-id-find
                             (org-taskforecast-entry-id insert-before))))
            (org-taskforecast--with-existing-file file
              (save-excursion
                (save-restriction
                  (widen)
                  (goto-char pos)
                  (insert tree "\n"))))))))))

;; Comparators
;;
;; Comparator is a function compares two entry interface objects.
;; Let the arguments A and B, the function should return:
;; - +1  if A > B
;; - nil if A = B
;; - -1  if A < B
;;
;; Comparators are given additional information via global variables below:
;; - `org-taskforecast-sort-info-today'
;; - `org-taskforecast-sort-info-sections'
;; For more details of them, see their documentation.

(defun org-taskforecast-ss-time-up (a b)
  "Compare A and B by scheduled/deadline, early first."
  (let* (;; date with hh:mm > date only
         (day-start org-taskforecast-day-start)
         (ta (org-taskforecast-entry-early-planning a day-start))
         (tb (org-taskforecast-entry-early-planning b day-start)))
    (cond ((and ta tb
                (org-taskforecast-timestamp-start-earlier-p ta tb day-start))
           +1)
          ((and ta tb
                (org-taskforecast-timestamp-start-earlier-p tb ta day-start))
           -1)
          ((and ta tb (org-taskforecast-timestamp-start-equal-p ta tb)) nil)
          ((and ta (null tb)) +1)
          ((and (null ta) tb) -1)
          (t nil))))

(defalias 'org-taskforecast-ss-time-down (-flip #'org-taskforecast-ss-time-up)
  "Compare A and B by scheduled/deadline, later first.")

(defun org-taskforecast-ss-effective-effort-up (a b)
  "Compare A and B by effective effort, high effort last.
The order is:
1. low effort
2. high effort
3. no effort"
  (let ((eea (org-taskforecast-entry-effective-effort
              a org-taskforecast-sort-info-today org-taskforecast-day-start))
        (eeb (org-taskforecast-entry-effective-effort
              b org-taskforecast-sort-info-today org-taskforecast-day-start)))
    (cond ((and eea eeb (< eea eeb)) +1)
          ((and eea eeb (> eea eeb)) -1)
          ((and eea eeb (= eea eeb)) nil)
          ((and eea (null eeb)) +1)
          ((and (null eea) eeb) -1)
          (t nil))))

(defun org-taskforecast-ss-effective-effort-down (a b)
  "Compare A and B by effective effort, high effort first.
The order is:
1. high effort
2. low effort
3. no effort"
  (let ((eea (org-taskforecast-entry-effective-effort
              a org-taskforecast-sort-info-today org-taskforecast-day-start))
        (eeb (org-taskforecast-entry-effective-effort
              b org-taskforecast-sort-info-today org-taskforecast-day-start)))
    (cond ((and eea (null eeb)) +1)
          ((and (null eea) eeb) -1)
          (t (org-taskforecast-ss-effective-effort-up b a)))))

(defun org-taskforecast--ss-todo-up (a b)
  "Compare A and B by todo state, done first.
If A and/or B is an instance of `org-taskforecast--section',
this function does not compare them.
So the returned value is nil.

This is an internal comparator, so down version is not defined."
  (unless (-any #'org-taskforecast-entry-is-section (list a b))
    (let ((sa (org-taskforecast-entry-todo-state-for-today
               a org-taskforecast-sort-info-today org-taskforecast-day-start))
          (sb (org-taskforecast-entry-todo-state-for-today
               b org-taskforecast-sort-info-today org-taskforecast-day-start)))
      (cond ((and (eq sa 'done) (eq sb 'todo)) +1)
            ((and (eq sa 'todo) (eq sb 'done)) -1)
            (t nil)))))

(defun org-taskforecast--ss-interruption-up (a b)
  "Compare A and B by interruption property for a same task, older farst.
If A and/or B is an instance of `org-taskforecast--section',
this function does not compare them.
So the returned value is nil.

This is an internal comparator, so down version is not defined."
  (unless (-any #'org-taskforecast-entry-is-section (list a b))
    (let ((esa (org-taskforecast-entry-effective-start-time a))
          (esb (org-taskforecast-entry-effective-start-time b))
          (eea (org-taskforecast-entry-effective-end-time a))
          (eeb (org-taskforecast-entry-effective-end-time b))
          (tida (org-taskforecast-tlink-task-id a))
          (tidb (org-taskforecast-tlink-task-id b)))
      (cond ((not (string= tida tidb)) nil)
            ((and esa esb (time-less-p esa esb)) +1)
            ((and esa esb (time-less-p esb esa)) -1)
            ((and eea eeb (time-less-p eea eeb)) +1)
            ((and eea eeb (time-less-p eeb eea)) -1)
            ((or (null esa) (null eeb)) +1)
            ((or (null eea) (null esb)) -1)
            (t nil)))))

(defun org-taskforecast--ss-default-section-up (a b)
  "Compare A and B by default section, earlier farst.
This is an internal comparator, so down version is not defined.

If an entry has no default section, this function tries to derive
a section by `org-taskforecast-entry-derive-default-section'."
  (-let* ((day-start org-taskforecast-day-start)
          (id-st
           (--map
            (cons (org-taskforecast-section-section-id it)
                  (org-taskforecast-section-start-time it))
            org-taskforecast-sort-info-sections))
          ((sta stb)
           (--> (list a b)
                (--map
                 (or
                  (org-taskforecast-entry-default-section-id it)
                  (-some--> (org-taskforecast-entry-derive-default-section
                             it
                             org-taskforecast-sort-info-sections
                             org-taskforecast-sort-info-today
                             day-start)
                    (org-taskforecast-section-section-id it)))
                 it)
                (--map
                 (when it
                   (alist-get it id-st nil nil #'equal))
                 it))))
    (cond ((and sta stb (< sta stb)) +1)
          ((and sta stb (= sta stb)) nil)
          ((and sta stb (> sta stb)) -1)
          ((and sta (null stb)) +1)
          ((and (null sta) stb) -1)
          (t nil))))

(defun org-taskforecast--ss-section-up (a b)
  "Compare A and B , section farst.
This is an internal comparator, so down version is not defined."
  (-let (((sa sb)
          (-map #'org-taskforecast-entry-is-section (list a b))))
    (cond ((and sa (not sb)) +1)
          ((and (not sa) sb) -1)
          (t nil))))

(defun org-taskforecast--sort-comparators-for-task-link (additional-comparators)
  "Get sort comparators for registering a task link.
ADDITIONAL-COMPARATORS is a list of additional comparators"
  (append (list #'org-taskforecast--ss-interruption-up
                #'org-taskforecast--ss-todo-up
                #'org-taskforecast--ss-default-section-up
                #'org-taskforecast--ss-section-up)
          additional-comparators))


;;;; org-taskforecast-cache-mode

(defcustom org-taskforecast-use-cache t
  "Non-nil means use `org-taskforecast-cache-mode'."
  :type 'boolean
  :group 'org-taskforecast
  :package-version '(org-taskforecast . "0.1.0"))

(defun org-taskforecast--cache-drop (&rest _)
  "Drop cache data for a heading at point.
This function is used for hook."
  (-some--> (org-id-get)
    (org-taskforecast--memoize-drop it)))

(defvar org-taskforecast-cache-mode nil
  "Cache parsing results and track heading modification.")

;;;###autoload
(define-minor-mode org-taskforecast-cache-mode
  "Cache parsing results and track heading modification.
This global minor mode is used to reduce parsing time of org-mode text.
This minor mode has 2 features:
- cache parsing results of org-mode text
- track heading modification to drop cached results

If you want not to use this global minor mode, set `org-taskforecast-use-cache'
to nil."
  :group 'org-taskforecast
  :global t
  (let ((hooks '(org-clock-in-hook
                 org-clock-out-hook
                 org-clock-cancel-hook
                 org-after-todo-state-change-hook
                 org-property-changed-functions))
        ;; What hooks are run after calling these functions?
        ;; If exist, remove functios from here and add hooks instead.
        (fns '(org-schedule
               org-deadline)))
    (if org-taskforecast-cache-mode
        (progn
          (setq org-taskforecast--memoize-cache
                (org-taskforecast--memoize-make-cache-table))
          (--each hooks
            (add-hook it #'org-taskforecast--cache-drop))
          (--each fns
            (advice-add it :after #'org-taskforecast--cache-drop)))
      (setq org-taskforecast--memoize-cache nil)
      (--each hooks
        (remove-hook it #'org-taskforecast--cache-drop))
      (--each fns
        (advice-remove it #'org-taskforecast--cache-drop)))))

(defun org-taskforecast--cache-mode-setup ()
  "Enable `org-taskforecast-cache-mode' if needed."
  (when (and org-taskforecast-use-cache (not org-taskforecast-cache-mode))
    (org-taskforecast-cache-mode 1)))

(defun org-taskforecast-cache-clear ()
  "Clear all cache data of `org-taskforecast-cache-mode'."
  (interactive)
  (org-taskforecast--memoize-clear))


;;;; General commands

;;;;; Registration

(defmacro org-taskforecast--at-agenda-heading (&rest body)
  "Eval BODY at a heading of the current line of `org-agenda' buffer."
  (declare (indent 0) (debug t))
  `(if (not (eq 'org-agenda-mode major-mode))
       (error "Not an org-agenda buffer")
     (let* ((marker (org-get-at-bol 'org-hd-marker))
            (buffer (marker-buffer marker)))
       (with-current-buffer buffer
         (save-excursion
           (save-restriction
             (widen)
             (goto-char marker)
             (org-show-context)
             ,@body))))))

(defcustom org-taskforecast-auto-generate-sections 'ask
  "Whether generate section headings automatically."
  :type '(choice
          (const :tag "Always" always)
          (const :tag "Ask" ask)
          (const :tag "Never" never))
  :group 'org-taskforecast
  :package-version '(org-taskforecast . "0.1.0"))

(defun org-taskforecast--ask-generat-sections (file sections date day-start)
  "Ask whether generate section headings to FILE if there is no ones.
If the answer is yes, this function generates section headings by
`org-taskforecast-generate-sections'.

- FILE is a today's daily task list file name
- SECTIONS is a list of section definitions like `org-taskforecast-sections'
- DATE is an encoded time as a date of today
- DAY-START is an integer, see `org-taskforecast-day-start'"
  (when (and sections
             (null (org-taskforecast--get-sections file day-start))
             (or (eq org-taskforecast-auto-generate-sections 'always)
                 (and (eq org-taskforecast-auto-generate-sections 'ask)
                      (yes-or-no-p
                       (format "There is no section headings in %s, generate them?"
                               file)))))
    (org-taskforecast-generate-sections file sections date day-start)))

;;;###autoload
(defun org-taskforecast-register-task (file date day-start &optional sections sorting-storategy)
  "Register a task at point as a task for today.
When the task is already registered, this command does nothing.

- FILE is a today's daily task list file name
- DATE is an encoded time as a date of today
- DAY-START is an integer, see `org-taskforecast-day-start'
- SECTIONS is a list of section definitions like `org-taskforecast-sections'
- SORTING-STORATEGY is a list, see `org-taskforecast-sorting-storategy'"
  (interactive
   (let ((today (org-taskforecast--date-of-time
                 (current-time)
                 org-taskforecast-day-start)))
     (list (org-taskforecast-get-dailylist-file today)
           today
           org-taskforecast-day-start
           org-taskforecast-sections
           org-taskforecast-sorting-storategy)))
  (-if-let* ((task (if (eq 'org-agenda-mode major-mode)
                       (org-taskforecast--at-agenda-heading
                         (org-taskforecast--get-task))
                     (org-taskforecast--get-task)))
             (id (org-taskforecast-task-id task)))
      (if (org-taskforecast--get-task-links-for-task id file)
          (message "The task is already registered.")
        (when (called-interactively-p 'any)
          (org-taskforecast--ask-generat-sections
           ;; Here is for interactive call only.
           ;; So sections and day-start are excluded from parameters.
           file sections date day-start))
        (--> (org-taskforecast--append-task-link id file)
             (org-taskforecast--get-task-link-by-id it)
             (org-taskforecast--sort-entry-up
              it file
              (org-taskforecast--sort-comparators-for-task-link
               sorting-storategy)
              date
              day-start)))
    (user-error "Heading is not a task")))

;;;###autoload
(defun org-taskforecast-register-tasks-for-today (file date day-start &optional sections sorting-storategy)
  "Register tasks for today or before as tasks for today from agenda files.
If a task is not registered, register and sort it.
If not, do nothing.

- FILE is a today's daily task list file name
- DATE is an encoded time as a date of today
- DAY-START is an integer, see `org-taskforecast-day-start'
- SECTIONS is a list of section definitions like `org-taskforecast-sections'
- SORTING-STORATEGY is a list, see `org-taskforecast-sorting-storategy'"
  (interactive
   (let ((today (org-taskforecast--date-of-time
                 (current-time)
                 org-taskforecast-day-start)))
     (list (org-taskforecast-get-dailylist-file today)
           today
           org-taskforecast-day-start
           org-taskforecast-sections
           org-taskforecast-sorting-storategy)))
  ;; TODO: consider deadline warning
  ;; TODO: make query and filtering customizable
  (when (called-interactively-p 'any)
    (org-taskforecast--ask-generat-sections file sections date day-start))
  (-let ((next-day-start (org-taskforecast--encode-hhmm
                          (+ day-start 2400)
                          date))
         ((hh mm) (org-taskforecast--split-hhmm day-start)))
    (org-ql-select (org-agenda-files)
      '(and (todo) (ts-a))
      :action
      (lambda ()
        (let* ((element (org-element-at-point))
               (stime (-some--> (org-element-property :scheduled element)
                        (org-taskforecast--get-timestamp-from-timestamp it)
                        (org-taskforecast-timestamp-start-time it hh mm)))
               (dtime (-some--> (org-element-property :deadline element)
                        (org-taskforecast--get-timestamp-from-timestamp it)
                        (org-taskforecast-timestamp-start-time it hh mm)))
               (todayp (--> (list stime dtime)
                            (-non-nil it)
                            (--some (time-less-p it next-day-start) it))))
          (when todayp
            (let* ((id (org-id-get-create))
                   (registerdp (org-taskforecast--get-task-links-for-task
                                id file))
                   (comparators (org-taskforecast--sort-comparators-for-task-link
                                 sorting-storategy)))
              (unless registerdp
                (--> (org-taskforecast--append-task-link id file)
                     (org-taskforecast--get-task-link-by-id it)
                     (org-taskforecast--sort-entry-up
                      it file comparators date day-start))))))))))

;;;###autoload
(defun org-taskforecast-generate-sections (file sections date day-start)
  "Generate sectios headings to FILE.
The section headings are generated from SECTIONS.

When this command is called interactively, generates section headings
from `org-taskforecast-sections' to today's daily task list file.

- SECTIONS is a list of section definitions like `org-taskforecast-sections'
- DATE is an encoded time as a date of today
- DAY-START is an integer, see `org-taskforecast-day-start'"
  (interactive
   (let ((today (org-taskforecast--date-of-time
                 (current-time)
                 org-taskforecast-day-start)))
     (list (org-taskforecast-get-dailylist-file today)
           org-taskforecast-sections
           today
           org-taskforecast-day-start)))
  (let ((exist-secids
         (-map #'org-taskforecast-section-section-id
               (org-taskforecast--get-sections file day-start))))
    ;; remove existent sections from targets to prevet sorting them again
    (--each (--reject (member (cl-first it) exist-secids) sections)
      (--> (-let (((id start-time description) it))
             (org-taskforecast--append-section
              id start-time description file))
           (org-taskforecast--get-section-by-id it)
           (org-taskforecast--sort-entry-up
            it file
            (list
             #'org-taskforecast--ss-default-section-up
             #'org-taskforecast--ss-section-up)
            date
            day-start)))))

;;;;; Setting properties

;;;###autoload
(defun org-taskforecast-set-default-section-id (section-id)
  "Set default section id of the heading at the current point to SECTION-ID."
  (interactive
   (list (completing-read "section id: "
                          (-map #'cl-first org-taskforecast-sections))))
  (org-entry-put nil
                 org-taskforecast--task-default-section-id-prop-name
                 section-id))


;;;; task-forecast-list mode

(defcustom org-taskforecast-list-tlfmt-scheduled-strategy 'earlier
  "Which time to show as a scheduled time of a task, SCHEDULED or DEADLINE.
This changes the behavior of `org-taskforecast-list-tlfmt-scheduled-time'."
  ;; "Later" is not implemented which may be unused.
  :type '(choice
          (const :tag "SCHEDULED" scheduled)
          (const :tag "DEADLINE" deadline)
          (const :tag "Earlier" earlier))
  :group 'org-taskforecast
  :package-version '(org-taskforecast . "0.1.0"))

(defconst org-taskforecast--list-entry-property 'entry
  "A property symbol for an entry data to propertize string.")

(defun org-taskforecast--list-propertize-entry-data (str entry)
  "Put an entry data, ENTRY, into STR."
  (propertize str
              org-taskforecast--list-entry-property
              entry))

(defun org-taskforecast--list-get-entry-at-point ()
  "Get an entry data via text property from current point.
When there is no entry data, this function returns nil."
  (save-excursion
    ;; A character of end of line (newline) is not propertized by
    ;; `org-taskforecast--list-propertize-entry-data'.
    ;; So always get the text property from the beginning of line.
    (beginning-of-line)
    (get-text-property (point)
                       org-taskforecast--list-entry-property)))

(defun org-taskforecast--list-get-task-link-at-point ()
  "Get a task link data via text property from current point.
When there is no task link data, this function returns nil."
  (-some--> (org-taskforecast--list-get-entry-at-point)
    (when (org-taskforecast-entry-is-task-link it)
      it)))

;; `org-taskforecast--list-get-section-at-point' is currently unused.
;; So it is not defined

(defvar org-taskforecast-list-info-task-link nil
  "This variable is used to pass a task link to formatter.
This value will be an instance of `org-taskforecast--tlink'.
See `org-taskforecast-list-task-link-formatters' for more detail.")

(defvar org-taskforecast-list-info-section nil
  "This variable is used to pass a section to formatter.
This value will be an instance of `org-taskforecast--section'.
See `org-taskforecast-list-section-formatter' for more detail.")

(defvar org-taskforecast-list-info-today nil
  "This variable is used to pass a date of today to formatter.
This value will be an encoded time.
Its hour, minute and second are set to zero.
See `org-taskforecast-list-task-link-formatters' and
`org-taskforecast-list-section-formatter' for more detail.")

(defvar org-taskforecast-list-info-now nil
  "This variable is used to pass the current time to formatter.
This value will be an encoded time.
See `org-taskforecast-list-task-link-formatters' and
`org-taskforecast-list-section-formatter' for more detail.")

(defvar org-taskforecast-list-info-task-link-start-end-time nil
  "This variable is used to pass the start and end time to formatter.
This value will be an instance of `org-taskforecast--eclock'.
See `org-taskforecast-list-task-link-formatters' for more detail.")

(defvar org-taskforecast-list-info-sections nil
  "This variable is used to pass the section list to formatter.
This value will be a list of instances of `org-taskforecast--section'.
See `org-taskforecast-list-task-link-formatters' for more detail.")

(defmacro org-taskforecast--list-define-toggleable-tlfmt (name default docstring &rest body)
  "Define toggleable task link formatter.
This macro defines two functions and a variable:
- NAME: a task link formatter function
- NAME-toggle: a command which toggles show/hide of the formatter
- NAME-show: a variable which indicates the show/hide state of the formatter

DOCSTRING and BODY are used in NAME function like below:

    (defun NAME ()
      DOCSTRING
      (when NAME-show
        BODY...))

DEFAULT is the default value of NAME-show."
  (declare (indent 2))
  (let* ((namestr (symbol-name name))
         (name-toggle (intern (concat namestr "-toggle")))
         (name-show (intern (concat namestr "-show")))
         (now-sym (cl-gensym "now-")))
    `(progn
       (defvar ,name-show ,default ,(format "Show/hide state of `%s'." name))
       (defun ,name-toggle (,now-sym)
         ,(format "Toggle show/hide of `%s'." name)
         (interactive (list (current-time)))
         (cl-callf not ,name-show)
         (org-taskforecast-list-refresh nil ,now-sym))
       (defun ,name ()
         ,docstring
         (when ,name-show
           ,@body)))))

(org-taskforecast--list-define-toggleable-tlfmt org-taskforecast-list-tlfmt-scheduled-time t
  "Format scheduled/deadline time of a task.
This function is used for `org-taskforecast-list-task-link-formatters'."
  (let* ((today org-taskforecast-list-info-today)
         (day-start org-taskforecast-day-start)
         (todo (org-taskforecast-entry-todo-state-for-today
                org-taskforecast-list-info-task-link
                today org-taskforecast-day-start))
         (scheduled (org-taskforecast-entry-scheduled
                     org-taskforecast-list-info-task-link))
         (deadline (org-taskforecast-entry-deadline
                    org-taskforecast-list-info-task-link))
         (stime (and scheduled
                     (not (org-taskforecast-timestamp-start-date-only-p
                           scheduled))
                     (org-taskforecast-timestamp-start-time scheduled)))
         (dtime (and deadline
                     (not (org-taskforecast-timestamp-start-date-only-p
                           deadline))
                     (org-taskforecast-timestamp-start-time deadline))))
    (--> (cl-case org-taskforecast-list-tlfmt-scheduled-strategy
           (scheduled stime)
           (deadline dtime)
           (earlier (-some--> (-non-nil (list stime dtime))
                      (-min-by (-flip #'time-less-p) it))))
         ;; TODO: define faces
         (cond
          ((and it
                (eq todo 'todo)
                (org-taskforecast--today-p it today day-start))
           (-let (((hour min) (org-taskforecast--time-to-hhmm it today))
                  (delayp (time-less-p
                           it
                           (org-taskforecast-eclock-start
                            org-taskforecast-list-info-task-link-start-end-time))))
             (--> (format "%d:%02d" hour min)
                  (propertize
                   it 'face
                   (if delayp 'org-warning 'org-scheduled-today)))))
          ((and it (eq todo 'done))
           (-let (((hour min) (org-taskforecast--time-to-hhmm
                               it
                               ;; Repeated task's scheduled/deadline time
                               ;; may not be today.
                               ;; So adjusting date is needed to show
                               ;; hour and minute.
                               (org-taskforecast--date-of-time it day-start))))
             (--> (format "%d:%02d" hour min)
                  (propertize it 'face 'org-scheduled-previously))))
          (t ""))
         (format "%5s" it))))

(org-taskforecast--list-define-toggleable-tlfmt org-taskforecast-list-tlfmt-effort t
  "Format effort property of a task.
This function is used for `org-taskforecast-list-task-link-formatters'."
  (let ((effort (org-taskforecast-entry-effective-effort
                 org-taskforecast-list-info-task-link
                 org-taskforecast-list-info-today
                 org-taskforecast-day-start)))
    (format "%5s"
            (if effort
                (org-taskforecast--format-second-to-hhmm effort)
              "-:--"))))

(org-taskforecast--list-define-toggleable-tlfmt org-taskforecast-list-tlfmt-start t
  "Format time when a task has been started.
This function is used for `org-taskforecast-list-task-link-formatters'."
  (cl-assert
   (let ((decoded (decode-time org-taskforecast-list-info-today)))
     (= (org-taskforecast--decoded-time-hour decoded)
        (org-taskforecast--decoded-time-minute decoded)
        (org-taskforecast--decoded-time-second decoded)
        0)))
  (-let* ((start
           (org-taskforecast-eclock-start
            org-taskforecast-list-info-task-link-start-end-time))
          (start-estimated-p
           (org-taskforecast-eclock-start-estimated-p
            org-taskforecast-list-info-task-link-start-end-time))
          ((hour minute)
           (org-taskforecast--time-to-hhmm
            start
            org-taskforecast-list-info-today)))
    (cl-assert (--all-p (>= it 0) (list hour minute)))
    (propertize (format "%2d:%02d" hour minute)
                ;; TODO: define face
                'face (if start-estimated-p 'org-scheduled 'default))))

(org-taskforecast--list-define-toggleable-tlfmt org-taskforecast-list-tlfmt-end t
  "Format time when a task has been closed.
This function is used for `org-taskforecast-list-task-link-formatters'."
  (cl-assert
   (let ((decoded (decode-time org-taskforecast-list-info-today)))
     (= (org-taskforecast--decoded-time-hour decoded)
        (org-taskforecast--decoded-time-minute decoded)
        (org-taskforecast--decoded-time-second decoded)
        0)))
  (-let* ((todo-type
           (org-taskforecast-entry-todo-state-for-today
            org-taskforecast-list-info-task-link
            org-taskforecast-list-info-today
            org-taskforecast-day-start))
          (end
           (org-taskforecast-eclock-end
            org-taskforecast-list-info-task-link-start-end-time))
          (end-estimated-p
           (org-taskforecast-eclock-end-estimated-p
            org-taskforecast-list-info-task-link-start-end-time))
          (overrunp_
           (org-taskforecast-eclock-overrun-p
            org-taskforecast-list-info-task-link-start-end-time))
          (overrunp
           (and end-estimated-p
                (eq todo-type 'todo)
                overrunp_))
          ((hour minute)
           (org-taskforecast--time-to-hhmm
            (if overrunp org-taskforecast-list-info-now end)
            org-taskforecast-list-info-today)))
    (propertize (format "%2d:%02d" hour minute)
                ;; TODO: define face
                'face (cond
                       (overrunp 'org-warning)
                       (end-estimated-p 'org-scheduled)
                       (t 'default)))))

(org-taskforecast--list-define-toggleable-tlfmt org-taskforecast-list-tlfmt-clock t
  "Format clock of a task link.
This function is used for `org-taskforecast-list-task-link-formatters'."
  (let* ((eclocks (org-taskforecast-entry-effective-clocks
                   org-taskforecast-list-info-task-link
                   org-taskforecast-list-info-today
                   org-taskforecast-day-start))
         (total (-reduce-from
                 #'time-add
                 (seconds-to-time 0)
                 (-map #'org-taskforecast-clock-duration eclocks))))
    (format "%5s"
            (if eclocks
                (org-taskforecast--format-second-to-hhmm
                 (time-to-seconds total))
              "-:--"))))

(org-taskforecast--list-define-toggleable-tlfmt org-taskforecast-list-tlfmt-default-section-id nil
  "Format default section id of a task link.
This function is used for `org-taskforecast-list-task-link-formatters'."
  (let* ((day-start org-taskforecast-day-start)
         (task-link org-taskforecast-list-info-task-link)
         (date org-taskforecast-list-info-today)
         (sections org-taskforecast-list-info-sections)
         (len (--> (append (-map #'cl-first org-taskforecast-sections)
                           (-map #'org-taskforecast-section-section-id
                                 sections))
                   (-map #'length it)
                   ;; if there is no sections and no section templates,
                   ;; use "    " as dummy string
                   (if it (-max it) 4))))
    (--> (org-taskforecast-entry-default-section-id task-link)
         (if it
             it
           (-some--> (org-taskforecast-entry-derive-default-section
                      task-link sections date day-start)
             (org-taskforecast-section-section-id it)
             ;; TODO: define face
             (propertize it 'face 'font-lock-comment-face)))
         (s-pad-left len " " it))))

(defun org-taskforecast-list-tlfmt-todo ()
  "Format task link's todo state.
This function is used for `org-taskforecast-list-task-link-formatters'."
  (let ((todo-type
         (org-taskforecast-entry-todo-state-for-today
          org-taskforecast-list-info-task-link
          org-taskforecast-list-info-today
          org-taskforecast-day-start)))
    (propertize (cl-case todo-type
                  ('todo "TODO")
                  ('done "DONE"))
                ;; TODO: define face
                'face (cl-case todo-type
                        ('todo 'org-todo)
                        ('done 'org-done)))))

(defun org-taskforecast-list-tlfmt-title ()
  "Format task's title.
This function is used for `org-taskforecast-list-task-link-formatters'."
  (propertize (org-taskforecast-entry-title
               org-taskforecast-list-info-task-link)
              ;; TODO: define face
              'face 'org-scheduled-today))

(defun org-taskforecast-list-secfmt-section ()
  "Format section.
This function is used for `org-taskforecast-list-section-formatter'."
  (let* ((section org-taskforecast-list-info-section)
         (today org-taskforecast-list-info-today)
         (day-start org-taskforecast-day-start)
         (effective-effort
          (--> (org-taskforecast-section-entries section)
               (-map (-rpartial #'org-taskforecast-entry-effective-effort
                                today day-start)
                     it)
               (-non-nil it)
               (-sum it)))
         (effort (org-taskforecast-section-effort section))
         (title (org-taskforecast-section-description section)))
    (format "[%5s / %5s]: %s"
            (propertize
             (org-taskforecast--format-second-to-hhmm effective-effort)
             ;; TODO: define face
             'face (when (> effective-effort effort) 'org-warning))
            (org-taskforecast--format-second-to-hhmm effort)
            title)))

(defun org-taskforecast--list-create-task-link-content (task-link sections date start-end-time now day-start)
  "Create a content of a task link for today's task list.
- TASK-LINK is an instance of `org-taskforecast--tlink'
- SECTIONS is a list of instances of `org-taskforecast--section'
- DATE is an encoded time as a date of today
- START-END-TIME is an instance of `org-taskforecast--eclock'
- NOW is an encoded time as the current time
- DAY-START is an integer, see `org-taskforecast-day-start'"
  (let ((org-taskforecast-list-info-task-link task-link)
        (org-taskforecast-list-info-sections sections)
        (org-taskforecast-list-info-today date)
        (org-taskforecast-list-info-now now)
        (org-taskforecast-day-start day-start)
        (org-taskforecast-list-info-task-link-start-end-time start-end-time))
    (--> org-taskforecast-list-task-link-formatters
         (-map #'funcall it)
         (-reject #'s-blank-p it)
         (s-join " " it)
         (org-taskforecast--list-propertize-entry-data it task-link))))

(defun org-taskforecast--list-create-section-content (section date now day-start)
  "Create a content of a section for today's task list.
- SECTION is an instance of `org-taskforecast--section'
- DATE is an encoded time as a date of today
- NOW is an encoded time as the current time
- DAY-START is an integer, see `org-taskforecast-day-start'"
  (let ((org-taskforecast-list-info-section section)
        (org-taskforecast-list-info-today date)
        (org-taskforecast-list-info-now now)
        (org-taskforecast-day-start day-start))
    (--> (funcall org-taskforecast-list-section-formatter)
         (org-taskforecast--list-propertize-entry-data it section))))

(defun org-taskforecast--create-task-list (today day-start now)
  "Create a today's task list for TODAY.
This function returns a string as contents of `org-taskforecast-list-mode'.
Task list data are stored at each line of listed tasks.
To get them, use `org-taskforecast--list-get-task-link-at-point'.

- TODAY is an encoded time
- DAY-START is an integer, see `org-taskforecast-day-start'
- NOW is an encoded time"
  (--> (org-taskforecast--get-entries
        (org-taskforecast-get-dailylist-file today)
        day-start)
       (let ((sections
              (-filter #'org-taskforecast-entry-is-section it))
             (last-task-done-time
              (org-taskforecast--encode-hhmm day-start today)))
         (--map
          (if (org-taskforecast-entry-is-task-link it)
              (let* ((todo-type
                      (org-taskforecast-entry-todo-state-for-today
                       it today day-start))
                     (start-end-time
                      (org-taskforecast--tlink-start-end-time
                       it
                       today
                       day-start
                       last-task-done-time
                       (and (eq todo-type 'todo) now))))
                (prog1
                    (org-taskforecast--list-create-task-link-content
                     it sections today start-end-time now day-start)
                  ;; update last done time
                  (setq last-task-done-time
                        (org-taskforecast-eclock-end start-end-time))))
            (org-taskforecast--list-create-section-content
             it today now day-start))
          it))
       (s-join "\n" it)
       ;; The last empty line helps cursor movement by `next-line'
       (concat it "\n")))

(defun org-taskforecast--insert-task-list (today day-start now)
  "Insert a TODAY's task list.
This function inserts contents of `org-taskforecast-list-mode'.

- TODAY is an encoded time
- DAY-START is an integer, see `org-taskforecast-day-start'
- NOW is an encoded time"
  (insert (org-taskforecast--create-task-list today day-start now)))

(defvar org-taskforecast-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'org-taskforecast-list-refresh)
    (define-key map (kbd "I") #'org-taskforecast-list-clock-in)
    (define-key map (kbd "O") #'org-taskforecast-list-clock-out)
    (define-key map (kbd "n") #'org-taskforecast-list-next-line)
    (define-key map (kbd "p") #'org-taskforecast-list-previous-line)
    (define-key map (kbd "t") #'org-taskforecast-list-todo)
    (define-key map (kbd "e") #'org-taskforecast-list-set-effort)
    (define-key map (kbd "U") #'org-taskforecast-list-move-entry-up)
    (define-key map (kbd "D") #'org-taskforecast-list-move-entry-down)
    (define-key map (kbd "d") #'org-taskforecast-list-remove-entry)
    (define-key map (kbd "P") #'org-taskforecast-list-postpone-link)
    (define-key map (kbd "RET") #'org-taskforecast-list-goto-task)
    (define-key map (kbd "TAB") #'org-taskforecast-list-goto-task-other-window)
    (define-key map (kbd "q") #'org-taskforecast-list-quit)
    (define-key map (kbd "s") #'org-save-all-org-buffers)
    (define-key map (kbd "C-c C-s") #'org-taskforecast-list-schedule)
    (define-key map (kbd "C-c C-d") #'org-taskforecast-list-deadline)
    (define-key map (kbd "z") #'org-taskforecast-list-add-note)
    (define-key map (kbd ":") #'org-taskforecast-list-set-tags)
    (define-key map (kbd "S") #'org-taskforecast-list-set-default-section-id)
    (define-key map (kbd "vS") #'org-taskforecast-list-tlfmt-scheduled-time-toggle)
    (define-key map (kbd "vf") #'org-taskforecast-list-tlfmt-effort-toggle)
    (define-key map (kbd "vs") #'org-taskforecast-list-tlfmt-start-toggle)
    (define-key map (kbd "ve") #'org-taskforecast-list-tlfmt-end-toggle)
    (define-key map (kbd "vc") #'org-taskforecast-list-tlfmt-clock-toggle)
    (define-key map (kbd "vd") #'org-taskforecast-list-tlfmt-default-section-id-toggle)
    map)
  "A key map for `org-taskforecast-list-mode'.")

(define-derived-mode org-taskforecast-list-mode nil "org-taskforecast list"
  "A major-mode to manage today's tasks."
  :group 'org-taskforecast
  (org-taskforecast--cache-mode-setup)
  (setq-local truncate-lines t)
  (setq-local buffer-read-only t))

(defvar org-taskforecast--list-buffer-name "*org-taskforecast list*"
  "A buffer name for `org-taskforecast-list-mode'.")

(defun org-taskforecast--get-list-buffer ()
  "Get the buffer for `org-taskforecast-list-mode'.
When the buffer is not found, this function returns nil."
  (get-buffer org-taskforecast--list-buffer-name))

(defun org-taskforecast--create-list-buffer (today day-start now)
  "Create a buffer for `org-taskforecast-list-mode'.
If the buffer already exists, only returns the buffer.

- TODAY is an encoded time
- DAY-START is an integer, see `org-taskforecast-day-start'
- NOW is an encoded time"
  (let ((buffer (org-taskforecast--get-list-buffer)))
    (or buffer
        (with-current-buffer (get-buffer-create
                              org-taskforecast--list-buffer-name)
          (org-taskforecast-list-mode)
          (save-excursion
            (let ((inhibit-read-only t))
              (org-taskforecast--insert-task-list today day-start now)))
          (current-buffer)))))

;;;###autoload
(defun org-taskforecast-list (date day-start now)
  "Show the buffer of `org-taskforecast-list-mode'.
- DATE is an encoded time as a date of today
- DAY-START is an integer, see `org-taskforecast-day-start'
- NOW is an encoded time"
  (interactive
   (let ((now (current-time)))
     (list (org-taskforecast--date-of-time now org-taskforecast-day-start)
           org-taskforecast-day-start
           now)))
  (switch-to-buffer
   (org-taskforecast--create-list-buffer date day-start now)))

(defmacro org-taskforecast--save-window-start (buffer &rest body)
  "Save window start position of windows while evaluating BODY.
Windows displaying BUFFER are the targets."
  (declare (debug t) (indent 1))
  (let ((conf-sym (cl-gensym "conf-"))
        (buf-sym (cl-gensym "buf-")))
    `(let* ((,buf-sym ,buffer)
            (,conf-sym
             (--> (window-list)
                  (--filter (eq ,buf-sym (window-buffer it)) it)
                  (--map (cons it (window-start it)) it))))
       (prog1 (progn ,@body)
         (--each ,conf-sym
           (set-window-start (car it) (cdr it) t))))))

(defun org-taskforecast--list-refresh (now)
  "Refresh `org-taskforecast-list-mode' buffer.
NOW is an encoded time."
  (-when-let (buffer (org-taskforecast--get-list-buffer))
    (with-current-buffer buffer
      (org-taskforecast--save-window-start buffer
        (let ((inhibit-read-only t)
              (current-entry (org-taskforecast--list-get-entry-at-point)))
          (erase-buffer)
          (org-taskforecast--insert-task-list
           (org-taskforecast--date-of-time now org-taskforecast-day-start)
           org-taskforecast-day-start
           now)
          ;; Restore the line position of the cursor
          (goto-char (point-min))
          (-some--> current-entry
            (save-excursion
              (cl-loop until (eobp)
                       for entry = (org-taskforecast--list-get-entry-at-point)
                       if (and entry
                               (string=
                                (org-taskforecast-entry-id it)
                                (org-taskforecast-entry-id entry)))
                       return (point)
                       else do (forward-line)))
            (goto-char it)))))))

(defun org-taskforecast-list-refresh (clear-cache now)
  "Refresh `org-taskforecast-list-mode' buffer.
If this command called with a prefix argument (CLEAR-CACHE),
clear all cache data of `org-taskforecast-cache-mode'.
NOW is an encoded time."
  (interactive (list current-prefix-arg (current-time)))
  (when clear-cache
    (org-taskforecast-cache-clear))
  (if (org-taskforecast--get-list-buffer)
      (org-taskforecast--list-refresh now)
    (user-error "List buffer, %s, is not found"
                org-taskforecast--list-buffer-name)))

(defun org-taskforecast-list-clock-in (now)
  "Start the clock on the task linked from the current line.
NOW is an encoded time."
  (interactive (list (current-time)))
  (-if-let* ((task-link (org-taskforecast--list-get-task-link-at-point))
             (task-id (org-taskforecast-tlink-task-id task-link)))
      (progn
        (org-taskforecast--at-id task-id
          (org-clock-in))
        (org-taskforecast--list-refresh now))
    (user-error "Task link not found at the current line")))

(defun org-taskforecast-list-clock-out (now)
  "Stop the current running clock.
NOW is an encoded time."
  (interactive (list (current-time)))
  (org-clock-out)
  (org-taskforecast--list-refresh now))

(defun org-taskforecast-list-next-line ()
  "Go to the next line."
  (interactive)
  (call-interactively #'next-line))

(defun org-taskforecast-list-previous-line ()
  "Go to the previous line."
  (interactive)
  (call-interactively #'previous-line))

(defun org-taskforecast-list-goto-task ()
  "Go to the task linked from the current line."
  (interactive)
  (-if-let* ((task-link (org-taskforecast--list-get-task-link-at-point))
             (task-id (org-taskforecast-tlink-task-id task-link)))
      (org-id-goto task-id)
    (user-error "Task link not found at the current line")))

(defun org-taskforecast-list-goto-task-other-window ()
  "Go to the task linked from the current line in other window."
  (interactive)
  (-if-let* ((task-link (org-taskforecast--list-get-task-link-at-point))
             (task-id (org-taskforecast-tlink-task-id task-link))
             ((file . pos) (org-id-find task-id)))
      (progn
        (switch-to-buffer-other-window (find-file-noselect file))
        (widen)
        (goto-char pos)
        (org-show-context))
    (user-error "Task link not found at the current line")))

(defun org-taskforecast-list-todo (now)
  "Change the TODO state of the task linked from the current line.
NOW is an encoded time."
  (interactive (list (current-time)))
  (-if-let* ((task-link (org-taskforecast--list-get-task-link-at-point))
             (task-id (org-taskforecast-tlink-task-id task-link)))
      (progn
        (org-taskforecast--at-id task-id
          (org-todo))
        (org-taskforecast--list-refresh now))
    (user-error "Task link not found at the current line")))

(defun org-taskforecast-list-set-effort (now)
  "Change Effort property of the task at the current line.
NOW is an encoded time."
  (interactive (list (current-time)))
  (-if-let* ((task-link (org-taskforecast--list-get-task-link-at-point))
             (task-id (org-taskforecast-tlink-task-id task-link)))
      (progn
        (org-taskforecast--at-id task-id
          (org-set-effort))
        (org-taskforecast--list-refresh now))
    (user-error "Task link not found at the current line")))

(defun org-taskforecast-list-move-entry-up (arg now)
  "Move entry at the current line up past ARG others.
NOW is an encoded time."
  (interactive (list (prefix-numeric-value current-prefix-arg) (current-time)))
  (-if-let* ((entry (org-taskforecast--list-get-entry-at-point))
             (id (org-taskforecast-entry-id entry)))
      (progn
        (org-taskforecast--at-id id
          (org-move-subtree-up arg))
        (org-taskforecast--list-refresh now))
    (user-error "Entry not found at the current line")))

(defun org-taskforecast-list-move-entry-down (arg now)
  "Move entry at the current line down past ARG others.
NOW is an encoded time."
  (interactive (list (prefix-numeric-value current-prefix-arg) (current-time)))
  (org-taskforecast-list-move-entry-up (- arg) now))

(defun org-taskforecast--list-remove-link (link-id)
  "Remove a task-link of LINK-ID."
  (org-taskforecast--at-id link-id
    (save-restriction
      (save-excursion
        (widen)
        (org-narrow-to-subtree)
        (delete-region (point-min) (point-max))))))

(defun org-taskforecast-list-remove-entry (now)
  "Remove an entry at the current line.
NOW is an encoded time."
  (interactive (list (current-time)))
  (-when-let* ((entry (org-taskforecast--list-get-entry-at-point))
               (id (org-taskforecast-entry-id entry))
               (title (org-taskforecast-entry-title entry)))
    (org-taskforecast--list-remove-link id)
    ;; Move the cursor to the next line or the previous line to prevent
    ;; moving the cursor to the top of a task list.
    (when (/= 0 (forward-line 1))
      ;; This relies that there is an empty line at the end of buffer.
      (error "Empty line not found at the end of buffer"))
    (when (eobp)
      (forward-line -2))
    (org-taskforecast--list-refresh now)
    (message "%s has been removed from task list." title)))

(defun org-taskforecast-list-postpone-link (date now)
  "Postpone task link to DATE.
- DATE is an encoded time
- NOW is an encoded time"
  (declare (interactive-only t))
  (interactive
   (let ((link (org-taskforecast--list-get-task-link-at-point))
         (now (current-time)))
     (list (cond ((not link)
                  (user-error "Task link not found at the current line"))
                 ((eq 'done
                      (org-taskforecast-entry-todo-state-for-today
                       link now org-taskforecast-day-start))
                  (user-error "Done task cannot be postponed"))
                 (t
                  (org-read-date
                   nil t nil
                   (format "Postpone %s to: "
                           (org-taskforecast-entry-title link)))))
           now)))
  ;; Error checking is done in interactive code above.
  (-when-let* ((link (org-taskforecast--list-get-task-link-at-point))
               (link-id (org-taskforecast-tlink-id link))
               (task-id (org-taskforecast-tlink-task-id link))
               (file (org-taskforecast-get-dailylist-file date)))
    (when (called-interactively-p 'any)
      (org-taskforecast--ask-generat-sections
       file org-taskforecast-sections date org-taskforecast-day-start))
    (if (org-taskforecast-entry-has-effective-clock
         link now org-taskforecast-day-start)
        (org-taskforecast--at-id link-id
          (org-taskforecast--set-task-link-effective-end-time now))
      (org-taskforecast--list-remove-link link-id))
    (let ((new-link-id
           (org-taskforecast--append-task-link-maybe
            task-id file date org-taskforecast-day-start)))
      (org-taskforecast--at-id new-link-id
        (org-taskforecast--set-task-link-effective-start-time now))
      (org-taskforecast--sort-entry-up
       (org-taskforecast--get-task-link-by-id new-link-id)
       file
       (org-taskforecast--sort-comparators-for-task-link
        org-taskforecast-sorting-storategy)
       date
       org-taskforecast-day-start))
    ;; Move the cursor to the next line or the previous line to prevent
    ;; moving the cursor to the top of a task list.
    (when (or (/= 0 (forward-line 1)) (eobp))
      (forward-line -1))
    (org-taskforecast--list-refresh now)))

(defun org-taskforecast-list-schedule (arg now)
  "Call `org-schedule' for the task link at the current point.
ARG is passed to `org-schedule'.
NOW is an encoded time."
  (interactive (list current-prefix-arg (current-time)))
  (declare (interactive-only t))
  (-if-let* ((task-link (org-taskforecast--list-get-task-link-at-point))
             (task-id (org-taskforecast-tlink-task-id task-link)))
      (progn
        (org-taskforecast--at-id task-id
          (org-schedule arg))
        (org-taskforecast--list-refresh now))
    (user-error "Task link not found at the current line")))

(defun org-taskforecast-list-deadline (arg now)
  "Call `org-deadline' for the task link at the current point.
ARG is passed to `org-deadline'.
NOW is an encoded time."
  (interactive (list current-prefix-arg (current-time)))
  (declare (interactive-only t))
  (-if-let* ((task-link (org-taskforecast--list-get-task-link-at-point))
             (task-id (org-taskforecast-tlink-task-id task-link)))
      (progn
        (org-taskforecast--at-id task-id
          (org-deadline arg))
        (org-taskforecast--list-refresh now))
    (user-error "Task link not found at the current line")))

(defun org-taskforecast-list-add-note (now)
  "Call `org-add-note' for the task at the current point.
NOW is an encoded time."
  (interactive (list (current-time)))
  (declare (interactive-only t))
  (-if-let* ((task-link (org-taskforecast--list-get-task-link-at-point))
             (task-id (org-taskforecast-tlink-task-id task-link)))
      (progn
        (org-taskforecast--at-id task-id
          (org-add-note))
        (org-taskforecast--list-refresh now))
    (user-error "Task link not found at the current line")))

(defun org-taskforecast-list-set-tags (now)
  "Call `org-set-tags-command' for the task at the current point.
NOW is an encoded time."
  (interactive (list (current-time)))
  (declare (interactive-only t))
  (-if-let* ((task-link (org-taskforecast--list-get-task-link-at-point))
             (task-id (org-taskforecast-tlink-task-id task-link)))
      (progn
        (org-taskforecast--at-id task-id
          (call-interactively #'org-set-tags-command))
        (org-taskforecast--list-refresh now))
    (user-error "Task link not found at the current line")))

(defun org-taskforecast-list-set-default-section-id (now)
  "Set default section id of the task at the current line.
NOW is an encoded time."
  (interactive (list (current-time)))
  (-if-let* ((task-link (org-taskforecast--list-get-task-link-at-point))
             (task-id (org-taskforecast-tlink-task-id task-link)))
      (progn
        (org-taskforecast--at-id task-id
          (call-interactively #'org-taskforecast-set-default-section-id))
        (org-taskforecast--list-refresh now))
    (user-error "Task link not found at the current line")))

(defun org-taskforecast-list-quit ()
  "Quit the today's task list buffer."
  (interactive)
  (quit-window))


;;;; org-taskforecast-track-mode

(defun org-taskforecast--track-clock-in-task ()
  "Register clocked-in task and move it to top of todo tasks."
  (-when-let* ((now (current-time))
               (today (org-taskforecast--date-of-time
                       now org-taskforecast-day-start))
               (file (org-taskforecast-get-dailylist-file today))
               (task (org-taskforecast--get-task))
               (todo-type (org-taskforecast-task-todo-state-for-today
                           task today org-taskforecast-day-start)))
    ;; A new task link will not be registered if the task's state is done
    ;; and some task links for the task is already registered.
    ;; This function's purpose for a clocked-in task are:
    ;; - to register it if no task link of it is registered
    ;; - to move a task link of it to the top of todo task links
    (when (or (eq todo-type 'todo)
              (and (eq todo-type 'done)
                   (null (org-taskforecast--get-task-links-for-task
                          (org-taskforecast-task-id task) file))))
      (org-taskforecast--push-task-link-maybe
       (org-id-get-create) file today org-taskforecast-day-start now t))))

(defun org-taskforecast--track-done-task ()
  "Register done task and move it to top of todo tasks."
  ;; This function assumes that this is called via
  ;; `org-after-todo-state-change-hook'.
  ;; The hook is run before repeating a task.
  ;; So the state of the task at this time is the specified one by user.
  ;; The reason of not to use `org-state' is it causes
  ;; "reference to free variable" warning without definition of the variable
  ;; in this file.
  (when (org-entry-is-done-p)
    (let* ((now (current-time))
           (today (org-taskforecast--date-of-time
                   now org-taskforecast-day-start))
           (file (org-taskforecast-get-dailylist-file today)))
      (-if-let* ((task (org-taskforecast--get-task))
                 (id (org-taskforecast-task-id task))
                 (head-pos (org-taskforecast--get-todo-entry-head-pos
                            file today org-taskforecast-day-start now))
                 (links (org-taskforecast--get-task-links-for-task id file)))
          ;; When task links of task that placed after first todo task link,
          ;; move that task links to the first todo task link position.
          (progn
            ;; Make sure that cache is dropped for calling this function
            ;; before calling `org-taskforecast--cache-drop' from
            ;; `org-after-todo-state-change-hook'.
            (org-taskforecast--cache-drop)
            (--> links
                 (--filter
                  (< head-pos
                     (cdr (org-id-find (org-taskforecast-tlink-id it))))
                  it)
                 (--each it
                   (org-taskforecast--move-task-link-to-todo-head
                    (org-taskforecast-tlink-id it)
                    file today org-taskforecast-day-start now))))
        ;; When task is not registered, register it and move it to the
        ;; first todo task link position.
        (org-taskforecast--push-task-link-maybe
         (org-id-get-create) file today org-taskforecast-day-start now)))))

(defvar org-taskforecast-track-mode nil
  "Track changes of original tasks and update today's task list.")

;;;###autoload
(define-minor-mode org-taskforecast-track-mode
  "Register tasks automatically when clocked-in or set to done."
  :group 'org-taskforecast
  :global nil
  (org-taskforecast--cache-mode-setup)
  (let ((hook-fns
         (list (cons 'org-clock-in-hook
                     #'org-taskforecast--track-clock-in-task)
               (cons 'org-after-todo-state-change-hook
                     #'org-taskforecast--track-done-task))))
    (if org-taskforecast-track-mode
        (--each hook-fns
          (-let (((hook . fn) it))
            (add-hook hook fn nil t)))
      (--each hook-fns
        (-let (((hook . fn) it))
          (remove-hook hook fn t))))))

(provide 'org-taskforecast)
;;; org-taskforecast.el ends here

;; Local Variables:
;; eval: (when (fboundp 'flycheck-mode) (flycheck-mode 1))
;; eval: (when (fboundp 'flycheck-package-setup) (flycheck-package-setup))
;; byte-compile-error-on-warn: t
;; eval: (outline-minor-mode 1)
;; End:
