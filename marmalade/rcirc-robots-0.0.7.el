;;; rcirc-robots.el --- robots based on rcirc irc -*- lexical-binding: t -*-

;; Copyright (C) 2012  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: comm
;; Version: 0.0.7
;; Maintainer: Nic Ferrier <nferrier@ferrier.me.uk>
;; Created: 12th September 2012
;; Package-Requires: ((kv "0.0.8")(anaphora "0.0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Building a simpler robot framework out of rcirc.

;;; Code:


(require 'rcirc)
(require 'cl)
(require 'kv)
(require 'anaphora)
(require 'json)

(defcustom rcirc-robots-conf ()
  "Define what robots run on what channels."
  :group 'rcirc ; piggy back for now
  :type `(alist
          :key-type string
          :value-type (repeat string)))

(defcustom rcirc-robots-alist  ()
  "An alist of robot IRC config.

Exactly the same as `rcirc-server-alist'."
  :type '(alist :key-type string
          :value-type (plist :options
                       ((:nick string)
                        (:port integer)
                        (:user-name string)
                        (:password string)
                        (:full-name string)
                        (:channels (repeat string))
                        (:encryption (choice (const tls)
                                             (const plain))))))
  :group 'rcirc)

(defun rcirc-text>channel (process channel text)
  "Send the TEXT to the CHANNEL attached to PROCESS.

This is the building block of automatic responses."
  (with-current-buffer
      (cdr (assoc channel
                  (with-current-buffer
                      (process-buffer process)
                    rcirc-buffer-alist)))
    (goto-char (point-max))
    (insert text)
    (rcirc-send-input)))

(defvar rcirc-robot--process nil
  "Dynamic bound variable for robot send.")

(defvar rcirc-robot--channel nil
  "Dynamic bound variable for robot send")

(defun rcirc-robot-send (text)
  "Send TEXT to the current process and channel.

Robots can use this to send text back to the channel and process
that caused them to be invoked."
  (rcirc-text>channel
   rcirc-robot--process
   rcirc-robot--channel
   text))

(defvar rcirc-robots--list
  (list)
  "The list of robots.

Each robot definition is a plist.  The plist has the following keys:

 :name the name of the robot
 :version the version of the robot definition, currently only version 1
 :regex a regex that will be used to match input and fire the robot
 :function will be called with the strings matched by the regex

When the function is evaluated the function `rcirc-robot-send' is
in scope to send text to the channel that caused the robot
invocation.")

(defun* rcirc-robots-add-function (&key
                                   name
                                   version
                                   regex
                                   (type :text)
                                   function)
  "Add the specified robot to the list."
  (condition-case err
      (progn
        (mapc
         (lambda (p)
           (when (equal (plist-get p :name) name)
             (error "%s exists" name)))
         rcirc-robots--list)
        ;; Install the bot
        (add-to-list
         'rcirc-robots--list
         (list  :name name
                :version version
                :regex regex
                :function function)))
    (error nil)))

(defun rcirc-robots-environment (process channel thunk)
  "Eval THUNK with the correct rcirc environment.

This could be used by bot writers, if for example, you have a bot
that needs to asyncrhonously execute before responding in channel
it could pass the process and channel to a callback which could
then use this to establish the correct environment to call the
THUNK in."
  (let ((rcirc-robot--process process)
        (rcirc-robot--channel channel))
    (funcall thunk)))

;;;###autoload
(defun rcirc-robots--dispatcher (process sender response target text)
  "Loop through `rcirc-robots--list' attempting to dispatch to robots."
  (flet ((match-strings-all (&optional str)
           (let ((m (if str (match-data str) (match-data))))
             (loop for i
                from 0 to (- (/ (length m) 2) 1)
                collect (match-string i str)))))
    (let* ((this-nick (with-current-buffer
                          (process-buffer process)
                        rcirc-nick))
           ;; Is this Nick a registered robot nick?
           (config (assoc this-nick rcirc-robots-conf)))
      (when config
        ;; loop round each robot function config mentioned in the conf
        (loop for robot in
             (loop for robot-name in (cdr config)
                collect
                  (kvplist2get
                   rcirc-robots--list
                   :name
                   robot-name))
           if (let ((re (plist-get robot :regex)))
                (string-match
                 re
                 (case (plist-get robot :type)
                   (:command response)
                   (:text text)
                   (t text))))
           do (rcirc-robots-environment
               process
               target
               (lambda nil ; the thunk
                 (apply
                  (plist-get robot :function)
                  (match-strings-all text)))))))))

(defun rcirc-robots--plist->list (plist-server-defn)
  "Return the server plist in a form useful for `apply'."
  (list
   (car plist-server-defn)
   (plist-get (cdr plist-server-defn) :port)
   (plist-get (cdr plist-server-defn) :nick)
   (plist-get (cdr plist-server-defn) :user-name)
   (plist-get (cdr plist-server-defn) :full-name)
   (plist-get (cdr plist-server-defn) :channels)
   (plist-get (cdr plist-server-defn) :password)
   (plist-get (cdr plist-server-defn) :encryption)))

;;;###autoload
(defun rcirc-robots--connect ()
  ;; Connect any robot connection defined
  (loop for robot in rcirc-robots-alist
     do (apply
         'rcirc-connect
         (rcirc-robots--plist->list robot)))
  ;; Now add the hook
  (add-hook
   'rcirc-print-hooks
   'rcirc-robots--dispatcher))


;; More robots

(defun rcirc-robots-maker (&args)
  (rcirc-robot-send
   (concat
    "I am a robot, you can check my source here: "
    "https://github.com/nicferrier/rcirc-robots")))

(defun rcirc-robots-history (text)
  (message "rcirc-robots-history %s" text))


(defun rcirc-robots-time (text place)
  "Get the time of a place and report it."
  (let ((places
         '(("UTC" . "UTC")
           ("Australia" . "Australia/Sydney")
           ("Melbourne" . "Australia/Melbourne")
           ("Sydney" . "Australia/Sydney")
           ("Brisbane" . "Australia/Brisbane")
           ("Perth" . "Australia/West")
           ("Germany" . "Europe/Berlin")
           ("Berlin" . "Europe/Berlin")
           ("Hamburg" . "Europe/Berlin")
           ("England" . "Europe/London")
           ("London" . "Europe/London")
           ("Edinburgh" . "Europe/London")
           ("Manchester" . "Europe/London")
           ("Brazil" . "America/Sao_Paulo")
           ("Sao Paulo" . "America/Sao_Paulo")
           ("Sao-Paulo" . "America/Sao_Paulo")
           ("SaoPaulo" . "America/Sao_Paulo")
           ("Chicago" . "America/Chicago")
           ("Los Angeles" . "America/Los_Angeles")
           ("Los-Angeles" . "America/Los_Angeles")
           ("San Francisco" . "America/Los_Angeles")
           ("Chennai" . "Asia/Kolkata")
           ("Bangalore" . "Asia/Kolkata")
           ("Pune" . "Asia/Kolkata")
           ("India" . "Asia/Kolkata")
           ("Delhi" . "Asia/Kolkata")
           ("Agartala" . "Asia/Kolkata"))))
    (acond
      ((or
        (equal place "?")
        (equal place "help"))
       (rcirc-robot-send
        (format "places you can query for time %s"
                (kvalist->keys places))))
      ((or (assoc (capitalize place) places)
           (assoc (upcase place) places)) ; assigned to 'it'
       (rcirc-robot-send
        (format
         "the time in %s is %s"
         place
         (let ((tz (getenv "TZ")))
           (unwind-protect
                (progn
                  (setenv "TZ" (cdr it))
                  (format-time-string "%H:%M"))
             (if tz
                 (setenv "TZ" tz))))))))))

(defun rcirc-robots-hammertime (&rest args)
  (let ((quotes (list
                 "READY THE ENORMOUS TROUSERS!"
                 "YOU CAN'T TOUCH THIS!")))
    (rcirc-robot-send (elt quotes (random (length quotes))))))

(defcustom rcirc-robots-insult-adjectives-list
  (list
   "stinky"
   "tiny-minded"
   "pea-brained"
   "heavily lidded"
   "muck minded"
   "flat footed")
  "List of adjectives used in the insulter."
  :group 'rcirc
  :type '(repeat string))

(defcustom rcirc-robots-insult-nouns-list
  (list
   "bog warbler"
   "tin pincher"
   "yeti"
   "whoo-har")
  "List of nouns used in the insulter."
  :group 'rcirc
  :type '(repeat string))

(defun rcirc-robots-insult (text user)
  (message "insult string is: %s" user)
  (cond
    ((string-match "help\\( .*\\)*" user)
     (rcirc-robot-send
      (concat
       "you can insult a user: insult <nick>, "
       "or add a noun or adjective: insult add noun <noun>, "
       "insult add adjective <adjective> "
       "or list nouns or adjectives: insult list nouns")))
    ((string-match "add noun \\(.*\\)" user)
     (add-to-list
      'rcirc-robots-insult-nouns-list
      (match-string 1 user)))
    ((string-match "add adjective \\(.*\\)" user)
     (add-to-list
      'rcirc-robots-insult-adjectives-list
      (match-string 1 user)))
    ((string-match "list nouns" user)
     (rcirc-robot-send (format "%s" rcirc-robots-insult-nouns-list)))
    ((string-match "list adjectives" user)
     (rcirc-robot-send (format "%s" rcirc-robots-insult-adjectives-list)))
    (t
     (message "no special instruction")
     (rcirc-robot-send
      (format
       "%s is a %s %s"
       user
       (elt
        rcirc-robots-insult-adjectives-list
        (random (length rcirc-robots-insult-adjectives-list)))
       (elt
        rcirc-robots-insult-nouns-list
        (random (length rcirc-robots-insult-nouns-list))))))))

(defun remove-newlines (str)
  (while (string-match "\n" str)
    (setq str (replace-match "" t t str)))
  str)

(defun rcirc-robots-doctor (text question)
  (with-current-buffer (or
                        (get-buffer "*doctor*")
                        (progn
                          (doctor)
                          (get-buffer "*doctor*")))
    (goto-char (point-max))
    (if (and (stringp question)
             (< (length question) 1))
        (insert "I'm feeling unwell\n")
        ;; Else
        (insert question))
    (doctor-ret-or-read t)
    (let ((p (point)))
      (doctor-ret-or-read t)
      (rcirc-robot-send (remove-newlines (buffer-substring p (point-max)))))))

(defun rcirc-robots-ud-define (text word)
  (let ((url-request-method "GET"))
    (url-retrieve
     (format "http://urbanscraper.herokuapp.com/define/%s.json" word)
     (lambda (x &rest args)
       (goto-char (point-min))
       (search-forward-regexp "\\HTTP\/[0-9]\.[0-9] \\([0-9]+\\)")
       (let ((status (match-string-no-properties 1)))
         (destructuring-bind (rcirc-robot--process rcirc-robot--channel word) args
           (cond ((equal status "200")
                  (goto-char (point-min))
                  (search-forward-regexp "\{.*")
                  (rcirc-robot-send (rcirc-robots-ud-define-get-details
                                     (let ((json-object-type 'hash-table))
                                       (json-read-from-string
                                        (replace-regexp-in-string
                                         "\\\\r"
                                         " "
                                         (match-string-no-properties 0)))))))
                 ((equal status "404")
                  (rcirc-robot-send "Well, http://urbanscraper.herokuapp.com is 404ing!"))
                 ((equal status "500")
                  (rcirc-robot-send
                   (format  "I refuse to define \'%s\'. http://urbanscraper.herokuapp.com is 500ing!" word)))
                 (t
                  (rcirc-robot-send
                   (format "Unknown error trying to define \'%s\'. Status is %s" word status)))))))
     (list rcirc-robot--process rcirc-robot--channel word))))

(defun rcirc-robots-ud-define-get-details (hash)
  (let ((definition (gethash "definition" hash))
        (url (gethash "url" hash))
        (word (gethash "word" hash)))
    (format "%s - Definition: %s. URL: %s" word definition url)))

;; Robot config

(rcirc-robots-add-function
 :name "maker" :version 1 :regex "who are you?"
 :function 'rcirc-robots-maker)

(rcirc-robots-add-function
 :name "history" :version 1
 :type :command
 :regex "^\\(QUIT\\|JOIN\\)$"
 :function 'rcirc-robots-history)

(rcirc-robots-add-function
 :name "timezone" :version 1 :regex "time \\([A-Za-z\ -]+\\)"
 :function 'rcirc-robots-time)

(rcirc-robots-add-function
 :name "doctor" :version 1 :regex "^doctor\\(.*\\)"
 :function 'rcirc-robots-doctor)

(rcirc-robots-add-function
 :name "hammertime" :version 1 :regex "hammertime[?!]*"
 :function 'rcirc-robots-hammertime)

(rcirc-robots-add-function
 :name "insult" :version 1 :regex "^insult \\([A-Za-z0-9 -]+\\)"
 :function 'rcirc-robots-insult)

(rcirc-robots-add-function
 :name "definebot" :version 1 :regex "^define \\([A-Za-z0-9-]+\\)"
 :function 'rcirc-robots-ud-define)


(provide 'rcirc-robots)

;;; rcirc-robots.el ends here
