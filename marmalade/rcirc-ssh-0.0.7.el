;;; rcirc-ssh.el --- do irc over ssh sessions -*- lexical-binding: t -*-

;; Copyright (C) 2012  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Maintainer: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: processes, comm
;; Version: 0.0.7
;; Created: 14th September 2012
;; Package-Requires: ((kv "0.0.3"))

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

;; This is makes ssh sessions to boxes which have irc daemons on
;; them. You ssh in with port forwarding and then establish the irc
;; connection over the tunnel.

;;; Code:

(require 'cl)
(require 'kv)
(require 'rcirc)
(require 'assoc) ; only for aget - should provide an aget in kv

(defvar rcirc--server-ssh-connections nil
  "List of ssh connection buffer/processes.

This is state for rcirc-ssh.  It keeps the list of servers to
which we have a connection.")

(defvar rcirc--ssh-session-history nil
  "Completing read history variable.")

(defvar rcirc--ssh-server-history nil
  "Completing read history variable.")

(defvar rcirc--ssh-port-history nil
  "Completing read history variable.")


(defcustom rcirc-ssh-servers ()
  "List of rcirc servers that require an SSH connection first.

A server in this list will have the connection to it made through
an SSH session proxy.  The ssh connection is made with a port
forward to the relevant port (using a randomized local port) and
then the rcirc connection is made to that.

The value part of the list may be an SSH key filename which MUST
be present."
  :group 'rcirc
  :type '(alist
          :key-type string
          :value-type file))

(defun rcirc-ssh--find-free-service ()
  "Return a free (unused) TCP port.

The port is chosen randomly from the ephemeral ports.

This code is pinched from Elnode."
  (let (myserver
        (port 50000)) ; this should be ephemeral base
    (while
        (not
         (processp
          (condition-case sig
              (setq myserver
                    (make-network-process
                     :name "*test-proc*"
                     :server t
                     :nowait 't
                     :host 'local
                     :service port
                     :family 'ipv4))
            (file-error
             (if (equal
                  "Cannot bind server socket address already in use"
                  (mapconcat 'identity (cdr sig) " "))
                 (setq port (+ 50000 (random 5000)))))))))
    (delete-process myserver)
    port))

(defun rcirc--do-ssh (host port key &optional callback)
  "Make an rcirc SSH session to HOST on PORT with KEY.

KEY is an SSH key file name.

Optionally call CALLBACK when the processes state changes.
Callback is passed the PROC, the STATUS and the LOCAL-PORT."
  (let* ((url-form (format "%s:%s" host port))
         (connection-str (format " *ssh-%s-%s*" host port))
         (ssh-buffer
          (with-current-buffer (get-buffer-create connection-str)
            (erase-buffer)
            (current-buffer)))
         (local-port (rcirc-ssh--find-free-service))
         (proc
          (start-process
           ;; We should check for an existing process with this name before
           ;; starting the process
           connection-str
           ssh-buffer
           "ssh" "-N" "-v" ; need the -v so we can detect when the port is up
           "-L" (format "%s:localhost:%s" local-port port)
           "-i" (expand-file-name key)
           host)))
    ;; Set the filter so we can find the port starting
    (set-process-filter
     proc
     (lambda (proc data)
       (with-current-buffer (process-buffer proc)
         (goto-char (point-max))
         (insert data)
         (goto-char (point-min))
         (when (and
                (functionp callback)
                (re-search-forward
                 (format
                  "debug1: Local forwarding listening on 127.0.0.1 port %s."
                  local-port)
                 nil t))
           (funcall callback proc local-port)))))
    ;; Make sure the state of what proceses we have gets updated
    (let ((pair (cons url-form (list :process proc :localport local-port))))
      (add-to-list 'rcirc--server-ssh-connections pair)
      proc)))

;;;###autoload
(defun rcirc-ssh-kill (host-port)
  "Kill the ssh sesion for HOST-PORT a string.

The string is like: host:port, eg: localhost:22"
  (interactive
   (list
    (completing-read
     "host:port: "
     rcirc--server-ssh-connections
     nil ; predicate
     t   ; require-match
     nil ; initial
     'rcirc--ssh-session-history)))
  (let ((pair (assoc host-port rcirc--server-ssh-connections)))
    (when pair
      (delete-process
       (plist-get
        (assoc-default host-port rcirc--server-ssh-connections)
        :process))
      (setq rcirc--server-ssh-connections
            (delq pair rcirc--server-ssh-connections)))))

;;;###autoload
(defun rcirc-ssh-list ()
  "List the current rcirc ssh sessions."
  (interactive)
  (with-current-buffer (get-buffer-create "*rcirc ssh sessions*")
    (setq buffer-read-only t)
    (unwind-protect
         (let ((inhibit-read-only t))
           (erase-buffer)
           (loop for session in rcirc--server-ssh-connections
              do
                (destructuring-bind (host-port &key process localport) session
                  (princ
                   (format "%s on %s    [%s]\n"
                           host-port
                           localport
                           (process-status process))
                   (current-buffer))))
           (switch-to-buffer (current-buffer))))))


;;;###autoload
(defun rcirc-ssh-connect (server
                          &optional
                            port nick user-name
                            full-name startup-channels
                            password encryption)
  "Connecct to the rcirc with possible ssh proxying."
  (if (member server (kvalist->keys rcirc-ssh-servers))
      (let* (real-rcirc-con
             (ssh-session
              (rcirc--do-ssh
               server
               (or port 6667)
               (aget rcirc-ssh-servers server) ; lookup the ssh key
               ;; Supply the callback to start irc after the ssh
               (lambda (proc local-port)
                 ;; Do the proxy connection over the ssh tunnel
                 (unless (process-get proc :rcirc-con)
                   (let ((con (rcirc-ssh--rcirc-connect
                               "localhost"
                               local-port
                               nick user-name full-name
                               startup-channels password
                               encryption)))
                     (setq real-rcirc-con con)
                     (process-put proc :rcirc-con con)))))))
        (message "rcirc-ssh switching to ssh for something to watch")
        (switch-to-buffer (process-buffer ssh-session))
        ;; Wait for the rcirc connection to establish
        (while (not real-rcirc-con)
          (sit-for 0.1))
        ;; Return the correct irc connection
        real-rcirc-con)
      ;; Else do a straight connection to the server
      (rcirc-ssh--rcirc-connect
       server
       port
       nick user-name full-name
       startup-channels password
       encryption)))

;; Bootstrapping

(defvar rcirc-ssh--rcirc-connect 'x)

;;;###autoload
(defun rcirc-ssh--connect-proxy (&rest args)
  "Proxy to allow wiring things up properly"
  (apply 'rcirc-ssh-connect args))

;;;###autoload
(defun rcirc-ssh-bootstrap ()
  "Bootstrap rcirc-ssh by taking over `rcirc-connect'.

The `rcirc-connect' function is saved and changed to the
`rcirc-ssh-connect' function which does ssh connection to the irc
server (if required by the `rcirc-ssh-servers' variable) before
setting up the irc connection.

The original function is saved on the `rcirc-connect' symbol with
the property `rcirc-original'.

You can call this function interactively but the best way to
initialize `rcirc-ssh' is to add an `after-init-hook':

  (add-hook 'after-init-hook 'rcirc-ssh--bootstrap)

in your .emacs."
  (interactive)
  (unless (get 'rcirc-connect 'rcirc-original)
    (let ((original (symbol-function 'rcirc-connect)))
      (put 'rcirc-connect 'rcirc-original original)
      (fset 'rcirc-ssh--rcirc-connect original)
      (fset 'rcirc-connect (symbol-function 'rcirc-ssh--connect-proxy)))))

;;;###autoload
(eval-after-load "rcirc" '(rcirc-ssh-bootstrap))

(provide 'rcirc-ssh)

;;; rcirc-ssh.el ends here
