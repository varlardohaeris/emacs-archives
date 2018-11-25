;;; mongo-elnode.el --- elnode adapter for mongo-el

;; Copyright (C) 2012  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Maintainer: Nic Ferrier <nferrier@ferrier.me.uk>
;; Created: 13th August 2012
;; Package-Requires: ((mongo "0.5")(elnode "0.9.9"))
;; Version: 0.5.0
;; Keywords: hypermedia, data

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

;; Connect Elnode apps to your Mongo DB.

;;; Code:

(require 'elnode-db)
(require 'mongo)
(require 'bson)

(defun elnode-db-mongo (reference)
  "Make a db Mongo database

REFERENCE comes from the call to `elnode-db-make' and MUST
include:

 `:host' key with a hostname

 `:collection' key with a collection name

See `mongo-open-database' for more keys that will be passed to
mongo."
  (let* ((host (plist-get (cdr reference) :host))
         (collection (plist-get (cdr reference) :collection))
         (db (list
              :db (make-hash-table :test 'equal)
              :get 'elnode-db-mongo-get
              :put 'elnode-db-mongo-put
              :map 'elnode-db-mongo-map
              :collection collection
              :host host)))
    ;; Return the database
    db))

(defun elnode-db-mongo--do-query (query db)
  "A general querying tool."
  (let* ((host (plist-get db :host))
         (result
          (mongo-with-open-database
              (database :host host)
            (mongo-do-request
             (make-mongo-message-query
              :flags 0
              :number-to-skip 0
              :number-to-return 0
              :full-collection-name (plist-get db :collection)
              :query query)
             :database database)))
         (docres (mongo-message-reply-documents result)))
    docres))

(defun elnode-db-mongo-get (key db)
  "Read record identified by KEY from the mongo DB.

Not often a terribly useful function with mongo because it just
looks up on the id."
  (let ((res (elnode-db-mongo--do-query
              (list
               (cons
                "_id"
                (bson-oid-of-hex-string key)))
              db)))
    (car res)))

(defun elnode-db-mongo-put (key value db)
  "Put the VALUE into the DB at KEY."
  (error "updating mongo not supported yet"))

(defun elnode-db-mongo-map (func db &optional query)
  "Map the FUNC over the records in the DB.

Optionally only match QUERY."
  (mapcar func (elnode-db-mongo--do-query query db)))


;;; Tests

;; These tests require the mongo db to be present with marmalade's db
;; imported.
;;
;; Yes. That is stupid. Because I am not releasing that db (for
;; obvious privacy reasons).
;;
;; To improve this we need a demo mongodb. I haven't got time to make
;; one right now.

(require 'ert)

(ert-deftest elnode-db-mongo-marmalade-get ()
  "Test against Marmalade's Mongo."
  (let ((mdb
         (elnode-db-make
          '(mongo
            :host "localhost"
            :collection "marmalade.packages"))))
    (should
     (equal
      (cdr
       (assoc
        "_name"
        (elnode-db-mongo-get "4f65e980cd6108da68000252" mdb)))
      "fakir"))))

(ert-deftest elnode-db-mongo-marmalade-map ()
  "Test against Marmalade's Mongo."
  (let ((mdb
         (elnode-db-make
          '(mongo
            :host "localhost"
            :collection "marmalade.packages"))))
    ;; Single value, tight query
    (should
     (equal
      (cdr
       (assoc
        "_name"
        (car
         (elnode-db-mongo-map
          'identity
          mdb
          (list (cons "name" "fakir"))))))
      "fakir"))
    ;; Multiple values with a many rows query
    (should
     (equal
      (flet ((collector (res)
               (cdr (assoc "_name" res))))
        (elnode-db-mongo-map
         'collector
         mdb
         (list ; the query
          (cons
           "_latestVersion.headers.author"
             "Nic Ferrier <nferrier@ferrier.me.uk>"))))
      (list "org-email"
            "elnode"
            "creole"
            "fakir"
            "phantomjs"
            "package-store"
            "web")))))

;; Put the mongo db into the list of Elnode dbs
(puthash 'mongo 'elnode-db-mongo elnode-db--types)

(provide 'mongo-elnode)

;;; mongo-elnode.el ends here
