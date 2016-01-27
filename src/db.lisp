(in-package :cl-user)
(defpackage treehorse.db
  (:use :cl
        :postgres-json)
  (:import-from :treehorse.config
                :config)
  (:import-from :datafly
                :*connection*
                :connect-cached)
  (:export :connection-settings
           :db
           :with-connection
           :new-migration
           :-items-))
(in-package :treehorse.db)

(setf *postmodern-connection* '("treehorse" "treehorse" "pass" "localhost" :port 5432))
(ensure-top-level-connection)

(define-global-model items -items- (pgj-history-object-model))
(ensure-backend -items-)

(defun connection-settings (&optional (db :maindb))
  (cdr (assoc db (config :databases))))

(defun db (&optional (db :maindb))
  (apply #'connect-cached (connection-settings db)))

(defmacro with-connection (conn &body body)
  `(let ((*connection* ,conn))
     ,@body))

(defun new-migration (name)
  "Creates a new migration sql file."
  (let* ((fixed-name (cl-ppcre:regex-replace-all "\\s" (string-downcase name) "-"))
         (timestamp (write-to-string (local-time:timestamp-to-unix (local-time:now))))
         (file-name (concatenate 'string "migrations/" timestamp "-" fixed-name ".sql")))
    (with-open-file  (file file-name
                           :direction :output
                           :if-does-not-exist :create)
      (format file "Write your sql here"))))
