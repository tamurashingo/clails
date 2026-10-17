(in-package #:cl-user)
(defpackage #:clails-test/middleware/session-table
  (:use #:cl
        #:rove)
  (:import-from #:clails/environment
                #:*migration-base-dir*)
  (:import-from #:clails/middleware/session/table
                #:+default-session-table-definition+
                #:session-table-definition
                #:session-table-definition-file
                #:read-session-table-definition
                #:check-session-table-definition
                #:session-table-name
                #:session-table-column
                #:session-table-column-name
                #:session-table-ddl)
  (:import-from #:clails/middleware/session/store/db
                #:<db-session-store>
                #:make-db-session-store
                #:db-session-store-table-name
                #:db-session-store-id-column
                #:db-session-store-value-column
                #:db-session-store-last-access-column
                #:serialize-session
                #:deserialize-session
                #:session-time-string
                #:session-time-from-database)
  (:import-from #:clails/middleware/session/store
                #:<session-store>
                #:touch-session
                #:session-last-access-time)
  ;; `clails new' has to copy the definition into the project, so the test for
  ;; that runs the real thing.
  (:import-from #:clails/project/project
                #:create-project))
(in-package #:clails-test/middleware/session-table)


;;; M3-T3, the half that needs no database: the session table definition, the
;;; statement rendered from it, and the agreement between that statement and
;;; the store that reads the table.
;;;
;;; The three dialect suites in test/middleware/session-store/ run the other
;;; half -- creating the table and using it.


(defun dialect (name)
  (make-instance (ecase name
                   (:sqlite3 'clails/environment::<database-type-sqlite3>)
                   (:mysql 'clails/environment::<database-type-mysql>)
                   (:postgresql 'clails/environment::<database-type-postgresql>))))

(defparameter +dialects+ '(:sqlite3 :mysql :postgresql))

(defun ddl (dialect-name &optional (definition +default-session-table-definition+))
  (session-table-ddl definition (dialect dialect-name)))


;;; ------------------------------------------------------------------
;;; The definition
;;; ------------------------------------------------------------------

(deftest session-table-definition-test
  (testing "the framework's definition is a usable one"
    (ok (check-session-table-definition +default-session-table-definition+)
        "it passes its own check")
    (ok (string= (session-table-name +default-session-table-definition+) "clails_sessions")
        "the table is named after the framework, as clails_jobs is"))

  (testing "it fills the three roles the store looks for"
    (dolist (role '(:id :value :last-access))
      (ok (session-table-column role +default-session-table-definition+)
          (format nil "there is a column with role ~S" role))))

  (testing "the id column is 40 characters, not nullable, and the key"
    (let ((column (session-table-column :id +default-session-table-definition+)))
      (ok (eq (getf column :type) :string))
      (ok (eql (getf column :length) 40)
          "40 characters, which is the length of the session id the stage mints")
      (ok (getf column :not-null))
      (ok (getf column :primary-key)
          "PRIMARY KEY, i.e. NOT NULL and UNIQUE, written in the DDL itself")))

  (testing "the session value column is a long type"
    ;; The migration DSL's string type is varchar(255) and cannot be made
    ;; longer; a session is not 255 characters.
    (let ((column (session-table-column :value +default-session-table-definition+)))
      (ok (eq (getf column :type) :text))
      (ok (getf column :not-null))))

  (testing "there is a column for the last-access time (decision 11)"
    ;; The time is store-side state, so the table must have a place for it.
    (let ((column (session-table-column :last-access +default-session-table-definition+)))
      (ok (eq (getf column :type) :datetime)
          "a datetime column, not a number and not part of the session value")
      (ok (getf column :not-null))
      (ok (string= (getf column :name) "last_access_at"))
      ;; Why it is not called updated_at, and why no updated_at exists: nothing
      ;; would update it (the migration DSL has no such facility), and the
      ;; ready-made persistent store of the underlying library skips its UPDATE
      ;; entirely when the session did not change -- which would freeze the
      ;; timestamp of a user who is only reading pages and expire them while
      ;; they are active.  The store writes this column itself, always.
      (ok (not (string= (getf column :name) "updated_at"))
          "and not updated_at, which nothing would keep up to date")))

  (testing "a malformed definition is refused with a reason"
    (ok (signals (check-session-table-definition '(:table "t")) 'error)
        "no columns")
    (ok (signals (check-session-table-definition '(:columns ((:name "a" :type :string)))) 'error)
        "no table name")
    (ok (signals (check-session-table-definition
                  '(:table "t" :columns ((:name "a" :role :id :type :string)
                                         (:name "b" :role :value :type :text))))
                 'error)
        "a missing role")
    (ok (signals (check-session-table-definition
                  '(:table "t" :columns ((:name "a" :role :id :type :string)
                                         (:name "b" :role :id :type :string)
                                         (:name "c" :role :value :type :text)
                                         (:name "d" :role :last-access :type :datetime))))
                 'error)
        "a role filled twice")
    (ok (signals (check-session-table-definition
                  '(:table "t" :columns ((:name "a" :role :id :type "varchar")
                                         (:name "b" :role :value :type :text)
                                         (:name "c" :role :last-access :type :datetime))))
                 'error)
        "a type that is not a keyword")))


(deftest session-table-definition-file-test
  (testing "a definition file is read as data and checked"
    (let* ((directory (uiop:ensure-directory-pathname
                       (format nil "~Aclails-session-table-test/"
                               (uiop:temporary-directory))))
           (file (merge-pathnames "db/session-table.lisp" directory)))
      (ensure-directories-exist file)
      (unwind-protect
           (progn
             (with-open-file (out file :direction :output :if-exists :supersede)
               (format out ";;;; a comment, which read skips~%~
                            (:table \"my_sessions\"~%~
                             :columns ((:name \"sid\" :role :id :type :string :length 40~%~
                                        :not-null t :primary-key t)~%~
                                       (:name \"payload\" :role :value :type :text :not-null t)~%~
                                       (:name \"seen_at\" :role :last-access :type :datetime~%~
                                        :not-null t)))~%"))
             (let ((definition (read-session-table-definition file)))
               (ok (string= (session-table-name definition) "my_sessions"))
               (ok (string= (session-table-column-name :id definition) "sid"))
               (ok (string= (session-table-column-name :value definition) "payload"))
               (ok (string= (session-table-column-name :last-access definition) "seen_at"))
               (ok (eq (getf (session-table-column :id definition) :not-null) t)
                   "a bare T in the file means CL:T, whatever the reader was set to"))

             (testing "the definition in force is the project's when there is one"
               (let ((*migration-base-dir* (namestring directory)))
                 (ok (session-table-definition-file) "the file is found")
                 (ok (string= (session-table-name (session-table-definition)) "my_sessions")
                     "and it decides the table")
                 (testing "and a store built from it follows the file"
                   (let ((store (make-db-session-store)))
                     (ok (string= (db-session-store-table-name store) "my_sessions"))
                     (ok (string= (db-session-store-id-column store) "sid"))
                     (ok (string= (db-session-store-value-column store) "payload"))
                     (ok (string= (db-session-store-last-access-column store) "seen_at"))))))

             (testing "and the framework's own when there is none"
               (let ((*migration-base-dir* (namestring (uiop:temporary-directory))))
                 (ok (null (session-table-definition-file)))
                 (ok (equal (session-table-definition) +default-session-table-definition+))))

             (testing "an empty file is an error rather than an empty table"
               (with-open-file (out file :direction :output :if-exists :supersede)
                 (format out ";;;; nothing but a comment~%"))
               (ok (signals (read-session-table-definition file) 'error)))

             (testing "a file holding something that is not a definition is an error"
               (with-open-file (out file :direction :output :if-exists :supersede)
                 (format out "(:table \"t\")~%"))
               (ok (signals (read-session-table-definition file) 'error))))
        (uiop:delete-directory-tree directory :if-does-not-exist :ignore :validate t)))))


;;; ------------------------------------------------------------------
;;; The statement
;;; ------------------------------------------------------------------

(deftest session-table-ddl-test
  (testing "one CREATE TABLE IF NOT EXISTS statement, in every dialect"
    (dolist (name +dialects+)
      (let ((statement (ddl name)))
        (ok (eql 0 (search "CREATE TABLE IF NOT EXISTS clails_sessions (" statement))
            (format nil "~A: ~A" name statement))
        ;; IF NOT EXISTS is what makes the command safe to run twice, and it is
        ;; how the framework's own migration history table is created in all
        ;; three dialects.
        (ok (search "IF NOT EXISTS" statement) (format nil "~A: idempotent" name))
        (ok (not (find #\; statement))
            (format nil "~A: a single statement, so it needs no transaction" name)))))

  (testing "the columns are what the store needs, in every dialect"
    (dolist (name +dialects+)
      (let ((statement (ddl name)))
        (ok (search "session_id varchar(40) NOT NULL PRIMARY KEY" statement)
            (format nil "~A: the id column is 40 characters, NOT NULL and the key" name))
        (ok (search "session_value text NOT NULL" statement)
            (format nil "~A: the session value column is a long type" name))
        (ok (search "last_access_at" statement)
            (format nil "~A: there is a last-access time column" name))
        (ok (search "DEFAULT CURRENT_TIMESTAMP" statement)
            (format nil "~A: with a default, as the migration history table has" name))
        (ok (not (search "updated_at" statement))
            (format nil "~A: and no updated_at column" name))
        ;; None of the three columns the migration DSL would have inserted.
        (ok (not (search "auto_increment" (string-downcase statement)))
            (format nil "~A: no auto-increment id was inserted" name))
        (ok (not (search "created_at" statement))
            (format nil "~A: no created_at was inserted either" name)))))

  (testing "the only difference between the dialects is the datetime type name"
    ;; Measured rather than assumed, because it decides whether the definition
    ;; has to be kept per dialect: it does not.
    (ok (string= (ddl :sqlite3) (ddl :mysql))
        "SQLite3 and MySQL need the very same statement")
    (ok (search "last_access_at datetime NOT NULL" (ddl :mysql))
        "which spells the time column datetime")
    (ok (search "last_access_at timestamp NOT NULL" (ddl :postgresql))
        "while PostgreSQL spells it timestamp")
    (ok (string= (ddl :postgresql)
                 (let ((mysql (ddl :mysql)))
                   (with-output-to-string (out)
                     (let ((position (search "datetime" mysql)))
                       (write-string (subseq mysql 0 position) out)
                       (write-string "timestamp" out)
                       (write-string (subseq mysql (+ position (length "datetime"))) out)))))
        "and that one word is the whole difference"))

  (testing "a :UNIQUE column that is not the key is written as UNIQUE"
    ;; PRIMARY KEY already implies UNIQUE, so the two are not written together
    ;; (that would ask for a second, redundant index); a column that has to be
    ;; unique without being the key gets the constraint written directly, which
    ;; the migration DSL cannot express at all.
    (let ((statement (session-table-ddl
                      '(:table "t"
                        :columns ((:name "sid" :role :id :type :string :length 40
                                   :not-null t :primary-key t)
                                  (:name "handle" :type :string :not-null t :unique t)
                                  (:name "payload" :role :value :type :text :not-null t)
                                  (:name "seen_at" :role :last-access :type :datetime
                                   :not-null t)))
                      (dialect :mysql))))
      (ok (search "handle varchar(255) NOT NULL UNIQUE" statement) statement)
      (ok (search "sid varchar(40) NOT NULL PRIMARY KEY" statement)
          "and the key column is not given a redundant UNIQUE as well")))

  (testing "an unsupported default is refused rather than rendered"
    (ok (signals (session-table-ddl
                  '(:table "t"
                    :columns ((:name "sid" :role :id :type :string :not-null t :primary-key t)
                              (:name "payload" :role :value :type :text :not-null t)
                              (:name "seen_at" :role :last-access :type :datetime
                               :default :whenever)))
                  (dialect :mysql))
                 'error))))


;;; ------------------------------------------------------------------
;;; The store agrees with the definition
;;; ------------------------------------------------------------------

(deftest db-session-store-defaults-test
  (testing "the store is a derivation of the store base class"
    (let ((store (make-db-session-store :definition +default-session-table-definition+)))
      (ok (typep store '<db-session-store>))
      (ok (typep store '<session-store>)
          "so the session stage accepts it wherever a store is accepted")))

  (testing "its column names are the definition's, so the table always fits"
    (let ((store (make-db-session-store :definition +default-session-table-definition+)))
      (ok (string= (db-session-store-table-name store)
                   (session-table-name +default-session-table-definition+)))
      (ok (string= (db-session-store-id-column store)
                   (session-table-column-name :id +default-session-table-definition+)))
      (ok (string= (db-session-store-value-column store)
                   (session-table-column-name :value +default-session-table-definition+)))
      (ok (string= (db-session-store-last-access-column store)
                   (session-table-column-name :last-access +default-session-table-definition+)))))

  (testing "every column the store names appears in the DDL, in every dialect"
    (let ((store (make-db-session-store :definition +default-session-table-definition+)))
      (dolist (name +dialects+)
        (let ((statement (ddl name)))
          (dolist (column (list (db-session-store-id-column store)
                                (db-session-store-value-column store)
                                (db-session-store-last-access-column store)))
            (ok (search column statement)
                (format nil "~A: the statement defines ~A" name column)))))))

  (testing "the fourth operation is the store's own, not the base class default"
    ;; The base class carries a default TOUCH-SESSION that fetches the session,
    ;; changes nothing and stores it back -- a whole-value write-back on every
    ;; request.  This store must not be using it: it has a method of its own,
    ;; which is one UPDATE of the time column (the dialect suites assert that
    ;; the value column is untouched by it).
    (ok (find-method #'touch-session '()
                     (list (find-class '<db-session-store>) (find-class t)) nil)
        "TOUCH-SESSION has a method specialised on the store")
    (ok (find-method #'session-last-access-time '()
                     (list (find-class '<db-session-store>) (find-class t)) nil)
        "and so has SESSION-LAST-ACCESS-TIME, which reads the column back"))

  (testing "names can still be overridden one by one"
    (let ((store (make-db-session-store :table-name "other_sessions"
                                        :id-column "sid"
                                        :value-column "payload"
                                        :last-access-column "seen_at")))
      (ok (string= (db-session-store-table-name store) "other_sessions"))
      (ok (string= (db-session-store-id-column store) "sid"))
      (ok (string= (db-session-store-value-column store) "payload"))
      (ok (string= (db-session-store-last-access-column store) "seen_at")))))


;;; ------------------------------------------------------------------
;;; Serialising a session, and the time format
;;; ------------------------------------------------------------------
;;;
;;; The database round trip is exercised by the dialect suites; what is checked
;;; here is the encoding itself, which is the same text whatever the database.

(defun make-session (&rest key-value-pairs)
  (let ((session (make-hash-table :test 'equal)))
    (loop for (key value) on key-value-pairs by #'cddr
          do (setf (gethash key session) value))
    session))

(deftest session-serialisation-test
  (testing "a session survives the round trip through text"
    (let* ((session (make-session "string" "text"
                                  "integer" 42
                                  "float" 1.5d0
                                  "keyword" :a-keyword
                                  "list" (list 1 2 "three" :four)
                                  "vector" (vector 1 2 3)
                                  "nil" nil
                                  "true" t))
           (back (deserialize-session (serialize-session session))))
      (ok (hash-table-p back))
      (ok (eq (hash-table-test back) (hash-table-test session))
          "including the hash-table test, so string keys keep working")
      (ok (= (hash-table-count back) (hash-table-count session)))
      (ok (equal (gethash "string" back) "text"))
      (ok (eql (gethash "integer" back) 42))
      (ok (eql (gethash "float" back) 1.5d0))
      (ok (eq (gethash "keyword" back) :a-keyword))
      (ok (equal (gethash "list" back) (list 1 2 "three" :four)))
      (ok (equalp (gethash "vector" back) (vector 1 2 3)))
      (ok (null (gethash "nil" back)))
      (ok (eq (gethash "true" back) t))))

  (testing "a nested hash-table survives too"
    (let* ((inner (make-session "deep" "yes"))
           (session (make-session "inner" inner))
           (back (deserialize-session (serialize-session session))))
      (ok (hash-table-p (gethash "inner" back)))
      (ok (equal (gethash "deep" (gethash "inner" back)) "yes"))))

  (testing "the stored text is one line, so a table row stays readable"
    (let ((text (serialize-session (make-session "a" 1 "b" 2))))
      (ok (stringp text))
      (ok (not (find #\Newline text)))))

  (testing "a value that cannot be read back is refused rather than stored"
    ;; The deliberate limit: a session kept in a shared table holds data.
    (ok (signals (serialize-session (make-session "stream" *standard-output*)) 'error)))

  (testing "nothing in the stored text is evaluated when it is read"
    ;; #. in a column -- from a different store, or from someone with write
    ;; access to the table -- must not run.
    (ok (signals (deserialize-session "#.(error \"evaluated\")") 'error)))

  (testing "an empty session round-trips as an empty session"
    (let ((back (deserialize-session (serialize-session (make-session)))))
      (ok (hash-table-p back))
      (ok (zerop (hash-table-count back)))))

  (testing "nothing stored means no session"
    (ok (null (deserialize-session nil)))))


(deftest session-time-format-test
  (testing "a time is written in the format the model layer writes datetimes in"
    (ok (string= (session-time-string (encode-universal-time 7 34 12 2 3 2026))
                 "2026-03-02 12:34:07")
        "zero-padded, so comparing the text is comparing the time"))

  (testing "it reads back exactly, whichever shape the driver returns"
    (let ((instant (encode-universal-time 7 34 12 2 3 2026)))
      (ok (eql (session-time-from-database (session-time-string instant)) instant)
          "text, as SQLite3 returns it")
      (ok (eql (session-time-from-database instant) instant)
          "a universal time, as this project's PostgreSQL and MySQL paths return")
      (ok (eql (session-time-from-database
                (babel:string-to-octets (session-time-string instant)))
               instant)
          "octets")
      (ok (eql (session-time-from-database "2026-03-02T12:34:07") instant)
          "the ISO spelling with a T")
      (ok (eql (session-time-from-database "2026-03-02 12:34:07.123456") instant)
          "a fractional part, which is ignored rather than refused")))

  (testing "nothing stored means no time"
    (ok (null (session-time-from-database nil)))
    (ok (null (session-time-from-database :null))
        "including a driver's spelling of SQL NULL")))


;;; ------------------------------------------------------------------
;;; `clails new' copies the definition into the project
;;; ------------------------------------------------------------------

(deftest session-table-template-test
  (testing "the template and the framework's definition are the same table"
    ;; Two copies of the same content is a drift risk, so it is checked: the
    ;; file `clails new' writes has to read back as the definition the
    ;; framework falls back to.
    (let ((template (asdf:system-relative-pathname
                     :clails "template/project/db/session-table.lisp.tmpl")))
      (ok (probe-file template) "the template exists")
      (when (probe-file template)
        (ok (equal (read-session-table-definition template)
                   +default-session-table-definition+)
            "and it reads back as the framework's own definition"))))

  (testing "a new project gets db/session-table.lisp"
    (let* ((root (uiop:ensure-directory-pathname
                  (format nil "~Aclails-new-project-test/" (uiop:temporary-directory))))
           (project-dir (merge-pathnames "sessionproj/" root)))
      (ensure-directories-exist project-dir)
      (unwind-protect
           (progn
             (create-project "sessionproj" project-dir :sqlite3)
             (let ((file (merge-pathnames "db/session-table.lisp" project-dir)))
               (ok (probe-file file)
                   "the definition is copied next to db/seeds.lisp and db/migrate/")
               (when (probe-file file)
                 (let ((content (uiop:read-file-string file)))
                   (ok (search "db:create-session-table" content)
                       "the file says which command reads it"))
                 (ok (equal (read-session-table-definition file)
                            +default-session-table-definition+)
                     "and the copy is a definition the framework can read")
                 (testing "the project's copy is what a store in that project uses"
                   (let ((*migration-base-dir* (namestring project-dir)))
                     (ok (session-table-definition-file) "the copy is found")
                     (ok (string= (db-session-store-table-name (make-db-session-store))
                                  "clails_sessions")))))))
        (uiop:delete-directory-tree root :if-does-not-exist :ignore :validate t)))))
