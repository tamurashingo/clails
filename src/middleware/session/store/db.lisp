(in-package #:cl-user)
(defpackage #:clails/middleware/session/store/db
  (:use #:cl)
  (:import-from #:clails/environment
                #:*connection-pool*)
  (:import-from #:clails/model/connection
                #:get-connection-direct)
  (:import-from #:clails/middleware/session/clock
                #:session-clock-now)
  (:import-from #:clails/middleware/session/store
                #:<session-store>
                #:fetch-session
                #:store-session
                #:remove-session
                #:touch-session
                #:session-last-access-time)
  (:import-from #:clails/middleware/session/table
                #:session-table-definition
                #:session-table-name
                #:session-table-column-name)
  (:import-from #:babel
                #:octets-to-string)
  (:import-from #:local-time
                #:timestamp
                #:timestamp-to-universal)
  (:export #:<db-session-store>
           #:make-db-session-store
           #:db-session-store-table-name
           #:db-session-store-id-column
           #:db-session-store-value-column
           #:db-session-store-last-access-column
           #:serialize-session
           #:deserialize-session
           #:session-time-string
           #:session-time-from-database))
(in-package #:clails/middleware/session/store/db)


;;; ------------------------------------------------------------------
;;; Session store backed by a database table (M3-T3)
;;; ------------------------------------------------------------------
;;;
;;; The store to configure for production: sessions survive a restart and are
;;; shared by every process that reads the same table, which the default
;;; in-memory store cannot do.  The table it needs is created by
;;; `clails db:create-session-table' (clails/middleware/session/table), and
;;; the column names below default to the ones that definition gives, so the
;;; store and the table cannot be configured apart from each other.
;;;
;;;   (setf clails/middleware/session-middleware:*session-store*
;;;         (clails/middleware/session/store/db:make-db-session-store))
;;;
;;; Which connection, and why not the request's
;;; -------------------------------------------
;;; The session stage sits OUTSIDE the transaction stage, so the connection the
;;; request is using may be in the middle of the application's transaction.
;;; Writing the session through it would enrol the session write in that
;;; transaction -- an application rollback would silently roll the session back
;;; with it, and a session write would be held until the application's commit.
;;; So this store never uses the thread's connection
;;; (clails/model/connection:get-connection).  Each operation takes a
;;; connection of its own and gives it back when it is done:
;;;
;;;   * from the connection pool when there is one, which is the case in a
;;;     running server.  The connection is borrowed directly from the pool, not
;;;     through the per-thread map, so it is a different connection from the
;;;     one the transaction stage is using, and DBI:DISCONNECT on a pooled
;;;     connection returns it to the pool rather than closing it -- so the
;;;     number of connections in use does not grow with the number of requests;
;;;   * a direct connection when there is no pool, which is the case in a CLI
;;;     command or a test that never started one.
;;;
;;; Both branches end in DBI:DISCONNECT, which is the one function that means
;;; "I am done with this connection" for either kind.  An application that
;;; wants something else (a second pool, a replica for reads) passes
;;; :CONNECTOR and :DISCONNECTOR.
;;;
;;; The fourth operation, and why updated_at is not used for it
;;; ----------------------------------------------------------
;;; TOUCH-SESSION is ONE UPDATE of the last-access column and touches nothing
;;; else -- not the session value, not any other column.  Two measured reasons
;;; this is not an updated_at column maintained by something else:
;;;
;;;   (1) nothing updates it.  The migration DSL has no "touch this on update"
;;;       facility, so an updated_at column created by a migration is written
;;;       only by whoever writes the row;
;;;   (2) the ready-made persistent store of the underlying library skips the
;;;       UPDATE ENTIRELY when the serialised session is unchanged (it compares
;;;       the stored text with the new text and does nothing when they are
;;;       equal).  Under an idle-timeout policy that is fatal: a user who is
;;;       reading pages -- changing nothing in the session -- would have no
;;;       write issued on their behalf, their timestamp would stand still, and
;;;       they would be expired while actively using the application.
;;;
;;; Hence a column of this store's own, written by this store, on every store
;;; and on every touch.  STORE-SESSION here never skips its UPDATE for an
;;; unchanged value, for the same reason.
;;;
;;; What a shared table does NOT give you
;;; -------------------------------------
;;; Every FETCH-SESSION rebuilds the session from the stored text, so each
;;; request works on its own hash-table rather than on one container shared by
;;; reference (which is what the in-memory store hands out).  Two consequences,
;;; both intended and both documented:
;;;
;;;   * concurrent requests on one session cannot corrupt a container they do
;;;     not share -- the damage the wrapping store exists to prevent cannot
;;;     happen here at all;
;;;   * a whole-value write-back is exactly that: two requests that overlap
;;;     each write the session they fetched, and the later write wins
;;;     (decision 12 -- the same position Rails and Django take; no optimistic
;;;     locking, no CAS).  This is true between processes AND between two
;;;     concurrent requests in one process, because the session is read before
;;;     the application runs and written after it returns, so the per-session
;;;     lock of the wrapping store (WITH-SESSION-LOCK) cannot cover the pair.
;;;     An application that needs a counter in a shared session to be exact
;;;     keeps the counter in a table of its own, not in the session.


;;; ------------------------------------------------------------------
;;; Serialising a session
;;; ------------------------------------------------------------------
;;;
;;; The session is a hash-table of application data and it has to survive a
;;; round trip through a text column.  PRIN1 / READ do that for everything the
;;; printer can write readably, except for hash-tables themselves, which have
;;; no readable printed representation -- so a hash-table is encoded as a
;;; marker list and decoded back.  Everything else (strings, numbers,
;;; keywords, symbols, lists, vectors) is left exactly as it is, which keeps
;;; the stored text readable by a human looking at the table.
;;;
;;; Printing happens under WITH-STANDARD-IO-SYNTAX, so the text does not depend
;;; on the printer settings of whoever happens to call it; reading happens
;;; under WITH-STANDARD-IO-SYNTAX with *READ-EVAL* off, so a #. that somehow
;;; reached the column is refused rather than executed.
;;;
;;; A value the printer cannot write readably (a CLOS instance, a stream) makes
;;; STORE-SESSION signal rather than store something that cannot be read back.
;;; That is the deliberate limit of this store: a session holds data, and a
;;; shared session table holds data that another process can read.

(defparameter +hash-table-marker+ '%session-hash-table
  "Head of the list a hash-table is encoded as.

   An internal symbol of this package, so the encoded form prints with this
   package's name in it and cannot be confused with application data unless an
   application deliberately stores a list headed by this very symbol.")

(defun %encode (object)
  "Replace every hash-table in OBJECT with a readable marker form."
  (typecase object
    (hash-table
     (list +hash-table-marker+
           (hash-table-test object)
           (loop for key being the hash-keys of object using (hash-value value)
                 collect (cons (%encode key) (%encode value)))))
    (cons (cons (%encode (car object)) (%encode (cdr object))))
    (simple-vector (map 'simple-vector #'%encode object))
    (t object)))

(defun %marker-form-p (form)
  (and (consp form)
       (eq (car form) +hash-table-marker+)
       (consp (cdr form))
       (consp (cddr form))
       (null (cdddr form))))

(defun %decode (form)
  "Rebuild the hash-tables %ENCODE replaced with marker forms."
  (cond ((%marker-form-p form)
         (destructuring-bind (marker test entries) form
           (declare (ignore marker))
           (let ((table (make-hash-table :test test :size (max 16 (length entries)))))
             (dolist (entry entries)
               (setf (gethash (%decode (car entry)) table) (%decode (cdr entry))))
             table)))
        ((consp form) (cons (%decode (car form)) (%decode (cdr form))))
        ((simple-vector-p form) (map 'simple-vector #'%decode form))
        (t form)))

(defun serialize-session (session)
  "Return SESSION as the text that goes into the session value column.

   @param session [t] A session, normally a hash-table
   @return [string] The stored representation
   @condition error Signaled when SESSION holds something that cannot be
     printed readably, rather than storing text that cannot be read back
   "
  (handler-case
      (with-standard-io-syntax
        (prin1-to-string (%encode session)))
    (error (e)
      (error "This session cannot be stored in a database session store: ~A.~%~
              A session kept in a table may only hold data that can be printed ~
              and read back (strings, numbers, symbols, lists, vectors, ~
              hash-tables of those).  Objects such as class instances or ~
              streams have to be kept outside the session."
             e))))

(defun deserialize-session (text)
  "Rebuild the session stored as TEXT.

   TEXT may arrive as a string or, from MySQL, as the octets of one.

   @param text [(or string (vector (unsigned-byte 8)) null)] Stored session
   @return [t] The session, or NIL when TEXT is NIL
   "
  (let ((text (typecase text
                (null nil)
                (string text)
                ((vector (unsigned-byte 8)) (octets-to-string text))
                ;; A driver's spelling of SQL NULL (:NULL, for one) means the
                ;; same as no row: no session.
                (t nil))))
    (when text
      (with-standard-io-syntax
        (let ((*read-eval* nil))
          (%decode (read-from-string text)))))))


;;; ------------------------------------------------------------------
;;; The last-access time in the database
;;; ------------------------------------------------------------------
;;;
;;; Written as "YYYY-MM-DD HH:MM:SS", which is how the model layer writes a
;;; :DATETIME value in all three dialects, so there is one spelling of a time
;;; in this project rather than two.  Measured consequences of that choice,
;;; which matter because an idle timeout has to COMPARE these values:
;;;
;;;   * the three databases all accept it as a datetime / timestamp literal;
;;;   * what comes back differs by driver -- SQLite3 hands back the text,
;;;     MySQL and PostgreSQL hand back something already decoded -- so reading
;;;     goes through SESSION-TIME-FROM-DATABASE, which accepts all of them;
;;;   * the format is fixed-width and zero-padded, so ordering it as text is
;;;     the same as ordering it as a time.  A sweep may therefore compare in
;;;     SQL (WHERE last_access_at < ?) as well as in Lisp.

(defun session-time-string (universal-time)
  "Format UNIVERSAL-TIME for a datetime / timestamp column.

   @param universal-time [integer] Universal time
   @return [string] \"YYYY-MM-DD HH:MM:SS\"
   "
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time universal-time)
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month date hour min sec)))

(defun %parse-time-string (text)
  "Parse \"YYYY-MM-DD HH:MM:SS\" (or the ISO spelling with T, with or without
   a fractional part) into a universal time, or NIL when TEXT is not one."
  (let ((text (string-trim " " text)))
    (when (>= (length text) 19)
      (handler-case
          (encode-universal-time (parse-integer text :start 17 :end 19)
                                 (parse-integer text :start 14 :end 16)
                                 (parse-integer text :start 11 :end 13)
                                 (parse-integer text :start 8 :end 10)
                                 (parse-integer text :start 5 :end 7)
                                 (parse-integer text :start 0 :end 4))
        (error () nil)))))

(defun session-time-from-database (value)
  "Return VALUE, as read out of the last-access column, as a universal time.

   Every driver has its own idea of what a datetime column is: text (SQLite3),
   octets, a universal time already (the PostgreSQL and MySQL paths of this
   project's type conversion), or a local-time timestamp.  All of them are
   accepted here, so that the store does not need a per-dialect method for
   reading one column.

   @param value [t] What the driver returned for the column
   @return [(or integer null)] Universal time, or NIL when there is none
   "
  (typecase value
    (null nil)
    (integer value)
    (string (%parse-time-string value))
    ((vector (unsigned-byte 8)) (%parse-time-string (octets-to-string value)))
    (timestamp (timestamp-to-universal value))
    ;; A driver's spelling of SQL NULL, or anything else unexpected: no time.
    (t nil)))


;;; ------------------------------------------------------------------
;;; The store
;;; ------------------------------------------------------------------

(defclass <db-session-store> (<session-store>)
  ((table-name
    :initarg :table-name
    :reader db-session-store-table-name
    :documentation "Name of the session table.")
   (id-column
    :initarg :id-column
    :reader db-session-store-id-column
    :documentation "Column holding the session id (the primary key).")
   (value-column
    :initarg :value-column
    :reader db-session-store-value-column
    :documentation "Column holding the serialised session.")
   (last-access-column
    :initarg :last-access-column
    :reader db-session-store-last-access-column
    :documentation "Column holding the last-access time -- store-side state,
                    never part of the session value (decision 11).")
   (connector
    :initarg :connector
    :initform nil
    :reader db-session-store-connector
    :documentation "Function of no arguments returning a connection, or NIL for
                    the default (borrow from the pool, or connect directly when
                    there is no pool).")
   (disconnector
    :initarg :disconnector
    :initform nil
    :reader db-session-store-disconnector
    :documentation "Function of one argument releasing a connection, or NIL for
                    DBI:DISCONNECT -- which returns a pooled connection to its
                    pool and closes a direct one."))
  (:documentation "Session store that keeps sessions in a database table.

   The table is created by `clails db:create-session-table'; the column names
   default to the ones in the project's db/session-table.lisp (or, when the
   project has no such file, in the framework's default definition), so the
   store always matches the table that command creates."))


(defun make-db-session-store (&key (definition (session-table-definition))
                                   table-name
                                   id-column
                                   value-column
                                   last-access-column
                                   connector
                                   disconnector)
  "Create a session store backed by a database table.

   Every name defaults to the session table definition in force -- the
   project's db/session-table.lisp when it has one, the framework's default
   otherwise -- which is the same definition `db:create-session-table' renders
   its DDL from.  Editing the definition file therefore moves the table and the
   store together, and the arguments here are for the case of a table this
   framework did not create.

   CONNECTOR / DISCONNECTOR replace how a connection is obtained and released;
   by default one is borrowed from the connection pool (and returned to it), or
   -- when no pool has been started, as in a CLI command -- opened directly and
   closed.

   @param definition [list] Session table definition to take the names from
   @param table-name [(or string null)] Override the table name
   @param id-column [(or string null)] Override the session id column
   @param value-column [(or string null)] Override the session value column
   @param last-access-column [(or string null)] Override the last-access column
   @param connector [(or function null)] Function returning a connection
   @param disconnector [(or function null)] Function releasing a connection
   @return [<db-session-store>] The store
   "
  (make-instance '<db-session-store>
                 :table-name (or table-name (session-table-name definition))
                 :id-column (or id-column (session-table-column-name :id definition))
                 :value-column (or value-column (session-table-column-name :value definition))
                 :last-access-column (or last-access-column
                                         (session-table-column-name :last-access definition))
                 :connector connector
                 :disconnector disconnector))


(defun %open-connection (store)
  "Obtain a connection for one store operation.  See the file header."
  (let ((connector (db-session-store-connector store)))
    (cond (connector (funcall connector))
          ;; A pooled connection, borrowed straight from the pool rather than
          ;; through the per-thread map, so this is never the connection the
          ;; application's transaction is running on.
          (*connection-pool* (dbi-cp:get-connection *connection-pool*))
          ;; No pool: a CLI command or a test.  Direct connection, as the
          ;; migration code does.
          (t (get-connection-direct)))))

(defun %close-connection (store connection)
  "Release the connection one store operation used."
  (let ((disconnector (db-session-store-disconnector store)))
    (if disconnector
        (funcall disconnector connection)
        ;; One function for both kinds: on a pooled connection this returns it
        ;; to the pool, on a direct one it closes it.
        (dbi:disconnect connection))))

(defmacro with-store-connection ((connection store) &body body)
  "Run BODY with CONNECTION bound to a connection of STORE's own, released
   afterwards whatever happens."
  (let ((store-var (gensym "STORE")))
    `(let* ((,store-var ,store)
            (,connection (%open-connection ,store-var)))
       (unwind-protect
            (progn ,@body)
         (%close-connection ,store-var ,connection)))))

(defun %query-one-row (connection sql params)
  "Run SQL (one parameterised row) and return its columns as a list, or NIL
   when there is no row.

   The prepared query is freed afterwards, which DBI:DO-SQL does for itself but
   a hand-written SELECT has to do: of the three drivers, one allocates a
   statement in the database for every prepared query (PostgreSQL) and one
   allocates a foreign statement (SQLite3).  A session store prepares a query on
   every request, so what is a harmless omission in a one-off command would
   accumulate here for as long as a pooled connection lives."
  (let ((query (dbi:prepare connection sql)))
    (unwind-protect
         (dbi:fetch (dbi:execute query params) :format :values)
      (ignore-errors (dbi:free-query-resources query)))))

(defun %query-one-value (connection sql params)
  "Run SQL (one parameterised row, one column) and return the column, or NIL
   when there is no row."
  (first (%query-one-row connection sql params)))

(defun %row-exists-p (store connection sid)
  "True when STORE's table has a row for SID."
  (and (%query-one-row connection
                       (format nil "SELECT ~A FROM ~A WHERE ~A = ?"
                               (db-session-store-id-column store)
                               (db-session-store-table-name store)
                               (db-session-store-id-column store))
                       (list sid))
       t))


(defmethod fetch-session ((store <db-session-store>) sid)
  "Return the session stored under SID, rebuilt from its row, or NIL.

   Reading does not count as an access: recording one is TOUCH-SESSION's job,
   so that the session stage decides what activity means.

   Unlike the in-memory store this hands back a NEW hash-table on every call --
   a table is not a shared container.  See \"What a shared table does NOT give
   you\" in the file header.

   @param store [<db-session-store>] Session store instance
   @param sid [string] Session id
   @return [(or hash-table null)] The stored session, or NIL
   "
  (with-store-connection (connection store)
    (let ((text (%query-one-value connection
                                  (format nil "SELECT ~A FROM ~A WHERE ~A = ?"
                                          (db-session-store-value-column store)
                                          (db-session-store-table-name store)
                                          (db-session-store-id-column store))
                                  (list sid))))
      (when text
        (handler-case (deserialize-session text)
          (error (e)
            ;; A row that cannot be read back is treated as no session at all:
            ;; the request gets a fresh one instead of failing.  Rows like that
            ;; come from a store configured with a different serialisation, or
            ;; from a column truncated by the database.
            (warn "Ignoring the session stored under ~S: its stored value could not be read back (~A)."
                  sid e)
            nil))))))

(defmethod store-session ((store <db-session-store>) sid session)
  "Write SESSION into SID's row, inserting the row when it is not there yet,
   and record the write as an access.

   The UPDATE is issued even when the serialised session is byte for byte what
   the row already holds.  That is on purpose: skipping it -- as the underlying
   library's own database store does -- would leave the last-access time
   standing still for every request that only reads, and expire users who are
   actively browsing.

   @param store [<db-session-store>] Session store instance
   @param sid [string] Session id
   @param session [hash-table] Session contents to store
   @return [hash-table] SESSION
   "
  (let ((text (serialize-session session))
        (now (session-time-string (session-clock-now))))
    (with-store-connection (connection store)
      (flet ((update ()
               (dbi:do-sql connection
                 (format nil "UPDATE ~A SET ~A = ?, ~A = ? WHERE ~A = ?"
                         (db-session-store-table-name store)
                         (db-session-store-value-column store)
                         (db-session-store-last-access-column store)
                         (db-session-store-id-column store))
                 (list text now sid))))
        (if (%row-exists-p store connection sid)
            (update)
            (handler-case
                (dbi:do-sql connection
                  (format nil "INSERT INTO ~A (~A, ~A, ~A) VALUES (?, ?, ?)"
                          (db-session-store-table-name store)
                          (db-session-store-id-column store)
                          (db-session-store-value-column store)
                          (db-session-store-last-access-column store))
                  (list sid text now))
              (error ()
                ;; Another process inserted the same id between the check and
                ;; the insert.  The row exists now, so the write is an update.
                (update))))))
    session))

(defmethod remove-session ((store <db-session-store>) sid)
  "Delete SID's row.  Removing an id that is not there is not an error.

   @param store [<db-session-store>] Session store instance
   @param sid [string] Session id
   @return [integer] Rows deleted
   "
  (with-store-connection (connection store)
    (dbi:do-sql connection
      (format nil "DELETE FROM ~A WHERE ~A = ?"
              (db-session-store-table-name store)
              (db-session-store-id-column store))
      (list sid))))

(defmethod touch-session ((store <db-session-store>) sid &optional access-time)
  "Record an access to SID with ONE UPDATE of the last-access column.

   The session value column is not read, not written and not mentioned in the
   statement, which is the whole reason the fourth operation exists: the base
   class' default would fetch the session and write the whole thing back, on
   every request, including requests that only read.

   The time moves even when nothing in the session changed -- that is the
   defect this column exists to avoid (see the file header).

   @param store [<db-session-store>] Session store instance
   @param sid [string] Session id
   @param access-time [(or integer null)] Universal time, or NIL for now
   @return [(or integer null)] The recorded time, or NIL when SID is unknown
   "
  (let ((time (or access-time (session-clock-now))))
    (with-store-connection (connection store)
      (let ((updated (dbi:do-sql connection
                       (format nil "UPDATE ~A SET ~A = ? WHERE ~A = ?"
                               (db-session-store-table-name store)
                               (db-session-store-last-access-column store)
                               (db-session-store-id-column store))
                       (list (session-time-string time) sid))))
        (cond ((and (integerp updated) (plusp updated)) time)
              ;; MySQL reports zero rows CHANGED when the UPDATE wrote the
              ;; value the column already held, which happens when a session is
              ;; touched twice within one second.  Zero therefore does not mean
              ;; "no such session", and the difference matters: the caller reads
              ;; NIL as "this id holds nothing".
              ((%row-exists-p store connection sid) time)
              (t nil))))))

(defmethod session-last-access-time ((store <db-session-store>) sid)
  "Return when SID was last stored or touched, or NIL when it is unknown.

   @param store [<db-session-store>] Session store instance
   @param sid [string] Session id
   @return [(or integer null)] Universal time, or NIL
   "
  (with-store-connection (connection store)
    (session-time-from-database
     (%query-one-value connection
                       (format nil "SELECT ~A FROM ~A WHERE ~A = ?"
                               (db-session-store-last-access-column store)
                               (db-session-store-table-name store)
                               (db-session-store-id-column store))
                       (list sid)))))
