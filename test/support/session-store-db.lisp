(in-package #:cl-user)
(defpackage #:clails-test/support/session-store-db
  (:use #:cl
        #:rove
        #:clails-test/support/http
        #:clails-test/support/session-store)
  (:import-from #:clails/environment
                #:*migration-base-dir*)
  (:import-from #:clails/model/connection
                #:with-db-connection-direct
                #:startup-connection-pool
                #:shutdown-connection-pool)
  (:import-from #:clails/model/migration
                #:db-create
                #:db-migrate
                #:db-status)
  ;; The implementation of the `db:create-session-table' subcommand, so that
  ;; the command itself is what the idempotence check runs -- not just the
  ;; function underneath it.
  (:import-from #:clails/cmd
                #:db/create-session-table)
  (:import-from #:clails/middleware/session/store
                #:fetch-session
                #:store-session
                #:remove-session
                #:touch-session
                #:session-last-access-time)
  (:import-from #:clails/middleware/session/table
                #:+default-session-table-definition+
                #:+session-table-definition-path+
                #:session-table-definition
                #:session-table-definition-file
                #:session-table-name
                #:session-table-column-name
                #:session-table-ddl
                #:ensure-session-table)
  (:import-from #:clails/middleware/session/store/db
                #:make-db-session-store
                #:db-session-store-table-name
                #:db-session-store-id-column
                #:db-session-store-value-column
                #:db-session-store-last-access-column
                #:session-time-string)
  (:export #:prepare-session-table-database
           #:clear-session-rows
           #:stored-session-text
           #:assert-session-table-and-store-agree
           #:assert-session-table-creation-is-idempotent
           #:assert-session-store-three-operations
           #:assert-session-store-touches-only-the-time-column
           #:assert-session-timestamp-survives-the-database
           #:assert-session-table-constraints
           #:assert-session-store-roundtrip
           #:assert-session-store-works-without-the-pool
           #:assert-session-store-connections-do-not-accumulate
           #:assert-session-table-does-not-disturb-migrations
           #:assert-definition-file-is-used))
(in-package #:clails-test/support/session-store-db)


(defmacro ok-no-error (description &body body)
  "Assert that BODY runs without signalling, and say what it signalled if it did.

   Written as a macro rather than as (OK (HANDLER-CASE ... T) ...) because the
   obvious spelling of that is a trap: the error branch has to return something,
   and anything it returns other than NIL makes the assertion pass."
  (let ((value (gensym "VALUE"))
        (condition (gensym "CONDITION")))
    `(multiple-value-bind (,value ,condition)
         (handler-case (values (progn ,@body) nil)
           (error (e) (values nil e)))
       (declare (ignore ,value))
       (ok (null ,condition)
           (format nil "~A~@[ -- signalled: ~A~]" ,description ,condition)))))


;;; ------------------------------------------------------------------
;;; The checks every dialect of the database session store has to pass
;;; ------------------------------------------------------------------
;;;
;;; M3-T3.  The store, the table and the command that creates it are the same
;;; code for every dialect -- the only per-dialect part is the type name a
;;; column gets -- so the assertions live here once and the three suites in
;;; test/middleware/session-store/ supply the configuration and call them.
;;; That mirrors how the model layer is tested (one suite per dialect over one
;;; implementation) and makes a difference between dialects show up as one
;;; failing suite rather than as a difference between three hand-written tests.
;;;
;;; Each function configures nothing: the calling suite's SETUP has already
;;; chosen the dialect, created the database, created the session table and
;;; started the connection pool.


(defun prepare-session-table-database (&key (project-dir uiop:*temporary-directory*))
  "Create the database, the migration history table and the session table, and
   start the connection pool.  What a dialect suite's SETUP calls once the
   dialect is configured.

   PROJECT-DIR stands in for a project: *MIGRATION-BASE-DIR* (where the session
   table definition and the migrations would live) and *PROJECT-DIR* (where the
   schema dump is written) both point at it.  It holds no db/session-table.lisp,
   so the framework's own definition is what gets created -- which is also the
   case for a project that was created before that file existed.

   @param project-dir [pathname] Directory standing in for the project root
   @return [string] The name of the session table
   "
  (ensure-directories-exist (merge-pathnames "db/" project-dir))
  (setf clails/environment::*project-dir* project-dir)
  (setf clails/environment:*migration-base-dir* (namestring project-dir))
  (unless clails/environment:*project-name*
    (setf clails/environment:*project-name* "clails-test"))
  (db-create)
  (let ((table (ensure-session-table)))
    (startup-connection-pool)
    table))

(defun clear-session-rows (&optional (definition (session-table-definition)))
  "Delete every row of the session table.  rove runs SETUP once per file, so a
   test that counts rows clears them itself."
  (with-db-connection-direct (connection)
    (dbi:do-sql connection (format nil "DELETE FROM ~A" (session-table-name definition)))))

(defun session-row-count (&optional (definition (session-table-definition)))
  (with-db-connection-direct (connection)
    (let* ((query (dbi:prepare connection
                               (format nil "SELECT count(*) FROM ~A"
                                       (session-table-name definition))))
           (row (dbi:fetch (dbi:execute query '()) :format :values)))
      (first row))))

(defun stored-session-text (sid &optional (definition (session-table-definition)))
  "The session value column of SID's row, exactly as the database holds it.

   Used to show that TOUCH-SESSION does not go near that column: the text
   before and the text after are compared literally."
  (with-db-connection-direct (connection)
    (let* ((query (dbi:prepare connection
                               (format nil "SELECT ~A FROM ~A WHERE ~A = ?"
                                       (session-table-column-name :value definition)
                                       (session-table-name definition)
                                       (session-table-column-name :id definition))))
           (row (dbi:fetch (dbi:execute query (list sid)) :format :values))
           (value (first row)))
      (typecase value
        ((vector (unsigned-byte 8)) (babel:octets-to-string value))
        (t value)))))

(defun raw-last-access-value (sid &optional (definition (session-table-definition)))
  "The last-access column of SID's row exactly as the driver returns it --
   which is what the timestamp measurement reports."
  (with-db-connection-direct (connection)
    (let* ((query (dbi:prepare connection
                               (format nil "SELECT ~A FROM ~A WHERE ~A = ?"
                                       (session-table-column-name :last-access definition)
                                       (session-table-name definition)
                                       (session-table-column-name :id definition))))
           (row (dbi:fetch (dbi:execute query (list sid)) :format :values)))
      (first row))))

(defun count-idle-past (universal-time &optional (definition (session-table-definition)))
  "How many rows have a last-access time strictly before UNIVERSAL-TIME,
   compared BY THE DATABASE.

   This is the comparison an idle-timeout sweep will make (M5-T3), so whether
   the stored format can be compared in SQL is measured here rather than
   assumed."
  (with-db-connection-direct (connection)
    (let* ((query (dbi:prepare connection
                               (format nil "SELECT count(*) FROM ~A WHERE ~A < ?"
                                       (session-table-name definition)
                                       (session-table-column-name :last-access definition))))
           (row (dbi:fetch (dbi:execute query (list (session-time-string universal-time)))
                           :format :values)))
      (first row))))

(defun make-session (&rest key-value-pairs)
  (let ((session (make-hash-table :test 'equal)))
    (loop for (key value) on key-value-pairs by #'cddr
          do (setf (gethash key session) value))
    session))

(defun session-contents (session)
  "The contents of SESSION as a sorted alist, so two snapshots can be compared."
  (sort (loop for key being the hash-keys of session using (hash-value value)
              collect (cons key value))
        #'string< :key #'car))


;;; ------------------------------------------------------------------
;;; The table, and the store's agreement with it
;;; ------------------------------------------------------------------

(defun assert-session-table-and-store-agree (label)
  "The store's column names are the table's column names, and the DDL says
   what the store needs it to say.

   This is the condition that the store's defaults match the table the command
   creates.  It cannot be satisfied by coincidence here: both sides read the
   same definition, and that is what is being asserted."
  (let* ((definition (session-table-definition))
         (store (make-db-session-store))
         (ddl (session-table-ddl definition)))
    (ok (string= (db-session-store-table-name store) (session-table-name definition))
        (format nil "~A: the store uses the table the definition names (~A)"
                label (session-table-name definition)))
    (ok (string= (db-session-store-id-column store)
                 (session-table-column-name :id definition))
        (format nil "~A: the store's id column is the definition's" label))
    (ok (string= (db-session-store-value-column store)
                 (session-table-column-name :value definition))
        (format nil "~A: the store's value column is the definition's" label))
    (ok (string= (db-session-store-last-access-column store)
                 (session-table-column-name :last-access definition))
        (format nil "~A: the store's last-access column is the definition's" label))
    (dolist (column (list (db-session-store-id-column store)
                          (db-session-store-value-column store)
                          (db-session-store-last-access-column store)))
      (ok (search column ddl)
          (format nil "~A: the DDL defines ~A" label column)))
    ;; The store never reads or writes a column called updated_at, and the DDL
    ;; never creates one: the last-access time is a column of this store's own.
    ;; The two reasons are in the header of
    ;; clails/middleware/session/store/db.
    (ok (not (search "updated_at" ddl))
        (format nil "~A: the table has no updated_at column to be tempted by" label))
    (ok (not (string= (db-session-store-last-access-column store) "updated_at"))
        (format nil "~A: and the last-access column is not called updated_at" label))
    (ok (integerp (session-row-count definition))
        (format nil "~A: the table exists and can be counted" label))))

(defun assert-session-table-creation-is-idempotent (label)
  "One run of the command creates the table; a second run changes nothing and
   keeps the rows.

   The command itself is what is run here (CLAILS/CMD:DB/CREATE-SESSION-TABLE,
   which is all the `db:create-session-table' subcommand does after loading the
   project), so what is asserted is the two conditions the command has: ONE
   step -- no migration file, no second command -- and running it twice is
   harmless, which matters because the sessions of a running application are in
   that table.  The statement is CREATE TABLE IF NOT EXISTS, the same way the
   framework creates its own migration history table."
  (let ((table (session-table-name)))
    (with-db-connection-direct (connection)
      (dbi:do-sql connection (format nil "DROP TABLE IF EXISTS ~A" table)))
    (ok-no-error (format nil "~A: one run of the command creates the table" label)
      (db/create-session-table))
    (ok (eql (session-row-count) 0)
        (format nil "~A: and after that one run the table is there and empty" label)))

  (let ((store (make-db-session-store)))
    (store-session store "idempotent-sid" (make-session "kept" "yes"))
    (ok-no-error (format nil "~A: running the command again does not signal" label)
      (db/create-session-table))
    (ok-no-error (format nil "~A: and neither does a third time" label)
      (db/create-session-table))
    (let ((session (fetch-session store "idempotent-sid")))
      (ok (and (hash-table-p session) (equal (gethash "kept" session) "yes"))
          (format nil "~A: the session written before the second run is still there" label)))))


;;; ------------------------------------------------------------------
;;; The four operations
;;; ------------------------------------------------------------------

(defun assert-session-store-three-operations (label)
  "Store, fetch and remove, against a real table."
  (clear-session-rows)
  (let ((store (make-db-session-store)))
    (ok (null (fetch-session store "no-such-sid"))
        (format nil "~A: an unknown id holds nothing" label))

    (let ((session (make-session "user" "alice" "count" 3)))
      (store-session store "sid-1" session)
      (let ((fetched (fetch-session store "sid-1")))
        (ok (hash-table-p fetched)
            (format nil "~A: what was stored can be fetched" label))
        (ok (equal (session-contents fetched) (session-contents session))
            (format nil "~A: with the contents it was stored with" label))
        ;; Unlike the in-memory store, this one rebuilds the session: a NEW
        ;; table every fetch.  That is the documented difference, and the
        ;; reason the atomic-increment scenario expects what it expects.
        (ok (not (eq fetched session))
            (format nil "~A: as a rebuilt table rather than the same object" label))))

    (testing "values of several types survive the round trip"
      (let* ((long (make-string 2000 :initial-element #\x))
             (session (make-session "string" "text"
                                    "integer" 42
                                    "keyword" :a-keyword
                                    "list" (list 1 2 "three" :four)
                                    "long" long)))
        (store-session store "sid-types" session)
        (let ((fetched (fetch-session store "sid-types")))
          (ok (equal (gethash "string" fetched) "text"))
          (ok (eql (gethash "integer" fetched) 42))
          (ok (eq (gethash "keyword" fetched) :a-keyword))
          (ok (equal (gethash "list" fetched) (list 1 2 "three" :four)))
          ;; 2000 characters: proof that the value column is a long type and
          ;; not the 255 character string the migration DSL would have given.
          (ok (equal (gethash "long" fetched) long)
              (format nil "~A: a 2000 character value survives" label)))))

    (testing "storing again replaces the session"
      (store-session store "sid-1" (make-session "user" "bob"))
      (let ((fetched (fetch-session store "sid-1")))
        (ok (equal (gethash "user" fetched) "bob"))
        (ok (null (gethash "count" fetched))
            (format nil "~A: the previous contents are gone, not merged" label))))

    (testing "removing ends the session"
      (remove-session store "sid-1")
      (ok (null (fetch-session store "sid-1"))
          (format nil "~A: a removed id holds nothing" label))
      (ok-no-error (format nil "~A: removing an unknown id is not an error" label)
        (remove-session store "no-such-sid")))))

(defun assert-session-store-touches-only-the-time-column (label)
  "The fourth operation updates the time and NOTHING else.

   Two conditions in one: the session value column is untouched (compared as
   the literal text the database holds), and the time moves even though the
   session did not change -- which is the regression test for the defect that
   made a column of our own necessary (a store that skips the write when the
   value is unchanged leaves an active user's timestamp standing still)."
  (clear-session-rows)
  (let* ((store (make-db-session-store))
         (session (make-session "user" "carol" "n" 7)))
    (store-session store "touch-sid" session)
    (let ((text-before (stored-session-text "touch-sid"))
          (time-before (session-last-access-time store "touch-sid")))
      (ok (stringp text-before)
          (format nil "~A: the session is in the value column" label))
      (ok (integerp time-before)
          (format nil "~A: storing recorded a last-access time" label))

      ;; Touch with an explicit instant, well away from now, so the comparison
      ;; cannot pass by accident.
      (let* ((touched-at (+ time-before 3600))
             (answer (touch-session store "touch-sid" touched-at)))
        (ok (eql answer touched-at)
            (format nil "~A: touch-session answers with the time it recorded" label))
        (ok (eql (session-last-access-time store "touch-sid") touched-at)
            (format nil "~A: and that is what the column now holds" label))
        (ok (equal (stored-session-text "touch-sid") text-before)
            (format nil "~A: the value column is byte for byte what it was" label))
        (let ((fetched (fetch-session store "touch-sid")))
          (ok (equal (session-contents fetched) (session-contents session))
              (format nil "~A: so the session itself is unchanged" label))))

      ;; The condition B2 is about: nothing in the session changed, and the
      ;; time still moves.
      (let ((later (+ time-before 7200)))
        (touch-session store "touch-sid" later)
        (ok (eql (session-last-access-time store "touch-sid") later)
            (format nil "~A: an unchanged session still gets a later time" label)))

      ;; Touching twice within the same second must not be mistaken for
      ;; "there is no such session" (MySQL reports zero rows changed when an
      ;; UPDATE writes the value the column already holds).
      (let ((same (+ time-before 7200)))
        (ok (eql (touch-session store "touch-sid" same) same)
            (format nil "~A: touching with the time already stored still answers" label)))

      (ok (null (touch-session store "no-such-sid"))
          (format nil "~A: touching an unknown id does nothing and answers NIL" label))
      (ok (null (session-last-access-time store "no-such-sid"))
          (format nil "~A: an unknown id has no last-access time" label)))))


;;; ------------------------------------------------------------------
;;; The timestamp format (measurement C1)
;;; ------------------------------------------------------------------

(defun assert-session-timestamp-survives-the-database (label)
  "Write a time, read it back, and compare times IN THE DATABASE.

   The measurement the plan asked for: whether one spelling of a time
   (\"YYYY-MM-DD HH:MM:SS\", which is how the model layer writes a datetime in
   every dialect) survives storing, reading back and -- what an idle timeout
   needs -- comparing, in all three dialects.  The raw value the driver
   returned is reported, because that is the part that differs."
  (clear-session-rows)
  (let* ((store (make-db-session-store))
         ;; A fixed instant rather than NOW, so the assertion is exact.
         (instant (encode-universal-time 7 34 12 2 3 2026)))
    (store-session store "clock-sid" (make-session "k" "v"))
    (touch-session store "clock-sid" instant)
    (let ((read-back (session-last-access-time store "clock-sid"))
          (raw (raw-last-access-value "clock-sid")))
      (diag (format nil "~A: last_access_at came back from the driver as ~S (~A)~%"
                    label raw (type-of raw)))
      (ok (eql read-back instant)
          (format nil "~A: the time read back is the time written (~S vs ~S)"
                  label read-back instant))
      ;; Ordering, done by the database, is what a sweep will rely on.
      (ok (= (count-idle-past (+ instant 60)) 1)
          (format nil "~A: the database sees the row as older than a later instant" label))
      (ok (= (count-idle-past (- instant 60)) 0)
          (format nil "~A: and not older than an earlier one" label))
      (touch-session store "clock-sid" (+ instant 600))
      (ok (= (count-idle-past (+ instant 60)) 0)
          (format nil "~A: after a touch the row is no longer older than that instant" label))
      (ok (eql (session-last-access-time store "clock-sid") (+ instant 600))
          (format nil "~A: and the new time reads back exactly" label)))))


;;; ------------------------------------------------------------------
;;; The constraints the DDL writes directly
;;; ------------------------------------------------------------------

(defun assert-session-table-constraints (label)
  "The id column really is unique and the columns really are NOT NULL.

   Asserted against the database rather than against the text of the DDL,
   because what matters is that the constraint is enforced.  (It is written as
   PRIMARY KEY, which in SQL is NOT NULL + UNIQUE, and is how the framework's
   own migration history table writes the same thing -- the migration DSL can
   express neither.)"
  (clear-session-rows)
  (let* ((definition (session-table-definition))
         (table (session-table-name definition))
         (id (session-table-column-name :id definition))
         (value (session-table-column-name :value definition))
         (last-access (session-table-column-name :last-access definition))
         (now (session-time-string (get-universal-time))))
    (flet ((insert (sid value-text)
             (with-db-connection-direct (connection)
               (dbi:do-sql connection
                 (format nil "INSERT INTO ~A (~A, ~A, ~A) VALUES (?, ?, ?)"
                         table id value last-access)
                 (list sid value-text now))))
           (insert-null (null-column other-column other-value)
             ;; NULL is written into the statement rather than passed as a
             ;; parameter: the drivers do not agree on what a NIL parameter
             ;; means (cl-postgres sends it as a boolean, so it would land in
             ;; the column as text and never reach the constraint), and what is
             ;; being asserted is the constraint, not the driver.
             (with-db-connection-direct (connection)
               (dbi:do-sql connection
                 (format nil "INSERT INTO ~A (~A, ~A, ~A) VALUES (NULL, ?, ?)"
                         table null-column other-column last-access)
                 (list other-value now)))))
      (insert "unique-sid" "x")
      (ok (handler-case (progn (insert "unique-sid" "y") nil)
            (error () t))
          (format nil "~A: a second row with the same session id is refused" label))
      (ok (handler-case (progn (insert-null id value "y") nil)
            (error () t))
          (format nil "~A: a row without a session id is refused" label))
      (ok (handler-case (progn (insert-null value id "null-value-sid") nil)
            (error () t))
          (format nil "~A: a row without a session value is refused" label)))))


;;; ------------------------------------------------------------------
;;; Through the session stage
;;; ------------------------------------------------------------------

(defun assert-session-store-roundtrip (label)
  "The shared round-trip body (M3-T2) over a real table: the stage mints a
   session, the application's value is in the table, later requests with the
   same cookie see it, and dropping the row ends the session."
  (clear-session-rows)
  (let ((sid (run-session-store-roundtrip (make-db-session-store) :label label)))
    (ok (and sid (= (length sid) 40))
        (format nil "~A: the round trip ran to the end" label))
    ;; 40 characters is also what the id column was sized for.
    (ok (= (length sid) 40)
        (format nil "~A: and the session id fits the varchar(40) id column" label))))

(defun assert-session-store-works-without-the-pool (label)
  "The store and the table-creating command work with no connection pool.

   A CLI command never starts the pool (only the server's runtime startup,
   db:seed, test and job:work do), so the command that creates the table and a
   store used outside a server must both open a connection of their own.  The
   pool is shut down for the duration of this check and started again
   afterwards."
  (clear-session-rows)
  (shutdown-connection-pool)
  (unwind-protect
       (progn
         (ok (null clails/environment:*connection-pool*)
             (format nil "~A: no connection pool is running" label))
         (ok-no-error (format nil "~A: the table can be created without a pool" label)
           (ensure-session-table))
         (let ((store (make-db-session-store)))
           (store-session store "no-pool-sid" (make-session "k" "v"))
           (let ((fetched (fetch-session store "no-pool-sid")))
             (ok (and (hash-table-p fetched) (equal (gethash "k" fetched) "v"))
                 (format nil "~A: store and fetch work without a pool" label)))
           (ok (integerp (touch-session store "no-pool-sid" 3000000000))
               (format nil "~A: and so does the fourth operation" label))
           (remove-session store "no-pool-sid")
           (ok (null (fetch-session store "no-pool-sid"))
               (format nil "~A: and so does remove" label))))
    (startup-connection-pool)))

(defun assert-session-store-connections-do-not-accumulate (label &key (trips 30))
  "Many round trips through the stage do not use up connections.

   The pool holds ten connections and refuses to hand out an eleventh (after a
   wait), so a store that forgot to give a connection back would fail -- or
   stall -- long before the end of this loop.  Each round trip is a fetch and a
   store, i.e. two connections borrowed and returned."
  (clear-session-rows)
  (let ((store (make-db-session-store))
        (last-body nil))
    (ok-no-error
        (format nil "~A: ~D round trips in a row, each borrowing and returning connections"
                label trips)
      (with-middleware-stack (app :store store :inner (make-session-counter-app))
        (let ((client (make-session-client app)))
          (dotimes (index trips)
            (declare (ignore index))
            (client-request client "/"))
          (setf last-body (response-body-string (client-request client "/"))))))
    ;; The counter also shows that every one of those trips really read the
    ;; session back: a connection that was not returned would have stalled the
    ;; loop long before this.
    (ok (equal last-body (princ-to-string (1+ trips)))
        (format nil "~A: and the ~Dth request saw all ~D earlier ones" label (1+ trips) trips))))


;;; ------------------------------------------------------------------
;;; Not disturbing the migration machinery
;;; ------------------------------------------------------------------

(defun assert-session-table-does-not-disturb-migrations (label)
  "The session table is invisible to migrate / status / the schema dump.

   Keeping it out of the migration history is a decision, and this is the
   check that the decision costs nothing: the history table is untouched,
   db:status and db:migrate run, and the schema dump -- which is written from
   the tables a migration run tracked -- does not mention the session table."
  (let ((migration-rows
          (with-db-connection-direct (connection)
            (let* ((query (dbi:prepare connection "SELECT count(*) FROM migration"))
                   (row (dbi:fetch (dbi:execute query '()) :format :values)))
              (first row)))))
    (ensure-session-table)
    (let ((after
            (with-db-connection-direct (connection)
              (let* ((query (dbi:prepare connection "SELECT count(*) FROM migration"))
                     (row (dbi:fetch (dbi:execute query '()) :format :values)))
                (first row)))))
      (ok (eql migration-rows after)
          (format nil "~A: creating the session table adds nothing to the migration history"
                  label)))
    (ok-no-error (format nil "~A: db:status still runs" label)
      (db-status))
    (ok-no-error (format nil "~A: db:migrate still runs" label)
      (db-migrate))
    (let ((schema-file (merge-pathnames "db/schema.lisp" clails/environment::*project-dir*)))
      (ok (probe-file schema-file)
          (format nil "~A: db:migrate wrote the schema dump" label))
      (when (probe-file schema-file)
        (ok (not (search (session-table-name) (uiop:read-file-string schema-file)))
            (format nil "~A: and the session table is not in it (it is not a migrated table)"
                    label))))))


;;; ------------------------------------------------------------------
;;; The definition file is really what gets created
;;; ------------------------------------------------------------------

(defun assert-definition-file-is-used (label)
  "A db/session-table.lisp in the project decides the table and the columns.

   The end of the chain this milestone builds: the file is read, the statement
   is rendered from it, the table it names is the table that appears, and a
   store built from the same definition uses it.  Proven by renaming
   everything -- a table and columns with these names exist nowhere else."
  (let* ((file (merge-pathnames (format nil "~A" +session-table-definition-path+)
                                (uiop:ensure-directory-pathname *migration-base-dir*)))
         (definition '(:table "clails_sessions_renamed"
                       :columns ((:name "sid" :role :id :type :string :length 40
                                  :not-null t :primary-key t)
                                 (:name "payload" :role :value :type :text :not-null t)
                                 (:name "seen_at" :role :last-access :type :datetime
                                  :not-null t)))))
    (ensure-directories-exist file)
    (unwind-protect
         (progn
           (with-open-file (out file :direction :output :if-exists :supersede
                                     :external-format :utf-8)
             (format out ";;;; written by clails-test/support/session-store-db~%~S~%" definition))
           (ok (equal (namestring (session-table-definition-file))
                      (namestring (probe-file file)))
               (format nil "~A: the project's definition file is found" label))
           (ok (equal (session-table-definition) definition)
               (format nil "~A: and it is the definition in force" label))
           (ok (string= (ensure-session-table) "clails_sessions_renamed")
               (format nil "~A: the command creates the table the file names" label))
           (let ((store (make-db-session-store)))
             (ok (string= (db-session-store-table-name store) "clails_sessions_renamed")
                 (format nil "~A: a store built now uses that table" label))
             (ok (and (string= (db-session-store-id-column store) "sid")
                      (string= (db-session-store-value-column store) "payload")
                      (string= (db-session-store-last-access-column store) "seen_at"))
                 (format nil "~A: with the columns the file renamed" label))
             (store-session store "renamed-sid" (make-session "k" "v"))
             (let ((fetched (fetch-session store "renamed-sid")))
               (ok (and (hash-table-p fetched) (equal (gethash "k" fetched) "v"))
                   (format nil "~A: and the renamed table round-trips a session" label)))
             (ok (integerp (touch-session store "renamed-sid" 3000000000))
                 (format nil "~A: including the fourth operation" label))
             (with-db-connection-direct (connection)
               (dbi:do-sql connection "DROP TABLE clails_sessions_renamed"))))
      (delete-file file)
      ;; Back to the framework's definition for whatever runs next.
      (ok (equal (session-table-definition) +default-session-table-definition+)
          (format nil "~A: without the file, the framework's definition is used again" label)))))
