(in-package #:cl-user)
(defpackage #:clails/middleware/session/table
  (:use #:cl)
  (:import-from #:clails/environment
                #:*database-type*
                #:*migration-base-dir*)
  (:import-from #:clails/model/connection
                #:with-db-connection-direct)
  ;; The dialect difference of the statement below -- "datetime" against
  ;; "timestamp" -- is answered by DDL-COLUMN-TYPE-IMPL, whose methods live in
  ;; clails/model/impl/{sqlite3,mysql,postgresql} and are loaded by
  ;; clails/model (which every project loads, and which the database
  ;; configuration this file needs comes from anyway).
  (:import-from #:clails/model/migration
                #:ddl-column-type-impl)
  (:import-from #:clails/logger
                #:log.sql)
  (:export #:+default-session-table-definition+
           #:+session-table-definition-path+
           #:session-table-definition
           #:session-table-definition-file
           #:read-session-table-definition
           #:check-session-table-definition
           #:session-table-name
           #:session-table-columns
           #:session-table-column
           #:session-table-column-name
           #:session-table-ddl
           #:ensure-session-table))
(in-package #:clails/middleware/session/table)


;;; ------------------------------------------------------------------
;;; The session table: its definition, its DDL, and how it is created
;;; ------------------------------------------------------------------
;;;
;;; M3-T3.  A session store that keeps sessions in the database needs a table
;;; to keep them in, and this file is how that table comes into existence:
;;; a definition (data), a renderer that turns it into one CREATE TABLE
;;; statement per dialect, and one function that runs it.  The CLI command
;;; `db:create-session-table' is a two-line wrapper around ENSURE-SESSION-TABLE.
;;;
;;; Why not a migration
;;; -------------------
;;; The obvious route -- ship a migration -- was measured and rejected, and the
;;; reasons are worth keeping because they are properties of the migration DSL,
;;; not opinions:
;;;
;;;   * CREATE-TABLE-IMPL inserts three columns into every table it makes (an
;;;     auto-increment integer id, created_at, updated_at) and there is no way
;;;     to suppress them.  An id column of its own would collide with the one
;;;     this table needs, and created_at / updated_at would be dead weight --
;;;     worse than dead weight, see "updated_at" below;
;;;   * its type map fixes :STRING at varchar(255).  A session id is 40
;;;     characters; a session value needs a long type;
;;;   * ADD-INDEX cannot ask for UNIQUE, and the :CONSTRAINTS argument of
;;;     CREATE-TABLE is logged and then ignored, so a unique constraint cannot
;;;     be expressed at all;
;;;   * there is no mechanism for running a framework-supplied migration.  The
;;;     job queue, the only precedent, writes a migration file INTO the
;;;     project and asks the user to run `db:migrate' -- two steps, and a file
;;;     in the project's migration history that the framework then cannot
;;;     change.
;;;
;;; A raw CREATE TABLE statement has none of those limits, and -- the deciding
;;; argument -- it is what the framework already does for its own bookkeeping:
;;; `db:create' creates the migration history table from a per-dialect raw DDL
;;; constant dispatched through a generic function, with
;;; CREATE TABLE IF NOT EXISTS in all three dialects, the whole difference
;;; between them being "datetime" against "timestamp".  This file is that same
;;; shape, one step further: the statement is rendered from a definition
;;; instead of being a constant, so that the project can see and edit it.
;;;
;;; Consequence, which is intended and must be documented for users: a table
;;; created here is NOT in the migration history and NOT in db/schema.lisp.
;;; `db:migrate' and `db:status' read the history table only, the schema file
;;; is written from the tables a migration run tracked, and model table
;;; information is collected per registered model -- so nothing in those three
;;; mechanisms notices this table, which is exactly why creating it does not
;;; disturb them.
;;;
;;; The definition file and its format
;;; ----------------------------------
;;; The definition is DATA -- a single plist, READ and never EVALuated -- and
;;; it lives in the project at db/session-table.lisp, next to db/seeds.lisp
;;; and db/migrate/.  `clails new' copies it there so that it can be read and
;;; edited; a project that does not have the file (one created before this
;;; existed) falls back to +DEFAULT-SESSION-TABLE-DEFINITION+ below, which is
;;; the same content, so the feature works either way.
;;;
;;; Why a .lisp file holding a plist and not a .sql file: there is not one
;;; single .sql file anywhere in this repository, and introducing the first one
;;; would mean either (a) three of them, one per dialect, with three copies of
;;; the same table to keep in step, or (b) one file in some dialect's spelling
;;; that the framework rewrites for the other two, which is a worse version of
;;; what this plist does.  An S-expression also lets the one real dialect
;;; difference (the datetime type name) be resolved at render time rather than
;;; duplicated, and it reads as the rest of db/ reads.  The format deliberately
;;; is NOT the migration DSL: there is no up/down, nothing is tracked, and the
;;; vocabulary is the vocabulary of a CREATE TABLE statement.
;;;
;;;   (:table "clails_sessions"
;;;    :columns ((:name "session_id" :role :id :type :string :length 40
;;;               :not-null t :primary-key t)
;;;              ...))
;;;
;;; Column keys: :NAME (string), :ROLE (:ID / :VALUE / :LAST-ACCESS -- how the
;;; store finds the column whatever it is called), :TYPE (a migration type
;;; keyword), :LENGTH (for :STRING), :NOT-NULL, :PRIMARY-KEY, :UNIQUE,
;;; :DEFAULT (only :CURRENT-TIMESTAMP).
;;;
;;; The three roles are the contract between this file and
;;; clails/middleware/session/store/db: the store's column names default to
;;; the names found here, so the store and the table cannot drift apart.


(defparameter +session-table-definition-path+ "db/session-table.lisp"
  "Where the session table definition lives inside a project, relative to the
   project root -- the same place and the same relative spelling db/seeds.lisp
   uses, and resolved against the same variable (*MIGRATION-BASE-DIR*).")

(defparameter +default-session-table-definition+
  '(:table "clails_sessions"
    :columns ((:name "session_id"
               :role :id
               :type :string
               :length 40
               :not-null t
               :primary-key t)
              (:name "session_value"
               :role :value
               :type :text
               :not-null t)
              (:name "last_access_at"
               :role :last-access
               :type :datetime
               :not-null t
               :default :current-timestamp)))
  "The session table as the framework defines it, used when the project has no
   db/session-table.lisp of its own.

   Kept identical to template/project/db/session-table.lisp.tmpl, which is the
   copy `clails new' puts in a project; a test reads the template and compares
   it to this value, so the two cannot drift apart.

   Why these columns:

     * session_id is varchar(40) because that is the length of the session id
       lack mints, NOT NULL and the PRIMARY KEY -- i.e. the uniqueness a
       session table has to have, written in the DDL rather than attempted
       through the migration DSL (which cannot express it);
     * session_value is a long type, because the 255 character limit of the
       migration DSL's string type is not a session;
     * last_access_at is the store-side last-access time of decision 11.  It
       is a column of its own ON PURPOSE, and updated_at is deliberately NOT
       used for it -- see the note on TOUCH-SESSION in
       clails/middleware/session/store/db for the two measured reasons.

   There is no index on last_access_at.  One would help the periodic sweep
   (M5-T3), and the renderer could emit it -- but CREATE INDEX IF NOT EXISTS
   does not exist in MySQL, so a second statement would cost either a
   dialect-specific existence check or a non-idempotent command.  The sweep is
   not this milestone's work, so the table is kept to the single idempotent
   statement and the index is left to the milestone that needs it.")


;;; ------------------------------------------------------------------
;;; Reading and checking a definition
;;; ------------------------------------------------------------------

(defun session-table-definition-file ()
  "Return the project's session table definition file, or NIL when there is
   none.

   Resolved against *MIGRATION-BASE-DIR*, which is where DB-SEED looks for
   db/seeds.lisp and DB-MIGRATE for db/migrate/: the project's db/ directory.

   @return [(or pathname null)] The file, when it exists
   "
  (when *migration-base-dir*
    (probe-file (format nil "~A/~A" *migration-base-dir* +session-table-definition-path+))))

(defun read-session-table-definition (file)
  "Read the session table definition in FILE and return it.

   The file is READ, not LOADed: it is data, so nothing in it is evaluated and
   *READ-EVAL* is bound off while it is read.  Everything after the first form
   is ignored, which is what lets the file carry trailing commentary.

   @param file [pathname] A definition file
   @return [list] The definition plist
   @condition error Signaled when the file holds no form, or holds something
     that is not a usable definition
   "
  (let ((definition (with-open-file (in file :direction :input :external-format :utf-8)
                      ;; WITH-STANDARD-IO-SYNTAX so that the file means the
                      ;; same thing whatever the reader happens to be set to
                      ;; (*PACKAGE* becomes COMMON-LISP-USER, so bare T is
                      ;; CL:T and :KEYWORDs are keywords), and *READ-EVAL* off
                      ;; so that a #. in the file is refused instead of run.
                      (with-standard-io-syntax
                        (let ((*read-eval* nil))
                          (read in nil nil))))))
    (when (null definition)
      (error "The session table definition file ~A is empty.  It must hold one ~
              form, a plist with :TABLE and :COLUMNS; see ~
              clails/middleware/session/table:+DEFAULT-SESSION-TABLE-DEFINITION+."
             file))
    (check-session-table-definition definition)
    definition))

(defun session-table-definition ()
  "Return the session table definition in force: the project's, when
   db/session-table.lisp exists, otherwise the framework's default.

   Everything that needs to know the shape of the table goes through here --
   the DDL renderer, the CLI command and the column defaults of
   clails/middleware/session/store/db:<DB-SESSION-STORE> -- so that a project
   that edits the file gets a store and a table that still agree.

   @return [list] The definition plist
   "
  (let ((file (session-table-definition-file)))
    (if file
        (read-session-table-definition file)
        +default-session-table-definition+)))

(defparameter +session-table-roles+ '(:id :value :last-access)
  "The roles a session table must fill: the id column, the column the
   serialised session goes in, and the last-access time column.  The store
   looks columns up by role, so a project may rename the columns.")

(defun check-session-table-definition (definition)
  "Signal an error unless DEFINITION is a usable session table definition.

   Checked here rather than at the first failing SQL statement, because the
   failure modes of a hand-edited definition (a missing role, a misspelled
   type) otherwise surface as a syntax error from the database with the
   rendered statement as the only clue.

   @param definition [list] A definition plist
   @return [list] DEFINITION
   @condition error Signaled with what is wrong when it is not usable
   "
  (unless (and (listp definition) (evenp (length definition)))
    (error "A session table definition must be a plist, got: ~S" definition))
  (let ((table (getf definition :table))
        (columns (getf definition :columns)))
    (unless (and (stringp table) (plusp (length table)))
      (error "A session table definition needs a :TABLE name (a string), got: ~S" table))
    (unless (and (listp columns) columns)
      (error "A session table definition needs a non-empty :COLUMNS list, got: ~S" columns))
    (dolist (column columns)
      (unless (and (listp column) (evenp (length column)))
        (error "Each :COLUMNS entry must be a plist, got: ~S" column))
      (let ((name (getf column :name)))
        (unless (and (stringp name) (plusp (length name)))
          (error "Each column needs a :NAME (a string), got: ~S" column))
        (unless (keywordp (getf column :type))
          (error "Column ~A needs a :TYPE keyword (:string, :text, :datetime, ...), got: ~S"
                 name (getf column :type)))))
    (dolist (role +session-table-roles+)
      (let ((found (remove-if-not (lambda (column) (eq (getf column :role) role)) columns)))
        (unless (= (length found) 1)
          (error "A session table definition needs exactly one column with :ROLE ~S, found ~D.  ~
                  The store finds its columns by role, so all of ~{~S~^, ~} must be present."
                 role (length found) +session-table-roles+)))))
  definition)


;;; ------------------------------------------------------------------
;;; Asking a definition about its columns
;;; ------------------------------------------------------------------

(defun session-table-name (&optional (definition (session-table-definition)))
  "The name of the session table DEFINITION describes.

   @param definition [list] A definition plist
   @return [string] Table name
   "
  (getf definition :table))

(defun session-table-columns (&optional (definition (session-table-definition)))
  "The column plists of DEFINITION, in DDL order.

   @param definition [list] A definition plist
   @return [list] List of column plists
   "
  (getf definition :columns))

(defun session-table-column (role &optional (definition (session-table-definition)))
  "The column plist of DEFINITION that fills ROLE (:ID, :VALUE or :LAST-ACCESS).

   @param role [keyword] Column role
   @param definition [list] A definition plist
   @return [list] The column plist
   @condition error Signaled when no column fills ROLE
   "
  (or (find role (session-table-columns definition) :key (lambda (column) (getf column :role)))
      (error "No column with :ROLE ~S in the session table definition for ~S."
             role (session-table-name definition))))

(defun session-table-column-name (role &optional (definition (session-table-definition)))
  "The name of the column of DEFINITION that fills ROLE.

   This is where the column defaults of the database session store come from,
   which is what keeps the store and the table in step.

   @param role [keyword] Column role
   @param definition [list] A definition plist
   @return [string] Column name
   "
  (getf (session-table-column role definition) :name))


;;; ------------------------------------------------------------------
;;; Rendering the DDL
;;; ------------------------------------------------------------------

(defun %column-ddl (column database-type)
  "Render one column of a CREATE TABLE statement for DATABASE-TYPE."
  (format nil "~A ~A~@[ ~A~]~@[ ~A~]~@[ ~A~]~@[ DEFAULT ~A~]"
          (getf column :name)
          (ddl-column-type-impl database-type (getf column :type)
                                :length (getf column :length))
          (when (getf column :not-null) "NOT NULL")
          (when (getf column :primary-key) "PRIMARY KEY")
          ;; PRIMARY KEY already is NOT NULL + UNIQUE in SQL; :UNIQUE is for a
          ;; column that must be unique without being the key, and writing both
          ;; on one column would ask MySQL and PostgreSQL for a second,
          ;; redundant unique index.
          (when (and (getf column :unique) (not (getf column :primary-key))) "UNIQUE")
          (case (getf column :default)
            ((nil) nil)
            (:current-timestamp "CURRENT_TIMESTAMP")
            (t (error "Unsupported :DEFAULT ~S on column ~A of the session table definition.  ~
                       Only :CURRENT-TIMESTAMP is understood."
                      (getf column :default) (getf column :name))))))

(defun session-table-ddl (&optional (definition (session-table-definition))
                                    (database-type *database-type*))
  "Render the CREATE TABLE statement for DEFINITION in DATABASE-TYPE's dialect.

   IF NOT EXISTS, because that is how the framework's own DDL is written in all
   three dialects, and it is what makes `db:create-session-table' safe to run
   twice: the second run changes nothing and keeps the first run's rows.

   One statement, deliberately: a single CREATE TABLE is idempotent in all
   three dialects without any existence check, which a secondary CREATE INDEX
   would not be (MySQL has no IF NOT EXISTS there).

   @param definition [list] A definition plist
   @param database-type [<database-type>] Database type instance
   @return [string] One SQL statement
   "
  (check-session-table-definition definition)
  (format nil "CREATE TABLE IF NOT EXISTS ~A (~{~A~^, ~})"
          (session-table-name definition)
          (mapcar (lambda (column) (%column-ddl column database-type))
                  (session-table-columns definition))))


;;; ------------------------------------------------------------------
;;; Creating the table
;;; ------------------------------------------------------------------

(defun ensure-session-table (&optional (definition (session-table-definition)))
  "Create the session table unless it is already there.

   The implementation of `db:create-session-table'.  Opens a DIRECT connection
   rather than taking one from the pool, because a command never starts the
   connection pool (only the server's runtime startup, `db:seed', `test' and
   `job:work' do) -- the same reason, and the same mechanism, as
   ENSURE-MIGRATION-TABLE in clails/model/migration.

   Running it twice does nothing the second time and destroys nothing: the
   statement is CREATE TABLE IF NOT EXISTS.

   @param definition [list] The definition to create the table from; defaults
     to the project's db/session-table.lisp, or the framework's default
   @return [string] The name of the table that now exists
   "
  (let ((ddl (session-table-ddl definition)))
    (log.sql ddl :table (session-table-name definition))
    (with-db-connection-direct (connection)
      (dbi:do-sql connection ddl))
    (session-table-name definition)))
