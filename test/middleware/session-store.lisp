(in-package #:cl-user)
(defpackage #:clails-test/middleware/session-store
  (:use #:cl
        #:rove
        #:clails-test/support/clock
        #:clails-test/support/session-store)
  (:import-from #:clails-test/support/http
                #:with-middleware-stack
                #:call-app
                #:make-session-client
                #:client-request
                #:make-session-counter-app
                #:response-body-string
                #:response-session-id
                #:make-memory-session-store
                #:fetch-stored-session)
  (:import-from #:clails/middleware/session/store
                #:<session-store>
                #:fetch-session
                #:store-session
                #:remove-session
                #:touch-session
                #:session-last-access-time)
  (:import-from #:clails/middleware/session/store/memory
                #:<memory-session-store>))
(in-package #:clails-test/middleware/session-store)


;;; M3-T1: the session store base class, its three operations, and the fourth
;;; operation (update only the last-access time).


(defparameter +idle-timeout+ 1800
  "Idle limit, in seconds, used by these tests only.  The framework gains its
   own setting in M5; here it is just a number to judge expiry against.")

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
;;; The three operations
;;; ------------------------------------------------------------------

(deftest session-store-base-class-test
  (testing "the base class is abstract: the three operations must be implemented"
    (let ((store (make-instance '<session-store>)))
      (ok (handler-case (progn (fetch-session store "sid") nil)
            (error () t))
          "fetch-session signals on the base class")
      (ok (handler-case (progn (store-session store "sid" (make-session)) nil)
            (error () t))
          "store-session signals on the base class")
      (ok (handler-case (progn (remove-session store "sid") nil)
            (error () t))
          "remove-session signals on the base class")))

  (testing "the memory store is a derivation of the base class"
    (ok (typep (make-memory-session-store) '<memory-session-store>))
    (ok (typep (make-memory-session-store) '<session-store>))))


(deftest memory-store-three-operations-test
  (let ((store (make-memory-session-store)))
    (testing "an unknown id holds nothing"
      (ok (null (fetch-session store "unknown"))))

    (testing "what is stored is what is fetched, as the same object"
      (let ((session (make-session "user" "alice")))
        (store-session store "sid-1" session)
        (ok (eq (fetch-session store "sid-1") session))
        (ok (string= (gethash "user" (fetch-session store "sid-1")) "alice"))))

    (testing "ids are independent of each other"
      (store-session store "sid-2" (make-session "user" "bob"))
      (ok (string= (gethash "user" (fetch-session store "sid-1")) "alice"))
      (ok (string= (gethash "user" (fetch-session store "sid-2")) "bob")))

    (testing "storing again replaces the previous value"
      (store-session store "sid-1" (make-session "user" "carol"))
      (ok (string= (gethash "user" (fetch-session store "sid-1")) "carol")))

    (testing "removing an id forgets it, and the other ids are untouched"
      (remove-session store "sid-1")
      (ok (null (fetch-session store "sid-1")))
      (ok (string= (gethash "user" (fetch-session store "sid-2")) "bob")))

    (testing "removing an id that is not there is not an error"
      (ok (handler-case (progn (remove-session store "never-stored") t)
            (error () nil))))))


;;; ------------------------------------------------------------------
;;; Acceptance by the lack session stage
;;; ------------------------------------------------------------------

(deftest session-store-lack-bridge-test
  ;; This is the measurement the whole design rests on, kept as a test so a
  ;; later change to either side cannot silently invalidate it.
  (testing "a clails CLOS store cannot be a subtype of lack's store structure"
    (ok (handler-case
            (progn (eval '(defclass clails-test-store-metaclass-probe
                              (lack/middleware/session/store:store)
                            ()))
                   nil)
          (error () t))
        "defclass on lack's store structure signals (incompatible metaclasses)"))

  (testing "a clails CLOS store is nevertheless accepted by the lack session stage"
    ;; lack never checks the type of its :store, it only calls the three
    ;; generic functions; the bridge methods in
    ;; clails/middleware/session/store answer them.
    (let ((store (make-memory-session-store)))
      (with-middleware-stack (app :store store
                                  :inner (make-session-counter-app))
        (let* ((client (make-session-client app))
               (first-response (client-request client "/"))
               (sid (response-session-id first-response)))
          (ok (string= (response-body-string first-response) "1"))
          (ok (string= (response-body-string (client-request client "/")) "2"))
          (ok (hash-table-p (fetch-stored-session store sid))
              "the stage stored the session in the CLOS store")
          (ok (eql (gethash "counter" (fetch-stored-session store sid)) 2)))))))


;;; ------------------------------------------------------------------
;;; The fourth operation on the memory derivation
;;; ------------------------------------------------------------------

;;; A memory store that counts whole-value writes.  This is how the test
;;; distinguishes "the timestamp was updated" from "the whole session was
;;; written back again".
(defclass <counting-memory-store> (<memory-session-store>)
  ((stores :initform 0 :accessor counting-store-writes)
   (fetches :initform 0 :accessor counting-store-reads)))

(defmethod store-session :after ((store <counting-memory-store>) sid session)
  (declare (ignore sid session))
  (incf (counting-store-writes store)))

(defmethod fetch-session :after ((store <counting-memory-store>) sid)
  (declare (ignore sid))
  (incf (counting-store-reads store)))


(deftest memory-store-touch-test
  (testing "storing a session counts as an access"
    (let ((store (make-memory-session-store))
          (before (get-universal-time)))
      (store-session store "sid" (make-session))
      (let ((recorded (session-last-access-time store "sid")))
        (ok (integerp recorded) "a stored session has a last-access time")
        (ok (<= before recorded (get-universal-time))))))

  (testing "reading a session does not move its last-access time"
    (let ((store (make-memory-session-store)))
      (store-session store "sid" (make-session))
      (let ((recorded (session-last-access-time store "sid")))
        (fetch-session store "sid")
        (ok (eql (session-last-access-time store "sid") recorded)))))

  (testing "touching records the given time and nothing else"
    (let ((store (make-memory-session-store))
          (session (make-session "user" "alice")))
      (store-session store "sid" session)
      (ok (eql (touch-session store "sid" 3000000000) 3000000000)
          "touch-session answers the time it recorded")
      (ok (eql (session-last-access-time store "sid") 3000000000))
      (ok (eq (fetch-session store "sid") session)
          "the session object is still the very same object")))

  (testing "touching with no time given records now"
    (let ((store (make-memory-session-store))
          (before nil))
      (store-session store "sid" (make-session))
      (touch-session store "sid" 3000000000)
      (setf before (get-universal-time))
      (let ((recorded (touch-session store "sid")))
        (ok (<= before recorded (get-universal-time))))))

  (testing "touching an unknown id does nothing and creates nothing"
    (let ((store (make-memory-session-store)))
      (ok (null (touch-session store "unknown")))
      (ok (null (session-last-access-time store "unknown")))
      (ok (null (fetch-session store "unknown")))))

  (testing "a removed session has no last-access time"
    (let ((store (make-memory-session-store)))
      (store-session store "sid" (make-session))
      (remove-session store "sid")
      (ok (null (session-last-access-time store "sid")))))

  (testing "the memory derivation writes no value back: it replaces its index entry"
    (let ((store (make-instance '<counting-memory-store>))
          (session (make-session "user" "alice" "cart" 3)))
      (store-session store "sid" session)
      (let ((writes (counting-store-writes store))
            (reads (counting-store-reads store)))
        (touch-session store "sid" 3000000000)
        (ok (= (counting-store-writes store) writes)
            "store-session was not called by touch-session")
        (ok (= (counting-store-reads store) reads)
            "fetch-session was not called by touch-session either")
        (ok (eql (session-last-access-time store "sid") 3000000000)
            "and yet the last-access time did move")))))


(deftest memory-store-touch-leaves-value-alone-test
  ;; Decision 11: the timestamp is store-side state.  Nothing about the table
  ;; the application reads and writes may change -- not its contents, not its
  ;; size, not its identity.
  (testing "the session value is identical after a touch"
    (let* ((store (make-memory-session-store))
           (session (make-session "user" "alice" "cart" 3)))
      (store-session store "sid" session)
      (let ((contents-before (session-contents session))
            (count-before (hash-table-count session)))
        (touch-session store "sid" 3000000000)
        (touch-session store "sid" 3000000001)
        (let ((fetched (fetch-session store "sid")))
          (ok (eq fetched session) "same object")
          (ok (= (hash-table-count fetched) count-before)
              "no key was added to the session")
          (ok (equal (session-contents fetched) contents-before)
              "no value in the session changed")))))

  (testing "no framework key sneaks into the session over a real round trip"
    (let ((store (make-memory-session-store)))
      (with-middleware-stack (app :store store
                                  :inner (make-session-counter-app))
        (let* ((client (make-session-client app))
               (sid (response-session-id (client-request client "/"))))
          (touch-session store sid)
          (client-request client "/")
          (touch-session store sid)
          (let ((session (fetch-stored-session store sid)))
            (ok (equal (mapcar #'car (session-contents session)) (list "counter"))
                "the session holds the application's key and nothing else")))))))


;;; ------------------------------------------------------------------
;;; The fourth operation through the base class' default implementation
;;; ------------------------------------------------------------------

(deftest default-touch-implementation-test
  (testing "the default implementation is fetch -> change -> store"
    ;; <recording-session-store> implements the three operations only; it has
    ;; no touch-session method at all.
    (let ((store (make-recording-session-store)))
      (store-session store "sid" (make-session "user" "alice"))
      (clear-store-calls store)
      (let ((recorded (touch-session store "sid" 3000000000)))
        (ok (eql recorded 3000000000) "the default answers the recorded time")
        (ok (equal (store-calls store)
                   (list (cons :fetch "sid") (cons :store "sid")))
            "the naive default fetched the session and stored it back"))))

  (testing "a derivation with no fourth operation still gets a last-access time"
    (let ((store (make-recording-session-store)))
      (store-session store "sid" (make-session))
      (touch-session store "sid" 3000000000)
      (ok (eql (session-last-access-time store "sid") 3000000000))
      (touch-session store "sid" 3000000060)
      (ok (eql (session-last-access-time store "sid") 3000000060)
          "a later touch moves the time forward")))

  (testing "the default implementation does not change the session value"
    (let* ((store (make-recording-session-store))
           (session (make-session "user" "alice" "cart" 3))
           (contents-before (session-contents session)))
      (store-session store "sid" session)
      (touch-session store "sid" 3000000000)
      (let ((fetched (fetch-session store "sid")))
        (ok (eq fetched session))
        (ok (equal (session-contents fetched) contents-before))
        (ok (= (hash-table-count fetched) 2)))))

  (testing "the default implementation does nothing for an unknown id"
    (let ((store (make-recording-session-store)))
      (clear-store-calls store)
      (ok (null (touch-session store "unknown")))
      (ok (= (store-call-count store :fetch) 1))
      (ok (= (store-call-count store :store) 0)
          "nothing is written for an id that holds no session")
      (ok (null (session-last-access-time store "unknown")))))

  (testing "the base class forgets the time of a removed session"
    (let ((store (make-recording-session-store)))
      (store-session store "sid" (make-session))
      (touch-session store "sid")
      (ok (integerp (session-last-access-time store "sid")))
      (remove-session store "sid")
      (ok (null (session-last-access-time store "sid"))))))


(deftest default-touch-expiry-test
  ;; G2 must not be weakened: a store whose author never implemented the
  ;; fourth operation has to expire just as well as one that did.  Expiry
  ;; itself is M5; what is asserted here is that the information it needs is
  ;; there and moves.  REFERENCE-IDLE-EXPIRED-P is the stand-in predicate from
  ;; clails-test/support/clock.
  (dolist (entry (list (cons "memory derivation (own fourth operation)"
                             (make-memory-session-store))
                       (cons "minimal derivation (default fourth operation)"
                             (make-recording-session-store))))
    (destructuring-bind (label . store) entry
      (testing label
        (store-session store "sid" (make-session))
        (testing "a session touched long ago is judged idle"
          (touch-session store "sid" (- (get-universal-time) (* 2 +idle-timeout+)))
          (ok (reference-idle-expired-p (session-last-access-time store "sid")
                                        +idle-timeout+)))
        (testing "touching it again makes it active"
          (touch-session store "sid")
          (ok (not (reference-idle-expired-p (session-last-access-time store "sid")
                                             +idle-timeout+))))
        (testing "and letting the clock run past the limit expires it again"
          (touch-session store "sid")
          (with-clock-advanced-by ((- +idle-timeout+ 60))
            (ok (not (reference-idle-expired-p (session-last-access-time store "sid")
                                               +idle-timeout+))))
          (with-clock-advanced-by ((+ +idle-timeout+ 60))
            (ok (reference-idle-expired-p (session-last-access-time store "sid")
                                          +idle-timeout+)))
          (with-clock-advanced-by ((* 3600 24 21))
            (ok (reference-idle-expired-p (session-last-access-time store "sid")
                                          +idle-timeout+))))))))
