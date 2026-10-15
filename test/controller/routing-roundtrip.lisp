(in-package #:cl-user)
(defpackage #:clails-test/controller/routing-roundtrip
  (:use #:cl
        #:rove
        #:clails-test/support/http
        #:clails-test/support/routing)
  (:import-from #:clails/environment
                #:*routing-tables*)
  (:import-from #:clails/controller/base-controller
                #:<rest-controller>
                #:set-response
                #:param
                #:resources)
  (:import-from #:clails/middleware/clails-middleware
                #:*lack-middleware-clails-controller*))
(in-package #:clails-test/controller/routing-roundtrip)


;;; Round trips that go through route resolution.  Until now every routing
;;; test called the matching function directly; these go through the request
;;; stack instead, which is what M9 needs in order to judge "a form can reach
;;; the update and destroy routes of a resource".


(defclass <todo-controller> (<rest-controller>)
  ())

(defgeneric index (controller))
(defmethod index ((controller <todo-controller>))
  (set-response controller (list (cons "action" "index"))))

(defgeneric show (controller))
(defmethod show ((controller <todo-controller>))
  (set-response controller (list (cons "action" "show")
                                 (cons "id" (param controller "id")))))

(defgeneric create (controller))
(defmethod create ((controller <todo-controller>))
  (set-response controller (list (cons "action" "create"))))

(defgeneric update (controller))
(defmethod update ((controller <todo-controller>))
  (set-response controller (list (cons "action" "update")
                                 (cons "id" (param controller "id")))))

(defgeneric destroy (controller))
(defmethod destroy ((controller <todo-controller>))
  (set-response controller (list (cons "action" "destroy")
                                 (cons "id" (param controller "id")))))


(defparameter +todo-controller+
  "clails-test/controller/routing-roundtrip::<todo-controller>")

(defun todo-routes (&rest args)
  "The default resource routes of \"todos\", pointing at <todo-controller>."
  (apply #'resources "todos" +todo-controller+ args))

(defmacro with-routed-app ((app routes &key (session nil)) &body body)
  "Install ROUTES, build a stack whose innermost stage is the clails
   controller stage, and bind APP to it.

   The application below the controller stage answers 404: that is where the
   controller stage delegates when no route matches."
  `(with-routing-table (,routes)
     (with-middleware-stack (,app :session ,session
                                  :inner #'not-found-app
                                  :inner-middleware (list *lack-middleware-clails-controller*))
       ,@body)))

(defun action-of (response)
  "The action name reported by <todo-controller> in a JSON response."
  (let ((body (response-body-string response)))
    (cond ((search "\"index\"" body) :index)
          ((search "\"show\"" body) :show)
          ((search "\"create\"" body) :create)
          ((search "\"update\"" body) :update)
          ((search "\"destroy\"" body) :destroy)
          (t nil))))


(deftest resources-routing-roundtrip-test
  (testing "the routes generated for a resource can be reached over HTTP"
    (with-routed-app (app (todo-routes :except '(:new :edit)))
      (let ((response (call-app app "/todos")))
        (ok (= (response-status response) 200))
        (ok (string= (response-header response :content-type) "application/json"))
        (ok (eq (action-of response) :index)))
      (ok (eq (action-of (call-app app "/todos" :method :post)) :create))
      (ok (eq (action-of (call-app app "/todos/7")) :show))
      (ok (eq (action-of (call-app app "/todos/7" :method :put)) :update))
      (ok (eq (action-of (call-app app "/todos/7" :method :delete)) :destroy))))

  (testing "a path parameter arrives in the controller"
    (with-routed-app (app (todo-routes :only '(:show)))
      (ok (search "\"id\":\"42\"" (response-body-string (call-app app "/todos/42"))))))

  (testing "an unrouted path falls through to the application below"
    (with-routed-app (app (todo-routes))
      (ok (= (response-status (call-app app "/nothing-here")) 404))))

  (testing ":only keeps the other routes out"
    (with-routed-app (app (todo-routes :only '(:index)))
      (ok (= (response-status (call-app app "/todos")) 200))
      (ok (= (response-status (call-app app "/todos" :method :post)) 404))))

  (testing "a routed round trip can carry a session as well"
    (with-routed-app (app (todo-routes :only '(:index)) :session t)
      (let ((response (call-app app "/todos")))
        (ok (= (response-status response) 200))
        (ok (response-header response :set-cookie))
        (ok (= (length (response-session-id response)) 40))))))


(deftest hand-written-routing-roundtrip-test
  ;; A form that posts to a member path and spoofs the method in a hidden
  ;; field needs both a POST and a PUT route on the same path.  That shape has
  ;; to be expressible by hand, because it is the regression the e2e template
  ;; defect (R34) is about.
  (testing "the same path carries both a POST and a PUT route"
    (with-routed-app (app (list (list :path "/items"
                                      :controller +todo-controller+
                                      :action "create"
                                      :method :post)
                                (list :path "/items"
                                      :controller +todo-controller+
                                      :action "update"
                                      :method :put)))
      (ok (eq (action-of (call-app app "/items" :method :post)) :create))
      (ok (eq (action-of (call-app app "/items" :method :put)) :update))
      ;; a method with no route on that path is not routed at all
      (ok (= (response-status (call-app app "/items" :method :delete)) 404))))

  (testing "hand-written routes and generated routes can be mixed"
    (with-routed-app (app (append (list (list :path "/items/:id"
                                              :controller +todo-controller+
                                              :action "update"
                                              :method :post))
                                  (todo-routes :only '(:index))))
      (ok (eq (action-of (call-app app "/items/3" :method :post)) :update))
      (ok (search "\"id\":\"3\"" (response-body-string (call-app app "/items/3" :method :post))))
      (ok (eq (action-of (call-app app "/todos")) :index)))))


(deftest routing-table-isolation-test
  (testing "a table installed by a test is gone afterwards"
    (let ((paths-before (router-paths))
          (tables-before *routing-tables*))
      (with-routed-app (app (todo-routes :only '(:index)))
        (ok (member "/todos" (router-paths) :test #'string=))
        (ok (= (response-status (call-app app "/todos")) 200)))
      (ok (equal (router-paths) paths-before))
      (ok (eq *routing-tables* tables-before))))

  (testing "two tables in a row do not see each other's routes"
    (with-routed-app (app (todo-routes :only '(:index)))
      (ok (= (response-status (call-app app "/todos")) 200))
      (ok (= (response-status (call-app app "/notes")) 404)))
    (with-routed-app (app (resources "notes" +todo-controller+ :only '(:index)))
      (ok (= (response-status (call-app app "/notes")) 200))
      (ok (= (response-status (call-app app "/todos")) 404))))

  (testing "the order the tables are installed in does not change the results"
    (labels ((todos-case ()
               (with-routed-app (app (todo-routes :only '(:index)))
                 (list (response-status (call-app app "/todos"))
                       (response-status (call-app app "/notes")))))
             (notes-case ()
               (with-routed-app (app (resources "notes" +todo-controller+ :only '(:index)))
                 (list (response-status (call-app app "/notes"))
                       (response-status (call-app app "/todos"))))))
      (let ((forwards (list (todos-case) (notes-case)))
            (backwards (reverse (list (notes-case) (todos-case)))))
        (ok (equal forwards backwards))
        (ok (equal forwards (list (list 200 404) (list 200 404))))))))
