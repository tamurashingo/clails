# clails Controller Guide

## Overview

The clails Controller is an HTTP request handler inspired by Ruby on Rails' Controller.
Controllers receive HTTP requests, process data using Models, and pass data to Views or return JSON responses.

## Basic Concepts

- Controllers define processing for each HTTP method (GET, POST, PUT, DELETE)
- Action-based routing allows defining multiple actions (index, show, new, edit, etc.) in a single Controller
- The `resources` function provides concise RESTful route definitions
- There are `<web-controller>` for web applications and `<rest-controller>` for REST APIs
- Routing tables bind URL paths to Controllers
- URL parameters are automatically extracted and accessible from the Controller

---

## 1. Types of Controllers

clails has three types of Controller classes.

### `<base-controller>`

The base class for all Controllers. Provides basic HTTP request processing.

```common-lisp
(defclass <my-controller> (<base-controller>)
  ())
```

### `<web-controller>`

A Controller for web applications that render HTML views.

```common-lisp
(defclass <my-web-controller> (<web-controller>)
  ())
```

### `<rest-controller>`

A Controller for REST APIs that return structured data such as JSON.

```common-lisp
(defclass <my-api-controller> (<rest-controller>)
  ())
```

---

## 2. Defining Controllers

### Basic Controller Definition

```common-lisp
(in-package #:your-app/controller)

(defclass <users-controller> (<web-controller>)
  ()
  (:documentation "Users controller for managing user resources"))
```

### Implementing HTTP Method Handlers

Override methods corresponding to each HTTP method.

```common-lisp
;; GET request handling
(defmethod do-get ((controller <users-controller>))
  (let ((users (get-all-users)))
    (set-view controller "users/index.html" 
              `(:users ,users))))

;; POST request handling
(defmethod do-post ((controller <users-controller>))
  (let* ((name (param controller "name"))
         (email (param controller "email"))
         (user (create-user name email)))
    (set-redirect controller "/users")))

;; PUT request handling
(defmethod do-put ((controller <users-controller>))
  (let* ((id (param controller "id"))
         (name (param controller "name"))
         (user (update-user id name)))
    (set-view controller "users/show.html"
              `(:user ,user))))

;; DELETE request handling
(defmethod do-delete ((controller <users-controller>))
  (let ((id (param controller "id")))
    (delete-user id)
    (set-redirect controller "/users")))
```

### Sending PUT and DELETE Requests from HTML Forms

HTML `<form>` tags only support `GET` and `POST` methods natively.
To send PUT or DELETE requests from HTML forms, use the `_method` parameter.

**PUT Request Example:**

```html
<form action="/users/123" method="POST">
    <input type="hidden" name="_method" value="PUT">
    <input type="text" name="name" value="John Doe">
    <button type="submit">Update</button>
</form>
```

**DELETE Request Example:**

```html
<form action="/users/123" method="POST">
    <input type="hidden" name="_method" value="DELETE">
    <button type="submit">Delete</button>
</form>
```

clails checks the `_method` parameter in POST requests and routes as follows:

- When `_method` is `"PUT"` → calls the `do-put` method
- When `_method` is `"DELETE"` → calls the `do-delete` method
- When `_method` is not specified → calls the `do-post` method

This enables REST API-like operations from HTML forms.

---

## 3. Routing Configuration

Routing tables bind URL paths to Controllers.

### Defining Routing Tables

Define routes in `config/routes.lisp` or similar.

```common-lisp
(in-package #:your-app/config)

(setf clails/environment:*routing-tables*
  '(;; Top page
    (:path "/"
     :controller "your-app/controller::<top-controller>")
    
    ;; User list and creation
    (:path "/users"
     :controller "your-app/controller::<users-controller>")
    
    ;; User detail, update, and deletion
    (:path "/users/:id"
     :controller "your-app/controller::<user-controller>")
    
    ;; Nested resources
    (:path "/users/:user-id/posts/:post-id"
     :controller "your-app/controller::<user-posts-controller>")))

;; Initialize at application startup
(clails/controller/base-controller:initialize-routing-tables)
```

### Extracting URL Parameters

`:parameter-name` in URL paths are automatically extracted and accessible via the `param` function.

```common-lisp
;; Route definition: "/users/:user-id/posts/:post-id"
;; Access example: GET /users/123/posts/456

(defmethod do-get ((controller <user-posts-controller>))
  (let ((user-id (param controller "user-id"))   ; => "123"
        (post-id (param controller "post-id")))  ; => "456"
    ;; Processing...
    ))
```

### Action-Based Routing

By specifying `:action` and `:method` in a route, you can define multiple action methods in a single Controller.

```common-lisp
(setf clails/environment:*routing-tables*
  '((:path "/todos"
     :controller "your-app/controllers/todo-controller::<todo-controller>"
     :action "index"
     :method :get)
    (:path "/todos/:id"
     :controller "your-app/controllers/todo-controller::<todo-controller>"
     :action "show"
     :method :get)
    (:path "/todos/new"
     :controller "your-app/controllers/todo-controller::<todo-controller>"
     :action "new"
     :method :get)))
```

In the corresponding Controller, define action-named methods instead of `do-get`.

```common-lisp
(defclass <todo-controller> (<web-controller>)
  ())

(defmethod index ((controller <todo-controller>))
  (set-view controller "todos/index.html"))

(defmethod show ((controller <todo-controller>))
  (let ((id (param controller "id")))
    (set-view controller "todos/show.html" `(:id ,id))))

(defmethod new ((controller <todo-controller>))
  (set-view controller "todos/new.html"))
```

**Route matching priority:**

1. Routes where both path and HTTP method match (highest priority)
2. Routes where only the path matches and `:method` is not specified
3. No match → 404

**Backward compatibility:** Routes without `:action` continue to dispatch to `do-get`, `do-post`, etc. as before.

### RESTful Routing with the `resources` Function

The `resources` function lets you define 7 standard RESTful routes in a single line.

```common-lisp
(setf clails/environment:*routing-tables*
  `(,@(resources "todos" "your-app/controllers/todo-controller::<todo-controller>")
    ,@(resources "blogs" "your-app/controllers/blog-controller::<blog-controller>")))
```

`(resources "todos" "controller")` generates the following 7 routes:

| HTTP Method | Path | Action | Purpose |
|---|---|---|---|
| GET | /todos | index | List |
| GET | /todos/new | new | New form |
| POST | /todos | create | Create |
| GET | /todos/:id | show | Detail |
| GET | /todos/:id/edit | edit | Edit form |
| PUT | /todos/:id | update | Update |
| DELETE | /todos/:id | destroy | Delete |

`/todos/new` is placed before `/todos/:id`, so "new" will never be matched as an `:id` parameter.

#### `:only` Option — Generate Only Specific Actions

```common-lisp
;; Generate only index and show
(resources "todos" "controller" :only '(:index :show))
```

#### `:except` Option — Exclude Specific Actions

```common-lisp
;; Exclude destroy and update
(resources "todos" "controller" :except '(:destroy :update))
```

#### Controller Implementation Example with `resources`

```common-lisp
(defclass <todo-controller> (<web-controller>)
  ())

(defmethod index ((controller <todo-controller>))
  (let ((todos (get-all-todos)))
    (set-view controller "todos/index.html" `(:todos ,todos))))

(defmethod show ((controller <todo-controller>))
  (let ((todo (find-todo (param controller "id"))))
    (set-view controller "todos/show.html" `(:todo ,todo))))

(defmethod new ((controller <todo-controller>))
  (set-view controller "todos/new.html"))

(defmethod create ((controller <todo-controller>))
  (let ((title (param controller "title")))
    (create-todo title)
    (set-redirect controller "/todos")))

(defmethod edit ((controller <todo-controller>))
  (let ((todo (find-todo (param controller "id"))))
    (set-view controller "todos/edit.html" `(:todo ,todo))))

(defmethod update ((controller <todo-controller>))
  (let ((id (param controller "id"))
        (title (param controller "title")))
    (update-todo id title)
    (set-redirect controller (format nil "/todos/~A" id))))

(defmethod destroy ((controller <todo-controller>))
  (destroy-todo (param controller "id"))
  (set-redirect controller "/todos"))
```

---

## 4. Retrieving Request Parameters

### `param` Function

Retrieves request parameters (query parameters, POST data, URL parameters).

```common-lisp
(defmethod do-get ((controller <search-controller>))
  (let ((query (param controller "q"))
        (page (param controller "page")))
    ;; Search processing...
    ))
```

### Retrieving Form Data

Form data from POST requests can be retrieved the same way.

```common-lisp
(defmethod do-post ((controller <users-controller>))
  (let ((name (param controller "name"))
        (email (param controller "email"))
        (age (parse-integer (param controller "age"))))
    ;; User creation processing...
    ))
```

---

## 5. Rendering Views

### `set-view` Method

Renders a view template with data.

```common-lisp
(defmethod do-get ((controller <users-controller>))
  (let ((users (execute-query
                 (query <user>
                        :as :user
                        :order-by ((:user :created-at :desc)))
                 '())))
    (set-view controller "users/index.html"
              `(:users ,users))))
```

### Passing Data to Views

Data is passed to views as a property list.

```common-lisp
(defmethod do-get ((controller <user-controller>))
  (let* ((id (param controller "id"))
         (user (first (execute-query
                       (query <user>
                              :as :user
                              :where (:= (:user :id) :id))
                       (list :id id)))))
    (set-view controller "users/show.html"
              `(:user ,user
                :title ,(format nil "User: ~A" (ref user :name))
                :updated-at ,(ref user :updated-at)))))
```

---

## 6. Redirects

### `set-redirect` Method

Redirects to another URL.

```common-lisp
(defmethod do-post ((controller <users-controller>))
  (let* ((name (param controller "name"))
         (email (param controller "email"))
         (user (make-record '<user> :name name :email email)))
    (if (save user)
        ;; Redirect to user list on success
        (set-redirect controller "/users")
        ;; Return to form with errors on failure
        (set-view controller "users/new.html"
                  `(:errors ,(get-errors user))))))
```

### Redirect with Parameters

```common-lisp
(defmethod do-post ((controller <posts-controller>))
  (let ((post (create-post (param controller "title")
                          (param controller "body"))))
    ;; Redirect to post detail page
    (set-redirect controller 
                  (format nil "/posts/~A" (ref post :id)))))
```

---

## 7. REST API Responses

### `set-response` Method

Returns a JSON response. Available in `<rest-controller>`.

```common-lisp
(defclass <api-users-controller> (<rest-controller>)
  ())

(defmethod do-get ((controller <api-users-controller>))
  (let ((users (get-all-users)))
    (set-response controller
                  `((:status . "success")
                    (:count . ,(length users))
                    (:data . ,users)))))
```

### Setting HTTP Status Codes

Set status codes using the `code` slot.

```common-lisp
(defmethod do-post ((controller <api-users-controller>))
  (let* ((name (param controller "name"))
         (user (create-user name)))
    (if user
        (progn
          ;; 201 Created
          (setf (slot-value controller 'code) 201)
          (set-response controller
                        `((:status . "success")
                          (:data . ,user))))
        (progn
          ;; 400 Bad Request
          (setf (slot-value controller 'code) 400)
          (set-response controller
                        `((:status . "error")
                          (:message . "Failed to create user")))))))
```

### Common HTTP Status Codes

- `200` - OK (default)
- `201` - Created
- `204` - No Content
- `400` - Bad Request
- `401` - Unauthorized
- `403` - Forbidden
- `404` - Not Found
- `500` - Internal Server Error

---

## 8. Accessing Request Information

### `env` Method

Retrieves environment information from the request.

```common-lisp
(defmethod do-get ((controller <my-controller>))
  (let ((path (getf (env controller) :path-info))
        (method (getf (env controller) :request-method))
        (headers (getf (env controller) :headers)))
    ;; Processing...
    ))
```

### Common Environment Keys

- `:path-info` - Request path
- `:request-method` - HTTP method (`:GET`, `:POST`, etc.)
- `:query-string` - Query string
- `:content-type` - Content-Type header
- `:content-length` - Content-Length header
- `:headers` - All HTTP headers

---

## 9. Accessing Headers

### Retrieving Request Headers

```common-lisp
(defmethod do-get ((controller <my-controller>))
  (let* ((headers (getf (env controller) :headers))
         (auth-header (gethash "authorization" headers))
         (user-agent (gethash "user-agent" headers)))
    ;; Processing...
    ))
```

### Setting Response Headers

```common-lisp
(defmethod do-get ((controller <my-controller>))
  (setf (slot-value controller 'headers)
        '(("X-Custom-Header" . "Custom Value")
          ("Cache-Control" . "no-cache")))
  (set-response controller '((:status . "success"))))
```

---

## 10. Practical Examples

### CRUD Operations

```common-lisp
(defclass <users-controller> (<web-controller>)
  ())

;; List (GET /users)
(defmethod do-get ((controller <users-controller>))
  (let ((users (execute-query
                 (query <user>
                        :as :user
                        :order-by ((:user :name)))
                 '())))
    (set-view controller "users/index.html"
              `(:users ,users))))

;; Create (POST /users)
(defmethod do-post ((controller <users-controller>))
  (let* ((name (param controller "name"))
         (email (param controller "email"))
         (user (make-record '<user> :name name :email email)))
    (if (save user)
        (set-redirect controller "/users")
        (set-view controller "users/new.html"
                  `(:errors ,(get-errors user)
                    :user ,user)))))

(defclass <user-controller> (<web-controller>)
  ())

;; Detail (GET /users/:id)
(defmethod do-get ((controller <user-controller>))
  (let* ((id (param controller "id"))
         (user (first (execute-query
                       (query <user>
                              :as :user
                              :where (:= (:user :id) :id))
                       (list :id id)))))
    (if user
        (set-view controller "users/show.html"
                  `(:user ,user))
        (error '404/not-found :path (getf (env controller) :path-info)))))

;; Update (PUT /users/:id)
(defmethod do-put ((controller <user-controller>))
  (let* ((id (param controller "id"))
         (user (first (execute-query
                       (query <user>
                              :as :user
                              :where (:= (:user :id) :id))
                       (list :id id)))))
    (when user
      (setf (ref user :name) (param controller "name"))
      (setf (ref user :email) (param controller "email"))
      (if (save user)
          (set-redirect controller (format nil "/users/~A" id))
          (set-view controller "users/edit.html"
                    `(:errors ,(get-errors user)
                      :user ,user))))))

;; Delete (DELETE /users/:id)
(defmethod do-delete ((controller <user-controller>))
  (let* ((id (param controller "id"))
         (user (first (execute-query
                       (query <user>
                              :as :user
                              :where (:= (:user :id) :id))
                       (list :id id)))))
    (when user
      (destroy user)
      (set-redirect controller "/users"))))
```

### Transaction Handling

```common-lisp
(defmethod do-post ((controller <order-controller>))
  (handler-case
      (with-transaction
        (let* ((user-id (param controller "user-id"))
               (product-id (param controller "product-id"))
               (quantity (parse-integer (param controller "quantity")))
               (product (first (execute-query
                                (query <product>
                                       :as :product
                                       :where (:= (:product :id) :product-id))
                                (list :product-id product-id)))))
          
          ;; Check stock
          (unless (>= (ref product :stock) quantity)
            (error "Insufficient stock"))
          
          ;; Create order
          (let ((order (make-record '<order>
                                    :user-id user-id
                                    :product-id product-id
                                    :quantity quantity
                                    :total-price (* (ref product :price) quantity))))
            (save order)
            
            ;; Update stock
            (setf (ref product :stock) (- (ref product :stock) quantity))
            (save product)
            
            ;; Redirect on success
            (set-redirect controller (format nil "/orders/~A" (ref order :id))))))
    (error (e)
      ;; Handle error
      (setf (slot-value controller 'code) 400)
      (set-response controller
                    `((:status . "error")
                      (:message . ,(format nil "~A" e)))))))
```

### REST API Implementation Example

```common-lisp
(defclass <api-users-controller> (<rest-controller>)
  ())

;; List (GET /api/users)
(defmethod do-get ((controller <api-users-controller>))
  (let ((users (get-all-users)))
    (set-response controller
                  `((:status . "success")
                    (:count . ,(length users))
                    (:data . ,(mapcar #'user-to-json users))))))

;; Create (POST /api/users)
(defmethod do-post ((controller <api-users-controller>))
  (let* ((name (param controller "name"))
         (email (param controller "email"))
         (user (make-record '<user> :name name :email email)))
    (if (save user)
        (progn
          (setf (slot-value controller 'code) 201)
          (set-response controller
                        `((:status . "success")
                          (:data . ,(user-to-json user)))))
        (progn
          (setf (slot-value controller 'code) 400)
          (set-response controller
                        `((:status . "error")
                          (:errors . ,(get-errors-json user))))))))

(defun user-to-json (user)
  `((:id . ,(ref user :id))
    (:name . ,(ref user :name))
    (:email . ,(ref user :email))
    (:created-at . ,(format-datetime (ref user :created-at)))))
```

### Pagination

```common-lisp
(defmethod do-get ((controller <users-controller>))
  (let* ((page (or (parse-integer (param controller "page") :junk-allowed t) 1))
         (per-page 20)
         (offset (* (1- page) per-page))
         (users (execute-query
                  (query <user>
                         :as :user
                         :order-by ((:user :created-at :desc))
                         :limit per-page
                         :offset offset)
                  '()))
         (total-count (count-users)))
    (set-view controller "users/index.html"
              `(:users ,users
                :page ,page
                :per-page ,per-page
                :total-count ,total-count
                :total-pages ,(ceiling total-count per-page)))))
```

---

## 11. Error Handling

### 404 Not Found

By default, undefined HTTP methods raise a `404/not-found` error.

```common-lisp
;; If do-get is not implemented, automatic 404 error
(defclass <my-controller> (<base-controller>)
  ())

;; Custom 404 error
(defmethod do-get ((controller <my-controller>))
  (error '404/not-found :path (getf (env controller) :path-info)))
```

### Error Handling Controller

You can create a dedicated Controller to display error pages.

```common-lisp
(defclass <error-controller> (<web-controller>)
  ())

(defmethod do-get ((controller <error-controller>))
  (setf (slot-value controller 'code) 500)
  (set-view controller "errors/500.html"
            `(:message "Internal Server Error")))
```

---

## 12. Best Practices

### Controller Responsibilities

Controllers should only have the following responsibilities:

1. **Request Validation**: Check parameter existence and types
2. **Model Invocation**: Delegate business logic to Models
3. **Response Construction**: Set up Views or response data

### Separating Business Logic

Extract complex business logic into Models or Service classes.

```common-lisp
;; Bad example: Writing business logic in Controller
(defmethod do-post ((controller <order-controller>))
  (let ((product (find-product (param controller "product-id"))))
    ;; Complex calculations and validation...
    ))

;; Good example: Extract into Service class
(defmethod do-post ((controller <order-controller>))
  (let ((order-service (make-instance '<order-service>)))
    (create-order order-service
                  :user-id (param controller "user-id")
                  :product-id (param controller "product-id")
                  :quantity (param controller "quantity"))))
```

### Parameter Validation

Always validate parameters.

```common-lisp
(defmethod do-post ((controller <users-controller>))
  (let ((name (param controller "name"))
        (email (param controller "email")))
    ;; Validation
    (unless (and name email)
      (setf (slot-value controller 'code) 400)
      (set-response controller
                    `((:status . "error")
                      (:message . "Name and email are required")))
      (return-from do-post))
    
    ;; Continue processing...
    ))
```

### RESTful Design

REST APIs should follow RESTful design principles.

- `GET /users` - List
- `GET /users/:id` - Detail
- `POST /users` - Create
- `PUT /users/:id` - Update
- `DELETE /users/:id` - Delete

---

## Summary

clails Controllers have the following features:

1. **Simple Design**: Just define methods for each HTTP method
2. **RESTful Routing**: Rails-like route definitions with the `resources` function (supports `:only` / `:except` options)
3. **Flexible Routing**: Automatic URL parameter extraction and pattern matching
4. **View Integration**: Easy view rendering with `set-view`
5. **REST API Support**: Return JSON responses with `<rest-controller>`
6. **Transaction Support**: Transaction management in coordination with Models
7. **Backward Compatibility**: Traditional `do-get` / `do-post` style and new action-based style can coexist

For detailed API reference, please refer to the docstring of each function.
