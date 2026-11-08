# clails View Guide

## Overview

The clails View is a template engine similar to ERB (Embedded Ruby) or JSP (JavaServer Pages).
It allows you to embed Common Lisp code within HTML to generate dynamic web pages.

## Basic Concepts

- View templates are HTML files with embedded Common Lisp code
- Expressions enclosed with `<%=` and `%>` output evaluation results
- Code enclosed with `<%` and `%>` executes Common Lisp code (without output)
- Data passed from Controllers is accessible via the `view` function
- Templates are compiled and cached for fast execution
- Each directory requires a `package.lisp` file (package-inferred-system)

### About Frontend Development

The clails template engine provides simple server-side rendering.
For modern frontend development (React, Vue.js, etc.), we recommend using clails as a REST API backend
and building the frontend as a separate project.

Therefore, complex mechanisms like layout templates are not provided.
Each view should be written as complete HTML or used in combination with frontend frameworks.

---

## 1. View Template Basics

### File Placement

View templates are placed in the `app/views/` directory.

```
app/views/
├── package.lisp        # Top-level package definition (auto-generated)
├── index.html          # Top page
└── users/
    ├── package.lisp    # users directory package definition (auto-generated)
    ├── index.html      # User list
    ├── show.html       # User detail
    └── new.html        # User creation form
```

### About Package Files

Since clails adopts **package-inferred-system**, each View directory **requires** a `package.lisp`.

The package in which a View template is evaluated is automatically determined from its file path.

**Examples**:
- `app/views/index.html` → Package: `your-app/views/package`
- `app/views/todo/show.html` → Package: `your-app/views/todo/package`
- `app/views/admin/users/list.html` → Package: `your-app/views/admin/users/package`

When generating Views with the `clails generate:view` command, `package.lisp` is automatically created
and loading code is added to `app/application.lisp`.

If creating Views manually, the following steps are required:

1. Create `package.lisp` in that directory
2. Add loading code to `app/application.lisp`

**Important**: Placing Views in directories without `package.lisp` will result in package-not-found errors.

### Basic Template

```html
<!DOCTYPE html>
<html>
<head>
  <title>Welcome</title>
</head>
<body>
  <h1>Hello, World!</h1>
  <p>This is a static content.</p>
</body>
</html>
```

---

## 2. Embedding Data

### Expression Embedding (`<%=` `%>`)

Expressions enclosed with `<%=` and `%>` output their evaluation results to HTML.

```html
<!DOCTYPE html>
<html>
<head>
  <title><%= (view :title) %></title>
</head>
<body>
  <h1><%= (view :heading) %></h1>
  <p>Welcome, <%= (view :username) %>!</p>
</body>
</html>
```

Passing data from Controller:

```common-lisp
(defmethod do-get ((controller <users-controller>))
  (set-view controller "users/index.html"
            `(:title "User List"
              :heading "Users"
              :username "Taro")))
```

### `view` Function

Use the `view` function to access data passed from the Controller.

```html
<h1><%= (view :title) %></h1>
<p><%= (view :message) %></p>
<div>Count: <%= (view :count) %></div>
```

---

## 3. Control Structures

### Scriptlets (`<%` `%>`)

Code enclosed with `<%` and `%>` executes Common Lisp code (without output).

```html
<ul>
  <% (loop for user in (view :users)
           do %>
    <li><%= (getf user :name) %></li>
  <% ) %>
</ul>
```

### Conditional Branching

```html
<% (if (view :is-admin) %>
  <div class="admin-panel">
    <h2>Admin Panel</h2>
    <p>You have admin privileges.</p>
  </div>
<% ) %>

<% (if (> (view :count) 0) %>
  <p>There are <%= (view :count) %> items.</p>
<% ) %>
```

You can also use `when` or `unless`:

```html
<% (when (view :show-message) %>
  <div class="alert">
    <%= (view :message) %>
  </div>
<% ) %>

<% (unless (view :hide-footer) %>
  <footer>
    <p>&copy; 2024 Your App</p>
  </footer>
<% ) %>
```

### Iteration

#### Using `loop`

```html
<h2>User List</h2>
<ul>
  <% (loop for user in (view :users)
           do %>
    <li>
      <strong><%= (getf user :name) %></strong>
      (<%= (getf user :email) %>)
    </li>
  <% ) %>
</ul>
```

#### Using `dolist`

```html
<table>
  <thead>
    <tr>
      <th>ID</th>
      <th>Name</th>
      <th>Email</th>
    </tr>
  </thead>
  <tbody>
    <% (dolist (user (view :users)) %>
      <tr>
        <td><%= (getf user :id) %></td>
        <td><%= (getf user :name) %></td>
        <td><%= (getf user :email) %></td>
      </tr>
    <% ) %>
  </tbody>
</table>
```

### Structured Tags (`<cl:xxx>`)

In addition to scriptlets, clails provides structured XML-style tags for cleaner control flow syntax.

#### `<cl:loop>` Tag

The `<cl:loop>` tag provides a cleaner alternative to scriptlet-based loops:

```html
<h2>User List</h2>
<ul>
  <cl:loop for="user" in="(view :users)">
    <li>
      <strong><%= (getf user :name) %></strong>
      (<%= (getf user :email) %>)
    </li>
  </cl:loop>
</ul>
```

Attributes:
- `for` - Loop variable name
- `in` - Expression that returns a list to iterate over

#### `<cl:if>` Tag with `<cl:else>`

The `<cl:if>` tag provides conditional rendering with optional else clause:

```html
<cl:if test="(view :is-admin)">
  <div class="admin-panel">
    <h2>Admin Panel</h2>
    <p>You have admin privileges.</p>
  </div>
  <cl:else>
    <div class="user-panel">
      <h2>User Panel</h2>
      <p>Limited access.</p>
    </div>
  </cl:else>
</cl:if>
```

Attributes:
- `test` - Conditional expression to evaluate

#### `<cl:when>` Tag

The `<cl:when>` tag renders content only when the test is true:

```html
<cl:when test="(view :show-message)">
  <div class="alert">
    <%= (view :message) %>
  </div>
</cl:when>
```

Attributes:
- `test` - Conditional expression to evaluate

#### `<cl:unless>` Tag

The `<cl:unless>` tag renders content only when the test is false:

```html
<cl:unless test="(view :hide-footer)">
  <footer>
    <p>&copy; 2024 Your App</p>
  </footer>
</cl:unless>
```

Attributes:
- `test` - Conditional expression to evaluate

#### `<cl:cond>` Tag

The `<cl:cond>` tag supports multiple conditional branches:

```html
<cl:cond>
  <cl:when test="(> (view :score) 90)">
    <div class="grade-a">Excellent!</div>
  </cl:when>
  <cl:when test="(> (view :score) 70)">
    <div class="grade-b">Good!</div>
  </cl:when>
  <cl:when test="(> (view :score) 50)">
    <div class="grade-c">Fair</div>
  </cl:when>
  <cl:otherwise>
    <div class="grade-f">Need Improvement</div>
  </cl:otherwise>
</cl:cond>
```

The `<cl:otherwise>` clause is optional and acts as the default case.

#### Complex Example with Structured Tags

```html
<h1>Shopping Cart</h1>

<cl:if test="(view :items)">
  <table class="cart">
    <thead>
      <tr>
        <th>Item</th>
        <th>Price</th>
        <th>Status</th>
      </tr>
    </thead>
    <tbody>
      <cl:loop for="item" in="(view :items)">
        <tr>
          <td><%= (getf item :name) %></td>
          <td>$<%= (getf item :price) %></td>
          <td>
            <cl:cond>
              <cl:when test="(getf item :in-stock)">
                <span class="status-ok">In Stock</span>
              </cl:when>
              <cl:when test="(getf item :backordered)">
                <span class="status-warning">Backordered</span>
              </cl:when>
              <cl:otherwise>
                <span class="status-error">Out of Stock</span>
              </cl:otherwise>
            </cl:cond>
          </td>
        </tr>
      </cl:loop>
    </tbody>
  </table>
  
  <cl:when test="(> (view :total) 100)">
    <div class="discount-notice">
      Free shipping on orders over $100!
    </div>
  </cl:when>
<cl:else>
  <p>Your cart is empty.</p>
</cl:else>
</cl:if>
```

---

## 4. Displaying Model Data

### Rendering Model Instances

Display Model data retrieved in the Controller in the View.

Controller:

```common-lisp
(defmethod do-get ((controller <users-controller>))
  (let ((users (execute-query
                 (query <user>
                        :as :user
                        :order-by ((:user :name)))
                 '())))
    (set-view controller "users/index.html"
              `(:users ,users))))
```

View (`users/index.html`):

```html
<!DOCTYPE html>
<html>
<head>
  <title>User List</title>
</head>
<body>
  <h1>Users</h1>
  <table>
    <thead>
      <tr>
        <th>ID</th>
        <th>Name</th>
        <th>Email</th>
        <th>Created At</th>
      </tr>
    </thead>
    <tbody>
      <% (dolist (user (view :users)) %>
        <tr>
          <td><%= (ref user :id) %></td>
          <td><%= (ref user :name) %></td>
          <td><%= (ref user :email) %></td>
          <td><%= (ref user :created-at) %></td>
        </tr>
      <% ) %>
    </tbody>
  </table>
</body>
</html>
```

### Accessing Model Properties

Use the `ref` function to access Model properties.

```html
<div class="user">
  <h2><%= (ref (view :user) :name) %></h2>
  <p>Email: <%= (ref (view :user) :email) %></p>
  <p>Age: <%= (ref (view :user) :age) %></p>
</div>
```

---

## 5. Forms

### Form helpers

A project generated by `clails new` has **CSRF verification on**, so every
POST / PUT / PATCH / DELETE must carry the session's token in a hidden field,
and a form that means PUT, PATCH or DELETE must say so in a second hidden
field. The helpers in `clails/view/form-helper` write both; writing them by
hand is how a page ends up with a 403 or a 404.

#### Reaching them

Two ways, and both are fine:

1. **Import them into the view namespace.** This is what a generated
   `package.lisp` does — both `app/views/package.lisp` and each
   `app/views/<resource>/package.lisp` written by `clails generate:view`:

   ```common-lisp
   (:import-from #:clails/view/form-helper
                 #:form-tag
                 #:end-form-tag
                 #:form-with
                 #:form-authenticity-token
                 #:csrf-token-tag
                 #:method-override-tag)
   ```

2. **Import nothing** and write the package prefix in the template:
   `<%= (clails/view/form-helper:form-tag :url "/users") %>`.

#### What each one does

| Call | Result |
|---|---|
| `(form-tag &key url method multipart attributes)` | The opening `<form>` tag plus the hidden fields it needs. `url` `NIL` (the default) writes no `action`, i.e. submits to the current URL. `method` defaults to `:post` and is one of `:get :post :put :patch :delete`. `multipart` `T` adds `enctype="multipart/form-data"`; the token still travels as a top-level field. `attributes` is a plist of further attributes on the `<form>`, e.g. `(:class "edit" :id "user-form")` — `NIL` values are left out and `T` writes a boolean attribute |
| `(end-form-tag)` | `"</form>"`. A function rather than a literal so that the two halves of a form read as a pair |
| `(form-with &key url method multipart attributes body)` | `form-tag`, `body`, `end-form-tag` in one expression. `body` takes a string, a list of strings (concatenated in order), or `NIL` for an empty form. Handy for a delete button, which is a form with nothing in it but its submit control |
| `(form-authenticity-token)` | The token itself, as a string — for a JavaScript caller that wants to put it in a header |
| `(csrf-token-tag)` | Just the hidden token field |
| `(method-override-tag :put)` | Just the hidden `_method` field; `""` for `:get` and `:post`, which need none |

Asking for the token is what **mints** one if this session has none yet, and
every call within one request returns the same string — so several forms on one
page all carry the same token.

#### When they work

These are template helpers, for two separate reasons: the session comes from
the controller being rendered, which is bound only while a template runs, and
the token comes from a dynamic variable the CSRF stage binds for the duration
of one request. Both hold inside a template, because rendering happens at the
innermost stage of the stack and the CSRF stage is outside it. Elsewhere:

- **CSRF verification off?** `(csrf-token-tag)` returns `""` and
  `(form-authenticity-token)` returns `NIL`, so a template using them keeps
  working in a project that has not switched verification on.
- **Outside a template?** A `view-context-error` naming the helper.
- **Inside a template, but rendered by a stack that has no CSRF stage while
  `*enable-csrf-middleware*` is `T`?** A `form-helper-error` that says exactly
  that, rather than an `unbound-variable` surfacing out of a dependency.

#### A GET form carries no token, on purpose

`form-tag` omits the token field for `:method :get`. A GET form's fields are
appended to the URL as a query string, so the token would end up in the
browser's history, in the `Referer` header of every link followed from the next
page, and in every access log on the way. The CSRF stage does not verify GET
anyway, so there is nothing lost.

For the same reason the `_method` field is written into the form's **body** and
never into the action URL's query string: the override stage reads the body
only, because a method in a query string would turn any link — or any image
tag — into a deletion.

#### Reaching the request from a template

The renderer also binds the controller being rendered, which these read:

| Call | Returns |
|---|---|
| `(current-controller)` | The controller whose action is being rendered |
| `(current-request)` | Its request object |
| `(current-env)` | Its request environment plist |
| `(current-param name)` | One request parameter |
| `(current-session)` | Its session hash table |

They are exported by `clails/view/view-helper`, and each signals a
`view-context-error` naming the helper when there is no rendering in progress,
rather than returning `NIL`.

> **Do not print what `(current-session)` contains.** That hash table holds the
> framework's own reserved entries alongside the application's — keys beginning
> with `clails.`, **the CSRF token among them** (`clails.csrf-token`). Walking
> the table and rendering it leaks the token into the page. Read the keys your
> application wrote, by name.

#### One more thing that changed

**An action that calls neither `set-view` nor `set-redirect` now returns 200
with an empty body**, where it used to fail. That is friendlier, but it also
means a form posting to such an action *looks* like it worked — if a submission
returns 200 and a blank page, check the action rather than the form.


### Basic Form

```html
<%= (form-tag :url "/users" :method :post) %>
  <div>
    <label>Name:</label>
    <input type="text" name="name" required>
  </div>
  <div>
    <label>Email:</label>
    <input type="email" name="email" required>
  </div>
  <div>
    <label>Age:</label>
    <input type="number" name="age">
  </div>
  <button type="submit">Create User</button>
<%= (end-form-tag) %>
```

Written by hand, the same form is a `<form method="POST" action="/users">` with
no token field — which **a project with CSRF verification on answers with a
403**. A generated project has it on, so use the helper.

### Edit Form

```html
<% (let ((user (view :user))) %>
  <%= (form-tag :url (format nil "/users/~A" (ref user :id)) :method :put) %>
    <div>
      <label>Name:</label>
      <input type="text" name="name" value="<%= (ref user :name) %>" required>
    </div>
    <div>
      <label>Email:</label>
      <input type="email" name="email" value="<%= (ref user :email) %>" required>
    </div>
    <div>
      <label>Age:</label>
      <input type="number" name="age" value="<%= (ref user :age) %>">
    </div>
    <button type="submit">Update User</button>
  <%= (end-form-tag) %>
<% ) %>
```

`:method :put` is what makes the helper write **two** hidden fields: the CSRF
token and `_method=put`. The browser still sends a POST; the override stage
rewrites it before the route is matched, so the form reaches the resource's
`update` route.

**Writing `<input type="hidden" name="_method" value="PUT">` by hand still
works** — it is the same field, read by the same stage — but such a form also
needs `<%= (csrf-token-tag) %>` next to it, and the action URL has to be the
member path (`/users/<id>`), not the collection path. The helper gets both
right. Note that the value is read from the form's **body**: putting `_method`
in the action URL's query string does nothing, on purpose.

### Displaying Validation Errors

```html
<% (let ((user (view :user))) %>
  <% (when (has-error-p user) %>
    <div class="errors">
      <h3>Errors:</h3>
      <ul>
        <% (when (ref-error user :name) %>
          <li>Name: <%= (ref-error user :name) %></li>
        <% ) %>
        <% (when (ref-error user :email) %>
          <li>Email: <%= (ref-error user :email) %></li>
        <% ) %>
      </ul>
    </div>
  <% ) %>
  
  <%= (form-tag :url "/users" :method :post) %>
    <!-- form fields -->
  <%= (end-form-tag) %>
<% ) %>
```

---

## 6. Nested Data and Relationships

### Displaying Related Data

```html
<!-- Display company with departments -->
<% (let ((company (view :company))) %>
  <div class="company">
    <h1><%= (ref company :name) %></h1>
    
    <h2>Departments:</h2>
    <ul>
      <% (dolist (dept (ref company :departments)) %>
        <li>
          <strong><%= (ref dept :name) %></strong>
          - <%= (ref dept :location) %>
        </li>
      <% ) %>
    </ul>
  </div>
<% ) %>
```

### Multi-level Nesting

```html
<!-- Display company with departments and employees -->
<% (let ((company (view :company))) %>
  <h1><%= (ref company :name) %></h1>
  
  <% (dolist (dept (ref company :departments)) %>
    <div class="department">
      <h2><%= (ref dept :name) %></h2>
      
      <h3>Employees:</h3>
      <ul>
        <% (dolist (emp (ref dept :employees)) %>
          <li>
            <%= (ref emp :name) %> - <%= (ref emp :position) %>
          </li>
        <% ) %>
      </ul>
    </div>
  <% ) %>
<% ) %>
```

---

## 7. Helper Functions

### Defining Helper Functions

Helper functions can be defined in the package file.

`app/views/users/package.lisp`:

```common-lisp
(in-package #:cl-user)
(defpackage #:your-app/views/users/package
  (:use #:cl)
  (:import-from #:clails/view/view-helper
                #:view))

(in-package #:your-app/views/users/package)

(defun format-date (timestamp)
  "Format timestamp as YYYY-MM-DD"
  (when timestamp
    (local-time:format-timestring 
      nil timestamp 
      :format '(:year "-" (:month 2) "-" (:day 2)))))

(defun user-status-class (status)
  "Return CSS class based on user status"
  (case status
    (:active "status-active")
    (:inactive "status-inactive")
    (:pending "status-pending")
    (t "status-unknown")))
```

Using helpers in templates:

```html
<% (dolist (user (view :users)) %>
  <div class="<%= (user-status-class (ref user :status)) %>">
    <h3><%= (ref user :name) %></h3>
    <p>Joined: <%= (format-date (ref user :created-at)) %></p>
  </div>
<% ) %>
```

---

## 8. Advanced Package Management

### Package Definition Structure

Each View directory has an independent package definition.

`app/views/users/package.lisp`:

```common-lisp
(in-package #:cl-user)
(defpackage #:your-app/views/users/package
  (:use #:cl)
  ;; Import view helper
  (:import-from #:clails/view/view-helper
                #:*view-context*
                #:view)
  ;; Import the form helpers (a generated package.lisp already has this)
  (:import-from #:clails/view/form-helper
                #:form-tag
                #:end-form-tag
                #:form-with
                #:form-authenticity-token
                #:csrf-token-tag
                #:method-override-tag)
  ;; Import model functions
  (:import-from #:your-app/model
                #:ref
                #:has-error-p
                #:ref-error))

(in-package #:your-app/views/users/package)
```

### Importing External Functions

#### 1. Importing Model Functions

```common-lisp
(defpackage #:your-app/views/users/package
  (:use #:cl)
  (:import-from #:clails/view/view-helper #:view)
  (:import-from #:your-app/model
                #:ref
                #:has-error-p
                #:ref-error
                #:save
                #:destroy))
```

#### 2. Using Helper Functions from Other Packages

```common-lisp
(defpackage #:your-app/views/users/package
  (:use #:cl)
  (:import-from #:clails/view/view-helper #:view)
  (:import-from #:your-app/helpers/format-helper
                #:format-datetime
                #:format-currency
                #:truncate-text))
```

#### 3. Importing Types and Classes

```common-lisp
(defpackage #:your-app/views/users/package
  (:use #:cl)
  (:import-from #:clails/view/view-helper #:view)
  (:import-from #:your-app/model
                #:ref
                #:<user>
                #:<post>))
```

### Practical Example

#### Full-Spec Package Definition

`app/views/users/package.lisp`:

```common-lisp
(in-package #:cl-user)
(defpackage #:your-app/views/users/package
  (:use #:cl)
  ;; View helpers
  (:import-from #:clails/view/view-helper
                #:*view-context*
                #:view)
  ;; Model functions and classes
  (:import-from #:your-app/model
                #:ref
                #:<user>
                #:<post>
                #:has-error-p
                #:ref-error)
  ;; Controller helpers
  (:import-from #:your-app/controllers/application-controller
                #:format-datetime
                #:truncate-text
                #:gravatar-url)
  ;; Utility functions
  (:import-from #:alexandria
                #:when-let))

(in-package #:your-app/views/users/package)
```

#### Template Usage Example

`app/views/users/show.html`:

```html
<!DOCTYPE html>
<html>
<head>
  <title>User: <%= (ref (view :user) :name) %></title>
</head>
<body>
  <% (let ((user (view :user))) %>
    <div class="user-profile">
      <img src="<%= (gravatar-url (ref user :email)) %>" alt="Avatar">
      <h1><%= (ref user :name) %></h1>
      <p class="email"><%= (ref user :email) %></p>
      
      <% (when-let (bio (ref user :bio)) %>
        <div class="bio">
          <%= (truncate-text bio 200) %>
        </div>
      <% ) %>
      
      <p class="joined">
        Joined: <%= (format-datetime (ref user :created-at)) %>
      </p>
      
      <% (when (has-error-p user) %>
        <div class="errors">
          <p><%= (ref-error user :name) %></p>
        </div>
      <% ) %>
    </div>
  <% ) %>
</body>
</html>
```

### Package Debugging

You can verify the current package within templates.

```html
<!-- Display current package -->
<p>Current package: <%= (format nil "~A" *package*) %></p>

<!-- Example output: Current package: #<PACKAGE "YOUR-APP/VIEWS/USERS/PACKAGE"> -->
```

---

## 9. Best Practices

### View Responsibilities

Views should only have the following responsibilities:

1. **Data Display**: Display data passed from Controllers
2. **Display Logic**: Minimal logic necessary for display such as conditionals and loops
3. **Form Generation**: Generate forms for user input

### Logic Separation

Don't write complex logic in Views; place it in Controllers or Models.

```html
<!-- Bad example: Complex logic in View -->
<% (let ((filtered-users
          (remove-if-not 
           #'(lambda (u) 
               (and (> (ref u :age) 18)
                    (string= (ref u :status) "active")))
           (view :users)))) %>
  <!-- ... -->
<% ) %>

<!-- Good example: Pass pre-filtered data from Controller -->
<% (dolist (user (view :active-adult-users)) %>
  <!-- ... -->
<% ) %>
```

### Data Preparation

Prepare data needed by Views in Controllers.

```common-lisp
;; Good example
(defmethod do-get ((controller <users-controller>))
  (let* ((users (get-all-users))
         (active-users (remove-if-not #'user-active-p users))
         (user-count (length users)))
    (set-view controller "users/index.html"
              `(:users ,active-users
                :total-count ,user-count
                :active-count ,(length active-users)))))
```

### HTML Escaping

When displaying user input, properly escape it to prevent XSS attacks.

```html
<!-- Automatic escaping feature is not yet implemented -->
<!-- Be careful when displaying user input -->
<p>Name: <%= (view :user-input) %></p>
```

### Comments

Use HTML comments or Common Lisp comments within templates.

```html
<!-- HTML comment: sent to browser -->

<% #|
Common Lisp block comment: 
not included in output
|# %>

<% ; Common Lisp line comment %>
```

---

## 10. Performance

### Template Caching

Templates are compiled and cached, so second and subsequent renders are fast.

### Cache Clearing During Development

When changing templates during development, you need to clear the cache (automatic reload feature is planned for future implementation).

```common-lisp
;; Cache clearing method (depends on implementation)
(clails/view/cache:clear-cache)
```

---

## 11. Practical Examples

### User List Page

```html
<!DOCTYPE html>
<html>
<head>
  <title>User List</title>
  <style>
    .user-list { border-collapse: collapse; width: 100%; }
    .user-list th, .user-list td { border: 1px solid #ddd; padding: 8px; }
    .user-list th { background-color: #4CAF50; color: white; }
    .status-active { color: green; }
    .status-inactive { color: red; }
  </style>
</head>
<body>
  <h1>Users (<%= (length (view :users)) %>)</h1>
  
  <table class="user-list">
    <thead>
      <tr>
        <th>ID</th>
        <th>Name</th>
        <th>Email</th>
        <th>Status</th>
        <th>Actions</th>
      </tr>
    </thead>
    <tbody>
      <% (dolist (user (view :users)) %>
        <tr>
          <td><%= (ref user :id) %></td>
          <td><%= (ref user :name) %></td>
          <td><%= (ref user :email) %></td>
          <td class="<%= (if (ref user :is-active) \"status-active\" \"status-inactive\") %>">
            <%= (if (ref user :is-active) "Active" "Inactive") %>
          </td>
          <td>
            <a href="/users/<%= (ref user :id) %>">View</a> |
            <a href="/users/<%= (ref user :id) %>/edit">Edit</a> |
            <%= (form-with :url (format nil "/users/~A" (ref user :id))
                           :method :delete
                           :attributes '(:style "display:inline;")
                           :body "<button type=\"submit\">Delete</button>") %>
          </td>
        </tr>
      <% ) %>
    </tbody>
  </table>
  
  <p><a href="/users/new">Create New User</a></p>
</body>
</html>
```

### User Detail Page

```html
<!DOCTYPE html>
<html>
<head>
  <title>User Detail</title>
</head>
<body>
  <% (let ((user (view :user))) %>
    <h1><%= (ref user :name) %></h1>
    
    <dl>
      <dt>ID:</dt>
      <dd><%= (ref user :id) %></dd>
      
      <dt>Email:</dt>
      <dd><%= (ref user :email) %></dd>
      
      <dt>Age:</dt>
      <dd><%= (ref user :age) %></dd>
      
      <dt>Status:</dt>
      <dd><%= (if (ref user :is-active) "Active" "Inactive") %></dd>
      
      <dt>Created At:</dt>
      <dd><%= (ref user :created-at) %></dd>
      
      <dt>Updated At:</dt>
      <dd><%= (ref user :updated-at) %></dd>
    </dl>
    
    <p>
      <a href="/users/<%= (ref user :id) %>/edit">Edit</a> |
      <a href="/users">Back to List</a>
    </p>
  <% ) %>
</body>
</html>
```

### Form with Error Display

```html
<!DOCTYPE html>
<html>
<head>
  <title>Create User</title>
  <style>
    .errors { 
      background-color: #f8d7da; 
      border: 1px solid #f5c6cb; 
      color: #721c24;
      padding: 10px;
      margin-bottom: 20px;
    }
    .form-group { margin-bottom: 15px; }
    label { display: block; font-weight: bold; }
    input[type="text"], input[type="email"], input[type="number"] {
      width: 100%;
      padding: 8px;
      border: 1px solid #ddd;
    }
  </style>
</head>
<body>
  <h1>Create New User</h1>
  
  <% (let ((user (view :user))) %>
    <% (when (and user (has-error-p user)) %>
      <div class="errors">
        <h3>Please fix the following errors:</h3>
        <ul>
          <% (when (ref-error user :name) %>
            <li><%= (ref-error user :name) %></li>
          <% ) %>
          <% (when (ref-error user :email) %>
            <li><%= (ref-error user :email) %></li>
          <% ) %>
          <% (when (ref-error user :age) %>
            <li><%= (ref-error user :age) %></li>
          <% ) %>
        </ul>
      </div>
    <% ) %>
    
    <%= (form-tag :url "/users" :method :post) %>
      <div class="form-group">
        <label>Name:</label>
        <input type="text" name="name" 
               value="<%= (if user (ref user :name) \"\") %>" required>
      </div>
      
      <div class="form-group">
        <label>Email:</label>
        <input type="email" name="email" 
               value="<%= (if user (ref user :email) \"\") %>" required>
      </div>
      
      <div class="form-group">
        <label>Age:</label>
        <input type="number" name="age" 
               value="<%= (if user (ref user :age) \"\") %>">
      </div>
      
      <button type="submit">Create User</button>
      <a href="/users">Cancel</a>
    <%= (end-form-tag) %>
  <% ) %>
</body>
</html>
```

---

## Summary

clails Views have the following features:

1. **Simple Syntax**: Intuitive template notation with `<%=` and `<%`
2. **Leverage Common Lisp**: Full Common Lisp functionality available within templates
3. **Controller Integration**: Easy data access via the `view` function
4. **Fast Execution**: Acceleration through template compilation and caching
5. **Flexible Structure**: Namespace management via package system

For detailed API reference, please refer to the docstring of each function.
