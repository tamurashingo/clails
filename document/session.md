# clails Session & Authentication Guide

## Overview

clails ships an **opt-in** cookie session middleware, built on top of
[`lack-middleware-session`](https://github.com/fukamachi/lack), plus a tiny
`current-user` extension point on top of that session.

This is deliberately narrow in scope:

- **Session support** is a real, framework-provided feature: enable it, and
  every controller gets a per-request, cookie-backed hash table it can read
  and write.
- **`current-user`** is *plumbing only*. clails does **not** implement any
  authentication scheme — no password hashing, no login/logout routes, no
  "user" model convention. It only gives you a documented, session-backed
  place to store "whoever is currently logged in," so that every project
  doesn't have to invent its own ad-hoc mechanism for this extremely common
  need. Your project must implement its own login logic (verifying
  credentials, looking up a user record, etc.) and simply call
  `(setf (current-user controller) ...)` once that logic succeeds.

Explicitly **out of scope** (not provided by clails, not planned as part of
this feature):

- CSRF protection
- Remember-me / persistent-login tokens
- OAuth / OIDC integration
- Role-based authorization or permission checks
- A bundled password-hashing helper

If your project needs any of these, implement them yourself (or bring in a
dedicated library) on top of the `session` / `current-user` plumbing
described below.

---

## 1. Enabling Session Middleware

Session middleware is **disabled by default**. A project opts in by setting
`clails/middleware/session-middleware:*enable-session-middleware*` to `T`,
typically in `app/config/environment.lisp`:

```common-lisp
(in-package #:your-app/config/environment)

;; enable clails' cookie session middleware
(setf clails/middleware/session-middleware:*enable-session-middleware* t)
```

That's it — no changes to `*clails-middleware-stack*` are required. clails
always installs its session middleware in the stack, but when the flag is
`NIL` it is a pure passthrough: no cookie is ever set, and no per-request
hash table is created.

### Customizing the store and cookie state (optional)

By default, sessions are kept in an in-memory store
(`lack/middleware/session/store/memory:make-memory-store`) with a standard
cookie (`lack/middleware/session/state/cookie:make-cookie-state`). Both can
be overridden before the server starts:

```common-lisp
;; use a custom cookie name / httponly flag
(setf clails/middleware/session-middleware:*session-state*
      (lack/middleware/session/state/cookie:make-cookie-state
        :cookie-key "_your_app_session"
        :httponly t
        :secure t))

;; swap the in-memory store for e.g. lack-session-store-dbi / -redis
;; (setf clails/middleware/session-middleware:*session-store* ...)
```

The in-memory store is fine for local development and single-process
deployments; for anything that runs more than one server process (or that
must survive a restart) you will want a shared store such as
`lack-session-store-dbi` or `lack-session-store-redis`.

**Important:** these variables are read once, when the middleware stack is
built at server startup (`clails:server`). Set them in
`app/config/environment.lisp`, which is loaded before the server starts —
do not expect toggling them at runtime to change already-running behavior.

---

## 2. Reading and Writing Session Data

Once enabled, every controller instance has a `session` accessor that
returns a per-request hash table (`:test 'equal`), backed by the cookie
session:

```common-lisp
(defmethod do-get ((controller <my-controller>))
  ;; read
  (let ((visits (or (gethash "visits" (session controller)) 0)))
    ;; write
    (setf (gethash "visits" (session controller)) (1+ visits))
    (set-view controller "home/index.html"
              `(:visits ,(gethash "visits" (session controller))))))
```

Data placed in the session hash table is committed back into the session
store after the response is generated, and the client receives (or keeps)
a session cookie so the same hash table's contents are available on the
visitor's next request.

### If session middleware is not enabled

Calling `(session controller)` when
`clails/middleware/session-middleware:*enable-session-middleware*` is `NIL`
signals an error, telling you to enable the middleware. This is intentional
— silently returning an empty, throwaway hash table would make it easy to
"lose" data without noticing.

---

## 3. `current-user`: an Extension Point, Not an Auth System

`current-user` is a thin pair of accessors built directly on top of
`session`:

```common-lisp
(current-user controller)              ; => whatever you last stored, or NIL
(setf (current-user controller) user)  ; store `user` as the logged-in user
(setf (current-user controller) nil)   ; "log out"
```

Internally, `current-user` simply reads/writes a dedicated key inside the
same session hash table `session` exposes — there is no separate storage,
no user-model requirement, and no session-fixation handling beyond whatever
`lack-middleware-session` itself provides. `user` can be anything your
application finds useful: a full model instance, a plist of `:id`/`:name`,
just an integer ID you re-look-up per request — clails does not care.

### Illustrative login/logout example

**This example is intentionally minimal and is *not* production-security
grade.** It is meant to show the shape of the `current-user` API, not to be
copy-pasted into a real login system. In particular, it does no password
hashing, no timing-safe comparison, no rate limiting, no CSRF protection,
and no session-fixation mitigation (e.g. regenerating the session id on
login) — you must add all of that yourself, or use a dedicated library, for
anything that isn't a toy.

```common-lisp
(defclass <session-controller> (<rest-controller>)
  ())

;; POST /login  { "email": "...", "password": "..." }
(defmethod do-post ((controller <session-controller>))
  (let* ((email (param controller "email"))
         (password (param controller "password"))
         ;; find-user-by-email / verify-password are YOUR project's code;
         ;; clails does not provide user lookup or password verification.
         (user (find-user-by-email email)))
    (if (and user (verify-password user password))
        (progn
          (setf (current-user controller) (list :id (ref user :id)
                                                  :email (ref user :email)))
          (set-response controller '((:status . "ok"))))
        (progn
          (setf (slot-value controller 'code) 401)
          (set-response controller '((:status . "error")
                                      (:message . "invalid credentials")))))))

;; DELETE /login (logout)
(defmethod do-delete ((controller <session-controller>))
  (setf (current-user controller) nil)
  (set-response controller '((:status . "ok"))))
```

And a controller that requires a logged-in user:

```common-lisp
(defclass <account-controller> (<web-controller>)
  ())

(defmethod do-get ((controller <account-controller>))
  (let ((user (current-user controller)))
    (if user
        (set-view controller "account/show.html" `(:user ,user))
        (set-redirect controller "/login"))))
```

Because this check (`(if (current-user controller) ... )`) is just plain
Lisp, you are free to factor it into a helper, a `:before` method, or
whatever fits your project's conventions — clails does not impose a
particular authorization pattern.

---

## 4. Full Example

`app/config/environment.lisp`:

```common-lisp
(setf clails/middleware/session-middleware:*enable-session-middleware* t)
```

`app/controllers/greeting-controller.lisp`:

```common-lisp
(defclass <greeting-controller> (<rest-controller>)
  ())

;; GET /greeting/set?name=Alice
(defmethod index ((controller <greeting-controller>))
  (setf (gethash "name" (session controller)) (param controller "name"))
  (set-response controller '((:status . "ok"))))

;; GET /greeting/get
(defmethod show ((controller <greeting-controller>))
  (set-response controller
                `((:status . "ok")
                  (:name . ,(gethash "name" (session controller))))))
```

With routes wired to `index`/`show` as usual (see the
[Controller Guide](controller.md)), a client that keeps its cookies between
requests (a browser, or `curl -c cookies.txt -b cookies.txt`) will see the
name it set on the first request reflected back on the second — this was
verified manually end-to-end while developing this feature, using
`curl -c cookies.txt ...` followed by `curl -b cookies.txt ...` against a
running clails server.

---

## Summary

1. Session middleware is **opt-in**: set `*enable-session-middleware*` to
   `T` in `app/config/environment.lisp`.
2. `(session controller)` returns a request-scoped, cookie-backed hash
   table — read and write it with plain `gethash`.
3. `(current-user controller)` / `(setf (current-user controller) ...)` is
   plumbing for "whoever is logged in," backed by the same session. clails
   provides no login/logout routes, no password handling, and no
   authorization system — that is your project's responsibility.
4. CSRF protection, remember-me tokens, OAuth/OIDC, role-based
   authorization, and password-hashing helpers are explicitly **not**
   provided; add them yourself (or a dedicated library) if you need them.
