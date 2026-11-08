;;; The MEMBER route of the e2e todo resource (D3, M8-T2).
;;;
;;; `clails generate:scaffold todo' only appends the COLLECTION route
;;; (:path "/todo") to app/config/environment.lisp, while the view template it
;;; generates renders a form that posts to the MEMBER path (/todo/:id) with a
;;; hidden "_method=put".  Those two never matched, so submitting that form
;;; produced a 404 -- and nothing noticed, because the e2e script never
;;; submitted it.
;;;
;;; Adding the request-method override stage does NOT fix this on its own: the
;;; override rewrites POST -> PUT, but a PUT at /todo/7 still has to match a
;;; route, and there was no route with an :id segment to match.  The two halves
;;; have to land together -- hence this file, which is appended to the
;;; generated configuration by test/e2e/todo-app-e2e.sh.
;;;
;;; The collection route's scanner is anchored (^/todo$), so a member path only
;;; ever matches the entry below; neither route declares :method, so both
;;; dispatch through the legacy do-get / do-post / do-put branch, and
;;; (param controller "id") is filled from the path's :id segment.

(setf clails/environment:*routing-tables*
      (append clails/environment:*routing-tables*
              '((:path "/todo/:id"
                 :controller "todoapp/controllers/todo-controller:<todo-controller>"))))
