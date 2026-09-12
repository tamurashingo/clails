# clails Job Queue Guide

## Overview

This guide explains clails' background job queue.

Where the [task system](task.md) (`deftask`/`clails task`) runs a rake-style
command once, synchronously, from the CLI, the job queue lets application
code enqueue work to be picked up and executed later, off the request path,
by a worker process -- the kind of thing you'd reach for Sidekiq or
ActiveJob for in other frameworks. Jobs are persisted to a plain database
table (no external broker required), survive a worker restart, and are
retried automatically with exponential backoff if their handler signals an
error.

## Basic Concepts

- Job handlers are defined with the `defjob` macro (the async sibling of `deftask`)
- `enqueue-job` persists a job to the `clails_jobs` table so it can run later
- A worker (`clails/job:run-worker-loop`, or `clails job:work` from the CLI)
  polls that table and runs due jobs
- A failing job is retried with exponential backoff until it either succeeds
  or exhausts its `:max-attempts`, at which point it is marked `:failed` with
  the error recorded
- The `clails_jobs` table is created by a normal migration -- there is
  nothing job-queue-specific about `db:migrate`/`db:rollback`

## Scope

This is a deliberately small, single-process job queue:

- **In scope**: persisting a job, a worker that claims and runs due jobs,
  retry with exponential backoff up to a configurable max-attempts, running
  a job at or after a given time (`:run-at`).
- **Out of scope** (for now): an external broker (Redis/Sidekiq-style) --
  the queue is entirely DB-persisted; a web dashboard for inspecting jobs;
  cron-style recurring schedules (a job runs once, at or after `:run-at`).
  See [Future Directions](#8-future-directions).

---

## 1. Setting Up the `clails_jobs` Table

The job queue needs a table to persist jobs in. Generate its migration once,
the same way you would generate any other migration, then run it:

```bash
clails generate:job-queue-setup
clails db:migrate
```

`generate:job-queue-setup` drops a normal migration file into
`db/migrate/`, pre-filled with the `clails_jobs` table definition (see
`template/generate/job-queue-migration.lisp.tmpl`) -- there is no separate
migration mechanism to learn. It creates:

| Column | Type | Notes |
|--------|------|-------|
| `id` | integer | primary key (added automatically, like every clails table) |
| `job_name` | string | name the job was registered with via `defjob` |
| `arguments` | text | job arguments, JSON-encoded |
| `status` | string | `pending` / `running` / `succeeded` / `failed` |
| `attempts` | integer | attempts made so far |
| `max_attempts` | integer | attempts allowed before giving up |
| `available_at` | datetime | earliest time this job may run |
| `last_error` | text | error message from the most recent failed attempt |
| `created_at` / `updated_at` | datetime | added automatically, like every clails table |

If your project doesn't use the job queue, you never need to run this --
nothing else in clails depends on the table existing.

---

## 2. Defining Jobs

Use the `defjob` macro to register a job handler:

```common-lisp
(defpackage #:your-app/jobs/send-welcome-email
  (:use #:cl)
  (:import-from #:clails/job
                #:defjob)
  (:import-from #:your-app/models/user
                #:<user>))

(in-package #:your-app/jobs/send-welcome-email)

(defjob :send-welcome-email
  :description "Send a welcome email to a new user"
  :max-attempts 5
  :function (lambda (&key user-id)
              (let ((user (find-user user-id)))
                (send-mail (ref user :email) "Welcome!"))))
```

- `:function` is a lambda taking the same keyword arguments `enqueue-job`
  is called with.
- `:max-attempts` (default 25) is how many times the worker will attempt this
  job, including the first attempt, before giving up and marking it `:failed`.
  It can also be overridden per-call from `enqueue-job`.
- Like task files, job files should be loaded as part of your application
  (e.g. `app/jobs/*.lisp`, wired into your application loader) so `defjob`
  has run by the time you enqueue or run jobs.

A job handler that signals an error is exactly how you tell the queue to
retry it -- just let the error propagate (or explicitly `(error ...)`)
rather than catching it yourself, unless you want that particular failure
to *not* count as an attempt.

---

## 3. Enqueuing Jobs

```common-lisp
(clails/job:enqueue-job :send-welcome-email :arguments (list :user-id 42))
```

`enqueue-job` accepts:

| Argument | Description |
|----------|-------------|
| `job-name` | The keyword a job was registered with via `defjob` |
| `:arguments` | Plist passed to the job's `:function` when it runs |
| `:max-attempts` | Overrides the job's own `:max-attempts` for this call |
| `:run-at` | Universal time at/after which the job becomes eligible to run (default: now) |

```common-lisp
;; Run as soon as a worker is free
(enqueue-job :send-welcome-email :arguments (list :user-id 42))

;; Run this job at least 5 attempts even if the job was registered with fewer
(enqueue-job :send-welcome-email :arguments (list :user-id 42) :max-attempts 10)

;; Don't run until an hour from now
(enqueue-job :send-welcome-email
             :arguments (list :user-id 42)
             :run-at (+ (get-universal-time) 3600))
```

`enqueue-job` returns the new job's `id`.

---

## 4. Running the Worker

### From the CLI

```bash
clails job:work
```

This starts the connection pool, loads your application's model metadata
(the same startup steps `clails server` and `clails db:seed` perform), and
then polls the `clails_jobs` table forever, running due jobs as they become
available. Stop it with Ctrl-C.

```bash
# Poll less aggressively
clails job:work --poll-interval 5
```

### From application code

```common-lisp
;; Run forever on the current thread
(clails/job:run-worker-loop)

;; Run on a background thread instead
(clails/job:start-worker :poll-interval 1)
...
(clails/job:stop-worker)

;; Run a bounded number of polls (handy for scripts/tests)
(clails/job:run-worker-loop :max-iterations 100 :poll-interval 1)
```

`start-worker`/`stop-worker` run the poll loop on its own
`bordeaux-threads` thread, the same threading model the rest of clails
uses (see `clails/model/connection`); the worker thread lazily acquires its
own pooled database connection the first time it claims or finalizes a job.

---

## 5. Retries and Backoff

When a job's handler signals an error, the worker:

1. Records the error message on the job row (`last_error`).
2. If the job's attempt count is still below `max_attempts`, reschedules it
   with an exponential backoff delay: `available_at` is pushed out by
   `*default-backoff-base-seconds*` (default 5) doubled per attempt, capped
   at `*default-backoff-max-seconds*` (default 3600) -- i.e. 5s, 10s, 20s,
   40s, ... up to an hour. The job's `status` goes back to `pending` so any
   worker can pick it up again once due.
3. Otherwise, marks the job `:failed` -- it will not be retried again.

```common-lisp
;; Tune backoff globally (e.g. in tests, or if 5s is too slow/fast for
;; your workload)
(setf clails/job:*default-backoff-base-seconds* 1)
(setf clails/job:*default-backoff-max-seconds* 60)
```

A job whose name isn't registered (e.g. the worker hasn't loaded the file
that calls `defjob` for it yet) is treated the same way as a handler error:
it is retried, giving a worker that starts up in the wrong order a chance
to catch up, and is marked `:failed` if it's still unregistered after
`max_attempts`.

---

## 6. Concurrency and Locking

Claiming a job is done inside one transaction: `SELECT ... FOR UPDATE SKIP
LOCKED` on MySQL/PostgreSQL, or a `BEGIN IMMEDIATE` transaction on SQLite3
(the same transaction-mode mechanism
`clails/model/lock:with-locked-transaction` uses for pessimistic locking --
see [document/model.md](model.md#8-pessimistic-locking)), followed by an
`UPDATE` that marks the row `running` and increments its attempt count. This
means it's safe to run more than one worker (multiple threads, or multiple
`clails job:work` processes) against the same table: a row can only ever be
claimed by one of them.

What this queue does **not** provide is cross-process coordination beyond
that row-level claim -- there's no leader election, no distributed
scheduling, no cross-node rate limiting. For a single application talking to
one database, that's normally all you need.

---

## 7. Full Worked Example

```common-lisp
;; app/jobs/send-email.lisp
(defpackage #:your-app/jobs/send-email
  (:use #:cl)
  (:import-from #:clails/job
                #:defjob)
  (:import-from #:clails/logger/core
                #:log.job))

(in-package #:your-app/jobs/send-email)

(defjob :send-email
  :description "Send a transactional email"
  :max-attempts 5
  :function (lambda (&key to subject body)
              (log.job "Sending email" :to to :subject subject)
              (your-app/mailer:deliver :to to :subject subject :body body)))
```

```common-lisp
;; somewhere in a controller action, after creating an order:
(clails/job:enqueue-job :send-email
                        :arguments (list :to (ref order :customer-email)
                                        :subject "Order confirmed"
                                        :body (render-order-confirmation order)))
```

```bash
# one-time setup
clails generate:job-queue-setup
clails db:migrate

# in a separate process/terminal, keep the worker running
clails job:work
```

If `your-app/mailer:deliver` raises an error (say, the mail server is
briefly unreachable), the job is retried automatically: 5s later, then 10s,
20s, and 40s later, for a total of 5 attempts, before being marked `:failed`
with the underlying error recorded in `last_error` for you to inspect
(e.g. `select * from clails_jobs where status = 'failed'`).

---

## 8. Future Directions

Deliberately left out of this first version, as separate, larger design
decisions:

- **External broker integration** (Redis/Sidekiq, RabbitMQ, SQS, ...) for
  applications that need cross-process/cross-language queues or higher
  throughput than a polled DB table can offer. This would be an alternative
  backend behind the same `enqueue-job`/`defjob` API, not a replacement for
  it, and would add a new required dependency, so it's left for a follow-up
  proposal.
- **A web dashboard** for inspecting pending/failed jobs (a la Sidekiq Web
  or `good_job`'s dashboard).
- **Cron-style recurring schedules** ("every day at 2am") on top of the
  existing one-shot `:run-at` scheduling.
- **Per-queue/priority support** (multiple named queues, worker
  concurrency limits per queue).

---

## 9. Troubleshooting

### Jobs never run

- Is a worker actually running (`clails job:work`, or
  `start-worker`/`run-worker-loop` in your application)?
- Does the `clails_jobs` table exist (`clails generate:job-queue-setup` +
  `clails db:migrate`)?
- Check `available_at` on the row -- a job enqueued with a future `:run-at`
  won't be claimed until then.

### Job immediately fails with "No job registered for name ..."

The worker process hasn't loaded the file that calls `defjob` for that job
name. Make sure your job files are loaded as part of application startup,
the same way models and controllers are.

### A job keeps retrying and never succeeds

Check `last_error` on the row for the underlying exception message; the
job will be marked `:failed` once it reaches `max_attempts`.

---

## 10. Summary

- Define job handlers with `defjob`, persist work with `enqueue-job`
- Run `clails generate:job-queue-setup` once, then `clails db:migrate`, to
  create the `clails_jobs` table
- Run a worker with `clails job:work` (or `run-worker-loop`/`start-worker`
  from code)
- Failures retry automatically with exponential backoff up to
  `max-attempts`, then land in `:failed` with the error recorded

Reach for the job queue whenever you'd otherwise be tempted to do
slow/unreliable work (sending email, calling a third-party API, processing
an upload) directly on the request path.
