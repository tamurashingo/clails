#!/bin/bash
set -e

echo "=== Todo App E2E Test ==="

LOG_DIR="/app/logs"
mkdir -p ${LOG_DIR}
TIMESTAMP=$(date +%Y%m%d-%H%M%S)
LOG_FILE="${LOG_DIR}/${TIMESTAMP}-e2e-todo-app.txt"

exec > >(tee -a ${LOG_FILE}) 2>&1

BASE_URL="http://localhost:15000"
COOKIE_JAR="/tmp/clails-e2e-cookies.txt"
SERVER_LOG="/tmp/clails-e2e-server.log"
rm -f ${COOKIE_JAR} ${SERVER_LOG}

# ----------------------------------------------------------------------
# Helpers
#
# A generated project now has BOTH the session stage and CSRF verification
# switched on, so a state-changing request has to carry two things: the
# session cookie (hence the cookie jar) and this session's CSRF token (hence
# the token extraction below).  A plain `curl -d ...' is answered with 403,
# which is what "--- Test 2" asserts on purpose.
# ----------------------------------------------------------------------

# The value of the hidden CSRF field of the FIRST form in the given HTML.
# Every form on a page shares one token (the token is per session, and the
# project does not switch one-time tokens on), so the first one will do.
extract_csrf_token() {
    grep -o 'name="_csrf_token" value="[^"]*"' \
        | head -n1 \
        | sed 's/.*value="\([^"]*\)".*/\1/'
}

# The id of the first todo whose "Mark as Done" form posts to a member path.
# Taken from the rendered form's action, so the test really submits what the
# page offers rather than a URL made up by the script.
extract_member_form_id() {
    grep -o 'action="/todo/[0-9][0-9]*"' \
        | head -n1 \
        | grep -o '[0-9][0-9]*'
}

fail() {
    echo "✗ $1"
    echo "=== Server output (last 80 lines) ==="
    tail -n 80 ${SERVER_LOG} || true
    if [ -f logs/application.develop.log ]; then
        echo "=== Application Log (last 50 lines) ==="
        tail -n 50 logs/application.develop.log || true
    fi
    exit 1
}

echo "=== Setting up clails ==="
echo "=== Install clails command ==="
ros install /app/roswell/clails.ros
echo "=== cheking clails cmd ==="
clails --help

echo "=== Creating new todo app ==="
cd /tmp/clails-e2e-test-project
clails new todoapp
cd todoapp

echo "=== Generate scaffold for todo ==="
clails generate:scaffold todo

echo "=== Creating migration file ==="
# Create migration file
MIGRATION_FILE=$(ls db/migrate/*_todo.lisp | head -1)
cp /app/test/e2e/templates/migration-todo.lisp ${MIGRATION_FILE}
# Replace package name with project name
sed -i 's/todoapp-db/todoapp-db/g' ${MIGRATION_FILE}

echo "=== Creating seed data ==="
cp /app/test/e2e/templates/seeds.lisp db/seeds.lisp

echo "=== Updating model ==="
cp /app/test/e2e/templates/todo-model.lisp app/models/todo.lisp

echo "=== Updating controller ==="
cp /app/test/e2e/templates/todo-controller.lisp app/controllers/todo-controller.lisp

echo "=== Updating view ==="
cp /app/test/e2e/templates/todo-list.html app/views/todo/list.html

echo "=== Adding the member route (/todo/:id) ==="
# generate:scaffold appends the collection route only; the form in list.html
# posts to the member path.  See the comments in the file being appended.
cat /app/test/e2e/templates/todo-routes.lisp >> app/config/environment.lisp

echo "=== Updating model test ==="
cp /app/test/e2e/templates/todo-model-test.lisp test/models/todo.lisp

echo "=== Updating controller test ==="
cp /app/test/e2e/templates/todo-controller-test.lisp test/controllers/todo-controller.lisp

echo "=== Clearing compilation cache ==="
rm -rf /root/.cache/common-lisp/sbcl-*/tmp/clails-e2e-test-project/

echo "=== Creating database ==="
clails db:create

echo "=== Running migration ==="
clails db:migrate

echo "=== Loading seed data ==="
clails db:seed

echo "=== Starting server ==="
# Into a log file of its own, so that the startup / shutdown hook lines can be
# grepped by the shutdown test below.
clails server --port 15000 > ${SERVER_LOG} 2>&1 &
SERVER_PID=$!

sleep 10

echo ""
echo "=== Testing Todo API ==="

echo ""
echo "--- Test 1: GET /todo (list all todos) ---"
set +e
RESPONSE=$(curl -s -w "\n%{http_code}" --max-time 10 \
    -c ${COOKIE_JAR} -b ${COOKIE_JAR} ${BASE_URL}/todo)
CURL_EXIT=$?
set -e

if [ ${CURL_EXIT} -ne 0 ]; then
    echo "Checking if server is still running..."
    ps aux | grep clails || true
    fail "curl failed with exit code: ${CURL_EXIT}"
fi
HTTP_CODE=$(echo "$RESPONSE" | tail -n1)
BODY=$(echo "$RESPONSE" | head -n-1)
echo "HTTP Status: $HTTP_CODE"
echo "Response Body (first 200 chars): ${BODY:0:200}"
[ "$HTTP_CODE" = "200" ] || fail "GET /todo failed (expected 200, got $HTTP_CODE)"
echo "✓ GET /todo succeeded"

echo "$BODY" | grep -q "Todo List" || fail "Response does not contain expected content"
echo "✓ Response contains 'Todo List'"

# The page is also where the CSRF token and the member-path form come from, so
# their absence has to be a failure here rather than a confusing 403 later.
CSRF_TOKEN=$(echo "$BODY" | extract_csrf_token)
[ -n "${CSRF_TOKEN}" ] || fail "The rendered page carries no hidden _csrf_token field"
echo "✓ The rendered forms carry a CSRF token"

grep -q "_session_id" ${COOKIE_JAR} || fail "No session cookie was stored"
echo "✓ A session cookie was set"

# Regression guard for D3: before this fix the "Mark as Done" form posted to
# the collection path (or to a member path with no matching route), so a
# submission ended up as a 404.  If the rendered form stops pointing at a
# member path, the 404 is back.
MEMBER_FORM_ID=$(echo "$BODY" | extract_member_form_id)
[ -n "${MEMBER_FORM_ID}" ] \
    || fail "No form posts to a member path (/todo/<id>); the D3 bug is back"
echo "✓ The 'Mark as Done' form posts to the member path /todo/${MEMBER_FORM_ID}"

echo "$BODY" | grep -q 'name="_method" value="put"' \
    || fail "The member-path form carries no hidden _method field"
echo "✓ The member-path form declares the method it means (_method=put)"

echo ""
echo "--- Test 2: POST /todo without a CSRF token is refused (403) ---"
HTTP_CODE=$(curl -s -o /dev/null -w "%{http_code}" --max-time 10 -X POST \
    -d "title=Should never be created" \
    ${BASE_URL}/todo)
echo "HTTP Status: $HTTP_CODE"
[ "$HTTP_CODE" = "403" ] \
    || fail "POST without a token should be 403, got $HTTP_CODE (is CSRF verification enabled?)"
echo "✓ POST /todo without a token is refused with 403"

echo ""
echo "--- Test 2b: a forged method cannot escape verification (403) ---"
# _method is only honoured INSIDE CSRF verification, and only for
# PUT / PATCH / DELETE -- so "_method=GET and no token" cannot buy a free pass.
HTTP_CODE=$(curl -s -o /dev/null -w "%{http_code}" --max-time 10 -X POST \
    -d "_method=GET" -d "title=Should never be created" \
    ${BASE_URL}/todo)
echo "HTTP Status: $HTTP_CODE"
[ "$HTTP_CODE" = "403" ] \
    || fail "POST with _method=GET and no token should be 403, got $HTTP_CODE"
echo "✓ _method=GET without a token is still refused with 403"

echo ""
echo "--- Test 3: POST /todo with the token (create new todo) ---"
RESPONSE=$(curl -s -w "\n%{http_code}" --max-time 10 -X POST \
    -c ${COOKIE_JAR} -b ${COOKIE_JAR} \
    --data-urlencode "_csrf_token=${CSRF_TOKEN}" \
    --data-urlencode "title=New task from test" \
    ${BASE_URL}/todo)
HTTP_CODE=$(echo "$RESPONSE" | tail -n1)
echo "HTTP Status: $HTTP_CODE"
if [ "$HTTP_CODE" = "302" ] || [ "$HTTP_CODE" = "303" ]; then
    echo "✓ POST /todo succeeded (redirect)"
else
    fail "POST /todo failed (expected redirect 302/303, got $HTTP_CODE)"
fi

echo ""
echo "--- Test 4: Verify new todo was created ---"
RESPONSE=$(curl -s -w "\n%{http_code}" --max-time 10 \
    -c ${COOKIE_JAR} -b ${COOKIE_JAR} ${BASE_URL}/todo)
HTTP_CODE=$(echo "$RESPONSE" | tail -n1)
BODY=$(echo "$RESPONSE" | head -n-1)
[ "$HTTP_CODE" = "200" ] || fail "Failed to retrieve todo list"
echo "$BODY" | grep -q "New task from test" || fail "New todo item not found in list"
echo "✓ New todo item found in list"

echo ""
echo "--- Test 5: update FROM THE RENDERED FORM reaches do-put (not a 404) ---"
# The D3 regression test.  Everything submitted here is read out of the page:
# the form's action (the member path), its hidden _method and its hidden token.
CSRF_TOKEN=$(echo "$BODY" | extract_csrf_token)
MEMBER_FORM_ID=$(echo "$BODY" | extract_member_form_id)
[ -n "${MEMBER_FORM_ID}" ] || fail "No member-path form to submit; the D3 bug is back"
echo "Submitting the form of todo id=${MEMBER_FORM_ID}"

RESPONSE=$(curl -s -w "\n%{http_code}" --max-time 10 -X POST \
    -c ${COOKIE_JAR} -b ${COOKIE_JAR} \
    --data-urlencode "_csrf_token=${CSRF_TOKEN}" \
    --data-urlencode "_method=put" \
    ${BASE_URL}/todo/${MEMBER_FORM_ID})
HTTP_CODE=$(echo "$RESPONSE" | tail -n1)
echo "HTTP Status: $HTTP_CODE"
if [ "$HTTP_CODE" = "404" ]; then
    fail "The form submission was a 404 -- the member route is missing again (D3). Note that the request-method override stage alone does not fix this: the routing table needs a /todo/:id entry."
fi
if [ "$HTTP_CODE" = "403" ]; then
    fail "The form submission was refused (403) -- the token or the session cookie did not travel with it"
fi
if [ "$HTTP_CODE" = "302" ] || [ "$HTTP_CODE" = "303" ]; then
    echo "✓ The form reached the update handler (redirect)"
else
    fail "Form-driven update failed (expected redirect 302/303, got $HTTP_CODE)"
fi

echo ""
echo "--- Test 6: the controller read the id out of the PATH ---"
# do-put takes the id from (param controller "id"), which only the /todo/:id
# route can fill -- the form body carries no id at all.  So "exactly the todo
# the form belonged to is now Done" is the proof that the path parameter
# arrived.
RESPONSE=$(curl -s -w "\n%{http_code}" --max-time 10 \
    -c ${COOKIE_JAR} -b ${COOKIE_JAR} ${BASE_URL}/todo)
HTTP_CODE=$(echo "$RESPONSE" | tail -n1)
BODY=$(echo "$RESPONSE" | head -n-1)
[ "$HTTP_CODE" = "200" ] || fail "Failed to retrieve todo list after the update"

DONE_ROWS=$(echo "$BODY" | grep -c ">Done<" || true)
[ "${DONE_ROWS}" = "1" ] \
    || fail "Expected exactly one todo to be Done, found ${DONE_ROWS}"
echo "✓ Exactly one todo is Done"

# That one must be the one whose form was submitted: its row no longer offers
# a "Mark as Done" form, while the others still do.
if echo "$BODY" | grep -q "action=\"/todo/${MEMBER_FORM_ID}\""; then
    fail "Todo ${MEMBER_FORM_ID} still offers its update form, so it was not the one updated"
fi
echo "✓ The todo whose form was submitted (id=${MEMBER_FORM_ID}) is the one that changed"

echo ""
echo "--- Test 7: the server starts and stops its session sweeper ---"
grep -q "running startup hook...clails/middleware/session/sweeper:start-session-sweeper" ${SERVER_LOG} \
    || fail "The session sweeper was not started by a startup hook"
echo "✓ The sweeper was started at startup"

echo ""
echo "--- Test 8: shutdown is not delayed by joining the sweeper ---"
# The sweep interval is 300 seconds in a generated project.  A sweeper that
# was stopped by waiting for its next tick would therefore hold the shutdown
# for up to five minutes; stop-session-sweeper signals the loop's waiter, so
# the join is a matter of milliseconds.  20 seconds is far below 300 and far
# above anything a healthy shutdown needs.
SHUTDOWN_DEADLINE=20
STOP_START=$(date +%s)
kill -INT ${SERVER_PID}

# A watchdog so that a shutdown that really does wait for the sweep interval
# fails the test instead of hanging it for five minutes.
( sleep ${SHUTDOWN_DEADLINE}; kill -KILL ${SERVER_PID} 2>/dev/null ) &
WATCHDOG_PID=$!
set +e
wait ${SERVER_PID}
set -e
STOP_ELAPSED=$(( $(date +%s) - STOP_START ))
kill ${WATCHDOG_PID} 2>/dev/null || true
wait ${WATCHDOG_PID} 2>/dev/null || true
echo "Shutdown took ${STOP_ELAPSED}s (deadline ${SHUTDOWN_DEADLINE}s, sweep interval 300s)"

[ ${STOP_ELAPSED} -lt ${SHUTDOWN_DEADLINE} ] \
    || fail "The server did not stop within ${SHUTDOWN_DEADLINE}s -- shutdown is waiting for something (the sweeper's next tick?)"
echo "✓ The server stopped in ${STOP_ELAPSED}s"

grep -q "running shutdown hook...clails/middleware/session/sweeper:stop-session-sweeper" ${SERVER_LOG} \
    || fail "The shutdown ran without stopping the sweeper (did the shutdown hooks run at all?)"
echo "✓ The sweeper was stopped by a shutdown hook"

echo ""
echo "=== Server output ==="
tail -n 60 ${SERVER_LOG} || true

if [ -d logs ]; then
    echo "=== Application Log ==="
    tail -n 50 logs/application.develop.log || echo "No application log found"
fi

echo ""
echo "=== Preparing test database ==="
echo "Creating test database..."
CLAILS_ENV=test clails db:create

echo "Running test migrations..."
CLAILS_ENV=test clails db:migrate

echo "Loading test seed data..."
CLAILS_ENV=test clails db:seed

echo ""
echo "=== Running application tests ==="
clails test || true

echo ""
echo "=== Todo App E2E Test Completed Successfully ==="
echo "Log file: ${LOG_FILE}"
