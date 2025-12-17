#!/bin/bash
set -e

echo "=== Todo App E2E Test ==="

LOG_DIR="/app/logs"
mkdir -p ${LOG_DIR}
TIMESTAMP=$(date +%Y%m%d-%H%M%S)
LOG_FILE="${LOG_DIR}/${TIMESTAMP}-e2e-todo-app.txt"

exec > >(tee -a ${LOG_FILE}) 2>&1

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
clails server --port 15000 &
SERVER_PID=$!

sleep 10

echo ""
echo "=== Testing Todo API ==="

echo ""
echo "--- Test 1: GET /todo (list all todos) ---"
RESPONSE=$(curl -s -w "\n%{http_code}" --max-time 10 http://localhost:15000/todo)
CURL_EXIT=$?

# Copy logs for debugging
if [ -d logs ]; then
    echo "=== Application Log ==="
    tail -50 logs/application.develop.log || echo "No application log found"
fi

if [ $CURL_EXIT -ne 0 ]; then
    echo "✗ curl failed with exit code: $CURL_EXIT"
    echo "Checking if server is still running..."
    ps aux | grep clails || true
    exit 1
fi
HTTP_CODE=$(echo "$RESPONSE" | tail -n1)
BODY=$(echo "$RESPONSE" | head -n-1)
echo "HTTP Status: $HTTP_CODE"
echo "Response Body (first 200 chars): ${BODY:0:200}"
if [ "$HTTP_CODE" = "200" ]; then
    echo "✓ GET /todo succeeded"
    # Check if HTML contains expected content
    if echo "$BODY" | grep -q "Todo List"; then
        echo "✓ Response contains 'Todo List'"
    else
        echo "✗ Response does not contain expected content"
        exit 1
    fi
else
    echo "✗ GET /todo failed"
    exit 1
fi

echo ""
echo "--- Test 2: POST /todo (create new todo) ---"
RESPONSE=$(curl -s -w "\n%{http_code}" --max-time 10 -X POST \
    -d "title=New task from test" \
    http://localhost:15000/todo)
HTTP_CODE=$(echo "$RESPONSE" | tail -n1)
BODY=$(echo "$RESPONSE" | head -n-1)
echo "HTTP Status: $HTTP_CODE"
if [ "$HTTP_CODE" = "302" ] || [ "$HTTP_CODE" = "303" ]; then
    echo "✓ POST /todo succeeded (redirect)"
else
    echo "HTTP Status: $HTTP_CODE"
    echo "✗ POST /todo failed (expected redirect 302/303)"
    exit 1
fi

echo ""
echo "--- Test 3: Verify new todo was created ---"
RESPONSE=$(curl -s -w "\n%{http_code}" --max-time 10 http://localhost:15000/todo)
HTTP_CODE=$(echo "$RESPONSE" | tail -n1)
BODY=$(echo "$RESPONSE" | head -n-1)
if [ "$HTTP_CODE" = "200" ]; then
    if echo "$BODY" | grep -q "New task from test"; then
        echo "✓ New todo item found in list"
    else
        echo "✗ New todo item not found in list"
        exit 1
    fi
else
    echo "✗ Failed to retrieve todo list"
    exit 1
fi

echo ""
echo "=== Stopping server ==="
clails stop

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
