#!/bin/bash
set -e

echo "=== Clails E2E Test Runner ==="

echo '=== Setting up clails ===' 
cd /qlot
qlot install

echo '=== install clails command ==='
qlot exec ros install /app/roswell/clails.ros

echo '=== Running E2E Tests ===' 
cd /tmp/clails-e2e-test-project
clails new todoapp
cd todoapp
clails db:create
clails db:migrate
clails server --port 15000 &

sleep 10

curl http://localhost:15000/

clails stop

echo '=== List all available test tags ==='
clails test --list-tags

echo '=== List all available test packages ==='
clails test --list-packages


echo '=== Run Tests ==='
clails test
