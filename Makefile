DEV_NW := clails_dev_nw
DEV_NW_EXISTS := $(shell docker network ls --filter name=$(DEV_NW) --format '{{ .ID }}')

TEST_BUILDER = .test_image_built
TEST_DEPS = Dockerfile docker-compose.test.yml docker-compose.test-runner.yml qlfile clails.asd

E2E_BUILDER = .e2e_image_build
E2E_DEPS = Dockerfile.e2e docker-compose.e2e.yml qlfile clails.asd

.PHONY: setup
setup: dev.setup
	@echo "Setting up..."
	docker build -t clails-dev .


# ----------------------------------------
# for developing
# ----------------------------------------
.PHONY: dev.setup dev.up dev.down
dev.setup:
	@if [ -z $(DEV_NW_EXISTS) ]; then \
	    docker network create -d bridge $(DEV_NW) ; \
	fi

.PHONY: dev.up
dev.up:
	@echo "Booting up..."
	docker compose -f docker-compose.dev.yml up -d

.PHONY: dev.down
dev.down:
	@echo "Shutting down..."
	docker compose -f docker-compose.dev.yml down


# ----------------------------------------
# for testing
# ----------------------------------------
.PHONY: test.build test.rebuild test.clean
test.build: $(TEST_BUILDER)

$(TEST_BUILDER): $(TEST_DEPS)
	docker compose -f docker-compose.test-runner.yml build --no-cache

.PHONY: test.prev
test.prev:
	docker compose -f docker-compose.test.yml down || true
	sleep 1
	docker compose -f docker-compose.test.yml up -d
	echo wait...
	sleep 10

.PHONY: test
test: $(TEST_BUILDER) test.prev
	@echo "Running tests..."
	docker compose -f docker-compose.test-runner.yml run --rm --entrypoint qlot clails-test install
	docker compose -f docker-compose.test-runner.yml run --rm --entrypoint cp clails-test /app/clails-test.asd /qlot
	docker compose -f docker-compose.test-runner.yml run --rm --entrypoint qlot clails-test exec rove clails-test.asd

.PHONY: test.down
test.down:
	@echo "Shutting down..."
	docker compose -f docker-compose.test.yml down

.PHONY: test.console test.postgresql test.mysql test.sqlite3
test.console:
	docker compose -f docker-compose.test-runner.yml run -it --rm --entrypoint /bin/bash clails-test
test.postgresql:
	docker compose -f docker-compose.test-runner.yml run -it --rm --entrypoint /bin/bash clails-test /app/script/conn-postgresql.sh
test.mysql:
	docker compose -f docker-compose.test-runner.yml run -it --rm --entrypoint /bin/bash clails-test /app/script/conn-mysql.sh
test.sqlite3:
	docker compose -f docker-compose.test-runner.yml run -it --rm --entrypoint sqlite3 clails-test /qlot/volumes/clails_test.sqlite3

.PHONY: test.clean
test.clean:
	docker compose -f docker-compose.test-runner.yml down -v
	rm -f $(TEST_BUILDER)


# ----------------------------------------
# E2E testing
# ----------------------------------------
.PHONY: e2e.build e2e.test e2e.clean e2e.console

e2e.build: $(E2E_BUILDER)

$(E2E_BUILDER): $(E2E_DEPS)
	docker compose -f docker-compose.e2e.yml build --no-cache

e2e.test: $(E2E_BUILDER)
	@echo "Running E2E tests..."
	docker compose -f docker-compose.e2e.yml run --rm --entrypoint /bin/bash e2e-test /app/script/e2e.sh

e2e.clean:
	rm -f $(E2E_BUILDER)
	docker compose -f docker-compose.e2e.yml down -v

e2e.console:
	@echo "Starting E2E test console..."
	docker compose -f docker-compose.e2e.yml run --rm e2e-test bash

