DEV_NW := clails_dev_nw
DEV_NW_EXISTS := $(shell docker network ls --filter name=$(DEV_NW) --format '{{ .ID }}')
TEST_NW := clails_test_nw
TEST_NW_EXISTS := $(shell docker network ls --filter name=$(TEST_NW) --format '{{ .ID }}')

.PHONY: setup
setup: dev.setup test.setup
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
	docker-compose -f docker-compose.dev.yml up -d

.PHONY: dev.down
dev.down:
	@echo "Shutting down..."
	docker-compose -f docker-compose.dev.yml down


# ----------------------------------------
# for testing
# ----------------------------------------
.PHONY: test.setup test.prev
test.setup:
	@if [ -z $(TEST_NW_EXISTS) ]; then \
	    docker network create -d bridge $(TEST_NW) ; \
	fi

test.prev:
	docker-compose -f docker-compose.test.yml down || true
	rm -rf ./volumes
	mkdir ./volumes
	mkdir ./volumes/mysql
	mkdir ./volumes/postgresql
	mkdir ./volumes/postgresql/data
	mkdir ./volumes/postgresql/log
	sleep 1
	docker-compose -f docker-compose.test.yml up -d
	echo wait...
	sleep 10

.PHONY: test
test: test.prev
	@echo "Running tests..."
	docker run --rm -v $(PWD):/app --entrypoint qlot clails-dev install
	docker run --network $(TEST_NW) --rm -v $(PWD):/app --add-host host.docker.internal:host-gateway --entrypoint qlot clails-dev exec rove clails-test.asd

.PHONY: test.down
test.down:
	@Echo "Shutting down..."
	docker-compose -f docker-compose.test.yml down

.PHONY: console.test
console.test:
	docker run -it --rm --network $(TEST_NW) -v $(PWD):/app --add-host host.docker-internal:host-gateway --entrypoint "/bin/bash" clails-dev

