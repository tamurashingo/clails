TEST_NW := clails_test_nw
TEST_NW_EXISTS := $(shell docker network ls --filter name=$(TEST_NW) --format '{{ .ID }}')

.PHONY: setup
setup:
	@echo "Setting up..."
	docker build -t clails-dev .
	@if [ -z $(TEST_NW_EXISTS) ]; then \
	    docker network create-d bridge $(TEST_NW) ; \
	fi

.PHONY: test-prev
test-prev:
	docker-compose down || true
	#docker volume rm clails_test_db || true
	rm -rf ./volumes
	mkdir ./volumes
	mkdir ./volumes/mysql
	mkdir ./volumes/postgresql
	mkdir ./volumes/postgresql/data
	mkdir ./volumes/postgresql/log
	docker-compose up -d

.PHONY: test-wait
test-wait:
	echo wait...
	sleep 10

.PHONY: test
test: test-prev test-wait
	@echo "Running tests..."
	# CL_SOURCE_REGISTRY=$(PWD) rove clails-test.asd
	# qlot exec rove clails-test.asd
	docker run --rm -v $(PWD):/app --entrypoint qlot clails-dev install
	docker run --network $(TEST_NW) --rm -v $(PWD):/app --add-host host.docker.internal:host-gateway --entrypoint qlot clails-dev exec rove clails-test.asd

.PHONY: test-console
test-console:
	docker run -it --rm --network $(TEST_NW) -v $(PWD):/app --add-host host.docker-internal:host-gateway --entrypoint "/bin/bash" clails-dev

