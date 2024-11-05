TEST_NW := clails_test_nw

.PHONY: setup
setup:
	@echo "Setting up..."
	docker build -t clails-dev .
	test -z "$$(docker network ls --filter name=$(TEST_NW) --format '{{ .ID }}')"
	docker network create -d bridge $(TEST_NW)

.PHONY: test-prev
test-prev:
	docker-compose down || true
	docker volume rm clails_test_db || true
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

