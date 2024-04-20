.PHONY: setup setup.docker dev.up dev.down

DOCKER_BASE_IMG := clails-dev


setup: setup.docker

setup.docker:
	docker image build -t $(DOCKER_BASE_IMG) .

dev.up:
	docker-compose up -d

dev.down:
	docker-compose down
