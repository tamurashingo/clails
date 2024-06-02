.PHONY: setup setup.docker dev.up dev.down


# ----------------------------------------
DEV_PROJECT := clails-dev
DEV_NW := clails_dev_nw

# ----------------------------------------
DOCKER_BASE_IMG := clails-dev



setup: setup.docker setup.dev

setup.dev:
	test -z "$$(docker network ls --filter name=$(DEV_NW) --format '{{ .ID }}')" && docker network create -d bridge $(DEV_NW) || true

setup.docker:
	docker image build -t $(DOCKER_BASE_IMG) -f script/docker/Dockerfile.dev script/docker/


dev.up:
	docker-compose -f script/docker/mysql.dev.yml -p $(DEV_PROJECT) up -d
	docker-compose -f script/docker/postgresql.dev.yml -p $(DEV_PROJECT) up -d
	docker-compose -f script/docker/clails.dev.yml -p $(DEV_PROJECT) up -d

dev.down:
	docker-compose -f script/docker/clails.dev.yml -p $(DEV_PROJECT) down
	docker-compose -f script/docker/postgresql.dev.yml -p $(DEV_PROJECT) down
	docker-compose -f script/docker/mysql.dev.yml -p $(DEV_PROJECT) down


