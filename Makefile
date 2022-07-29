.PHONY: docker.examples.http-server.build
docker.examples.http-server.build:
	docker build --tag $(DOCKER_EXAMPLES_HTTP_SERVER_TAG) --file examples/http-server/Dockerfile .
