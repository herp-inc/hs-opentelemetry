DOCKER_TAG = hs-opentelemetry-examples-http-server

.PHONY: client.run
client.run: bin/http-server
	bin/http-server

.PHONY: server.run
server.run:
	docker-compose up

bin/http-server: http-server.cabal main.hs
	stack install --local-bin-path ./bin http-server:exe:http-server

.PHONY: docker.build
docker.build:
	$(MAKE) -C ../.. docker.examples.http-server.build DOCKER_EXAMPLES_HTTP_SERVER_TAG=$(DOCKER_TAG)

.PHONY: clean
clean:
	stack clean
	$(RM) bin/http-server
