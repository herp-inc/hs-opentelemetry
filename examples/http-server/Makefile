.PHONY: server.run
server.run:
	docker compose --file ../../docker-compose.yaml up $(DOCKER_COMPOSE_OPTS)

.PHONY: server.stop
server.stop:
	docker compose --file ../../docker-compose.yaml down $(DOCKER_COMPOSE_OPTS)

.PHONY: app.run
app.run: bin/http-server
	OTEL_SERVICE_NAME=http-server bin/http-server $(HTTP_SERVER_OPTS)

bin/http-server: FORCE
	stack install --local-bin-path ./bin http-server:exe:http-server

.PHONY: clean
clean:
	stack clean
	$(RM) bin/http-server

# Hack https://www.gnu.org/software/make/manual/html_node/Force-Targets.html
FORCE:
