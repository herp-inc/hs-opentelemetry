.PHONY: server.run
server.run:
	docker compose --file ../../docker-compose.yaml up $(DOCKER_COMPOSE_OPTS)

.PHONY: server.stop
server.stop:
	docker compose --file ../../docker-compose.yaml down $(DOCKER_COMPOSE_OPTS)

.PHONY: app.run
app.run: bin/hedis-example
	OTEL_SERVICE_NAME=hedis-example bin/hedis-example $(HTTP_SERVER_OPTS)

bin/hedis-example: FORCE
	stack install --local-bin-path ./bin hedis-example:exe:hedis-example

.PHONY: clean
clean:
	stack clean
	$(RM) bin/hedis-example

# Hack https://www.gnu.org/software/make/manual/html_node/Force-Targets.html
FORCE:
