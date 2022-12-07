.PHONY: server.run
server.run:
	docker compose --file ../../docker-compose.yaml up $(DOCKER_COMPOSE_OPTS)

.PHONY: server.stop
server.stop:
	docker compose --file ../../docker-compose.yaml down $(DOCKER_COMPOSE_OPTS)

.PHONY: app.run
app.run: bin/yesod-minimal
	OTEL_SERVICE_NAME=yesod-minimal bin/yesod-minimal $(YESOD_MINIMAL_OPTS)

bin/yesod-minimal:
	stack install --local-bin-path ./bin $(CABAL_OPTS) yesod-minimal:exe:yesod-minimal

.PHONY: clean
clean:
	stack clean
	$(RM) bin/yesod-minimal
