.PHONY: server.run
server.run:
	docker compose up $(DOCKER_COMPOSE_OPTS)

bin/yesod-minimal: package.yaml src/Minimal.hs src/Subsite.hs src/Subsite/Data.hs
	stack install --local-bin-path ./bin yesod-minimal:exe:yesod-minimal

.PHONY: clean
clean:
	stack clean
	$(RM) bin/yesod-minimal
