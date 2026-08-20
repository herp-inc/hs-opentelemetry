.PHONY: all
all: all.cabal-9.6

.PHONY: all.cabal-9.6
all.cabal-9.6:
	cabal v2-build --jobs --enable-tests --enable-benchmarks all
	cabal v2-test --jobs all

.PHONY: build.all
build.all: build.all.cabal-9.6

.PHONY: build.all.cabal-9.6
build.all.cabal-9.6:
	cabal build --jobs --enable-tests --enable-benchmarks all

# format requires fourmolu 0.13.1.0 or later
.PHONY: format
format:
	fourmolu --mode inplace $$(git ls-files | grep -E "\.hs$$")

.PHONY: format.check
format.check:
	fourmolu --mode check $$(git ls-files | grep -E "\.hs$$")


# Hack https://www.gnu.org/software/make/manual/html_node/Force-Targets.html
FORCE:
