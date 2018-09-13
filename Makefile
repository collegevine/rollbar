all: format lint build test

build:
	@stack build
.PHONY: build

format:
	@find src test -name '*.hs' -exec stack exec hindent -- {} \;
.PHONY: format

lint:
	@stack exec hlint src test
.PHONY: lint

setup:
	@stack build hindent
	@stack build hlint
.PHONY: setup

test:
	@stack test
.PHONY: test
