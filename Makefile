all: format lint build

build:
	@stack build

format:
	@find src -name '*.hs' -exec stack exec hindent -- {} \;

lint:
	@stack exec hlint src

setup:
	@stack build hindent
	@stack build hlint

.PHONY: format lint setup
