.PHONY: build build-strict test test-unit test-integration clean lint format format-check dev-setup

build:
	nix build

build-strict:
	cabal build all --enable-tests --ghc-options="-Wall -Wcompat -Werror"

test: test-unit test-integration

test-unit: build
	@echo "=== Unit tests ==="
	nix build

test-integration: build
	@echo "=== Integration tests ==="
	bash test/integration.sh ./result/bin/fzfx

clean:
	rm -rf result dist-newstyle

dev-setup:
	git config --local core.hooksPath .githooks

lint:
	hlint src/ test/

format:
	fourmolu --mode inplace src/ test/

format-check:
	fourmolu --mode check src/ test/
