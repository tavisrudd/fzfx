.PHONY: build test test-unit test-integration clean

build:
	nix build

test: test-unit test-integration

test-unit: build
	@echo "=== Unit tests ==="
	nix build

test-integration: build
	@echo "=== Integration tests ==="
	bash test/integration.sh ./result/bin/fzfx

clean:
	rm -rf result dist-newstyle
