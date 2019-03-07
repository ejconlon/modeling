.PHONY: build
build:
	stack build --test

.PHONY: test
test:
	stack test

.PHONY: deps
deps:
	stack install apply-refact hlint intero stylish-haskell ghcid

.PHONY: watch
watch:
	stack exec ghcid

.PHONY: format
format:
	find . -name '*.hs' | xargs -t stack exec stylish-haskell -- -i

.PHONY: lint
lint:
	stack exec hlint -- src test

.PHONY: refactor
refactor:
	find . -name '*.hs' | xargs -t -L1 stack exec hlint -- --refactor --refactor-options -i
