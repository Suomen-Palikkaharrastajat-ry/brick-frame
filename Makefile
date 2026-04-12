HLINT ?= hlint
FOURMOLU ?= fourmolu
GENERATOR_NIX ?= generator-nix
LDRAW_SYNC_URL ?= https://library.ldraw.org/library/updates/complete.zip
LDRAW_SYNC_DIR ?= elm-app/public/ldraw
LDRAW_SYNC_LOCK ?= ldraw-sync.lock.json

.PHONY: help
help: ## Show available targets
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

# ── Vendor / submodules ──────────────────────────────────────────────────────

.PHONY: vendor
vendor: ## Init and update all git submodules to their pinned commits
	@# In CI environments (GitHub Actions) SSH access is unavailable;
	@# rewrite git@github.com: to https://github.com/ so submodules clone via HTTPS.
	@[ -z "$$CI" ] || git config --global url."https://github.com/".insteadOf "git@github.com:"
	@if [ -d .git ]; then git submodule update --init; elif [ ! -d vendor/master-builder ]; then mkdir -p vendor && git clone https://github.com/Suomen-Palikkaharrastajat-ry/master-builder.git vendor/master-builder; fi
	ln -sfn ../vendor/master-builder/packages elm-app/packages
	ln -sfn ../../../vendor/master-builder/review/src/LlmAgent elm-app/review/src/LlmAgent

# ── Development environment ──────────────────────────────────────────────────

.PHONY: shell
shell: ## Enter devenv shell
	devenv shell

.PHONY: develop
develop: devenv.local.nix devenv.local.yaml ## Bootstrap devenv shell + VS Code
	devenv shell --profile=devcontainer -- code .

devenv.local.nix:
	cp devenv.local.nix.example devenv.local.nix

devenv.local.yaml:
	cp devenv.local.yaml.example devenv.local.yaml

# ── Elm frontend ──────────────────────────────────────────────────────────────

.PHONY: all
all: build ## Build everything

HS_SOURCES := $(shell find src generator -name '*.hs') technic.cabal $(wildcard cabal.project*)
ELM_APP_SOURCES := $(shell find elm-app/src -name '*.elm' ! -name 'Data.elm')
ELM_PACKAGE_SOURCES := $(shell find vendor/master-builder/packages -name '*.elm' -o -name '*.css' 2>/dev/null)

technic-generator: $(HS_SOURCES)
	cabal build generator
	cp $$(cabal list-bin generator) $@

.PHONY: generate
generate: elm-app/src/Data.elm ## Run Haskell generator to produce elm-app/src/Data.elm

elm-app/src/Data.elm: technic-generator
	./technic-generator

elm-app/src/.data-nix-stamp:
	$(GENERATOR_NIX)
	touch $@

.PHONY: elm-dev
elm-dev: elm-app/src/Data.elm ## Start Elm + Vite dev server (hot reload)
	cd elm-app && vite

.PHONY: elm-tailwind-gen
elm-tailwind-gen: elm-app/.elm-tailwind/.stamp ## Generate typed Tailwind Elm modules into elm-app/.elm-tailwind/

elm-app/.elm-tailwind/.stamp: elm-app/elm.json elm-app/vite.config.mjs elm-app/main.css $(ELM_APP_SOURCES) $(ELM_PACKAGE_SOURCES) elm-app/src/Data.elm
	cd elm-app && elm-tailwind-classes gen
	mkdir -p elm-app/.elm-tailwind
	touch $@

build/.elm-stamp: elm-app/.elm-tailwind/.stamp $(ELM_APP_SOURCES) $(ELM_PACKAGE_SOURCES) elm-app/elm.json elm-app/vite.config.mjs elm-app/index.html elm-app/main.js elm-app/main.css
	cd elm-app && vite build
	touch $@

build/.elm-stamp-ci: elm-app/src/.data-nix-stamp elm-app/.elm-tailwind/.stamp $(ELM_APP_SOURCES) $(ELM_PACKAGE_SOURCES) elm-app/elm.json elm-app/vite.config.mjs elm-app/index.html elm-app/main.js elm-app/main.css
	cd elm-app && vite build
	touch $@

.PHONY: elm-build
elm-build: build/.elm-stamp ## Production build of Elm SPA → build/

.PHONY: elm-test
elm-test: elm-tailwind-gen ## Run Elm unit tests
	cd elm-app && elm-test

.PHONY: elm-check
elm-check: ## Check Elm formatting (no changes)
	cd elm-app && elm-format --validate src/

.PHONY: elm-format
elm-format: ## Auto-format Elm source files
	cd elm-app && elm-format --yes src/

.PHONY: elm-review
elm-review: ## Run elm-review LLM-agent rules (no changes)
	cd elm-app && elm-review

.PHONY: elm-review-fix
elm-review-fix: ## Run elm-review with auto-fix
	cd elm-app && elm-review --fix

# ── Combined targets ──────────────────────────────────────────────────────────

.PHONY: dev
dev: elm-dev ## Generate data + start Vite dev server

.PHONY: build
build: elm-build ## Generate data + production build → build/

.PHONY: dist-ci
dist-ci: build/.elm-stamp-ci ## CI build: generator (Nix binary) + Elm app → build/

.PHONY: watch
watch: ## Watch Haskell sources and run Vite dev server
	make generate
	find src generator technic.cabal -name "*.hs" -o -name "*.cabal" | entr -s 'make generate' &
	cd elm-app && elm-tailwind-classes gen && vite

.PHONY: sync-ldraw
sync-ldraw: ## Sync official LDraw library into local assets + write manifest
	LDRAW_SYNC_URL="$(LDRAW_SYNC_URL)" \
	LDRAW_SYNC_DIR="$(LDRAW_SYNC_DIR)" \
	LDRAW_SYNC_LOCK="$(LDRAW_SYNC_LOCK)" \
	./scripts/sync-ldraw.sh

.PHONY: sync-ldraw-clean
sync-ldraw-clean: ## Remove synced LDraw assets and lockfile
	rm -rf "$(LDRAW_SYNC_DIR)" "$(LDRAW_SYNC_LOCK)"

# ── Test & quality ────────────────────────────────────────────────────────────

.PHONY: test
test: check ## Run all tests
	cabal test
	$(MAKE) elm-test
	$(MAKE) test-lib

.PHONY: test-lib
test-lib: ## Run technic-simulator library unit tests
	cd packages/technic-simulator && elm-test

.PHONY: repl
repl: ## Open GHCi REPL
	cabal repl

.PHONY: check
check: ## Check formatting, hlint, and elm-review (no changes)
	$(HLINT) src generator
	$(MAKE) elm-check
	$(MAKE) elm-review

.PHONY: cabal-check
cabal-check: ## Check the package for common errors
	cabal check

.PHONY: format
format: ## Auto-format Haskell and Elm source files
	find src generator -name '*.hs' | xargs $(FOURMOLU) --mode inplace
	$(MAKE) elm-format
	treefmt

# ── Cleanup ───────────────────────────────────────────────────────────────────

.PHONY: clean
clean: ## Remove build artifacts and generated output
	cabal clean
	rm -rf build technic-generator .hpc elm-app/.elm-tailwind elm-app/elm-stuff elm-app/src/Data.elm elm-app/src/.data-nix-stamp
