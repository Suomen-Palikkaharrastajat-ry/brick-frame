HLINT ?= hlint
FOURMOLU ?= fourmolu
GENERATOR_NIX ?= generator-nix
# node_modules is a read-only Nix-store symlink, so Vite's default ESM config
# loader (which bundles the config into node_modules/.vite-temp) can't write and
# throws. --configLoader native loads the config via a plain import() instead.
# The dep-optimization cache is redirected off node_modules via cacheDir in the
# vite config files.
VITE ?= vite --configLoader native
LDRAW_SYNC_URL ?= https://library.ldraw.org/library/updates/complete.zip
LDRAW_SYNC_DIR ?= viewer-app/public/ldraw
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
	ln -sfn ../vendor/master-builder/packages viewer-app/packages
	ln -sfn ../../../vendor/master-builder/review/src/LlmAgent viewer-app/review/src/LlmAgent

# ── Development environment ──────────────────────────────────────────────────

.PHONY: shell
shell: ## Enter devenv shell
	devenv shell

.PHONY: develop
develop: devenv.local.nix devenv.local.yaml ## Bootstrap devenv shell + VS Code
	devenv shell --profile=devcontainer -- code .

# ── Elm frontend ──────────────────────────────────────────────────────────────

.PHONY: all
all: build ## Build everything

HS_SOURCES := $(shell find src generator -name '*.hs') brick-frame.cabal $(wildcard cabal.project*)
VIEWER_APP_SOURCES := $(shell find viewer-app/src -name '*.elm' ! -name 'Data.elm')
ELM_PACKAGE_SOURCES := $(shell find vendor/master-builder/packages -name '*.elm' -o -name '*.css' 2>/dev/null)
WC_SOURCES := $(shell find viewer-app/web-components viewer-app/runtime -name '*.js' 2>/dev/null)

brick-data-generator: $(HS_SOURCES)
	cabal build generator
	cp $$(cabal list-bin generator) $@

GENERATED_ELM := viewer-app/src/Data.elm packages/brick-frame-simulator/src/LDraw/ColorData.elm

.PHONY: generate
generate: viewer-app/src/Data.elm ## Run Haskell generator to produce generated Elm data modules

# Both generated modules come out of a single generator run; Data.elm stands in
# for the pair as the make target.
viewer-app/src/Data.elm: brick-data-generator viewer-app/public/ldraw/LDConfig.ldr
	./brick-data-generator
	cd viewer-app && elm-format --yes $(patsubst viewer-app/%,%,$(filter viewer-app/%,$(GENERATED_ELM))) $(addprefix ../,$(filter-out viewer-app/%,$(GENERATED_ELM)))

viewer-app/src/.data-nix-stamp:
	$(GENERATOR_NIX)
	touch $@

.PHONY: elm-dev
elm-dev: viewer-app/src/Data.elm ## Start Elm + Vite dev server (hot reload)
	cd viewer-app && $(VITE)

.PHONY: elm-tailwind-gen
elm-tailwind-gen: viewer-app/.elm-tailwind/.stamp ## Generate typed Tailwind Elm modules into viewer-app/.elm-tailwind/

viewer-app/.elm-tailwind/.stamp: viewer-app/elm.json viewer-app/vite.config.mjs viewer-app/main.css $(VIEWER_APP_SOURCES) $(ELM_PACKAGE_SOURCES) viewer-app/src/Data.elm
	cd viewer-app && elm-tailwind-classes gen
	mkdir -p viewer-app/.elm-tailwind
	touch $@

build/.elm-stamp: viewer-app/.elm-tailwind/.stamp $(VIEWER_APP_SOURCES) $(ELM_PACKAGE_SOURCES) viewer-app/elm.json viewer-app/vite.config.mjs viewer-app/index.html viewer-app/main.js viewer-app/main.css
	cd viewer-app && $(VITE) build
	touch $@

build/.elm-stamp-ci: viewer-app/src/.data-nix-stamp viewer-app/.elm-tailwind/.stamp $(VIEWER_APP_SOURCES) $(ELM_PACKAGE_SOURCES) viewer-app/elm.json viewer-app/vite.config.mjs viewer-app/index.html viewer-app/main.js viewer-app/main.css
	cd viewer-app && $(VITE) build
	touch $@

.PHONY: elm-build
elm-build: build/.elm-stamp ## Production build of Elm SPA → build/

build/.wc-stamp: viewer-app/src/Data.elm viewer-app/elm.json viewer-app/vite.web-components.config.mjs viewer-app/src/Main.elm viewer-app/src/Ports.elm viewer-app/geometry-worker.js viewer-app/main.css $(WC_SOURCES)
	cd viewer-app && $(VITE) build --config vite.web-components.config.mjs
	touch $@

build/.wc-stamp-ci: viewer-app/src/.data-nix-stamp viewer-app/elm.json viewer-app/vite.web-components.config.mjs viewer-app/src/Main.elm viewer-app/src/Ports.elm viewer-app/geometry-worker.js viewer-app/main.css $(WC_SOURCES)
	cd viewer-app && $(VITE) build --config vite.web-components.config.mjs
	touch $@

.PHONY: wc-build
wc-build: build/.wc-stamp ## Production build of Web Components bundle

.PHONY: elm-test
elm-test: elm-tailwind-gen viewer-app/src/Data.elm ## Run Elm unit tests
	cd viewer-app && elm-test

.PHONY: elm-check
elm-check: ## Check Elm formatting (no changes)
	cd viewer-app && elm-format --validate src/

.PHONY: elm-format
elm-format: ## Auto-format Elm source files
	cd viewer-app && elm-format --yes src/

.PHONY: elm-review
elm-review: ## Run elm-review LLM-agent rules (no changes)
	cd viewer-app && elm-review

.PHONY: elm-review-fix
elm-review-fix: ## Run elm-review with auto-fix
	cd viewer-app && elm-review --fix

# ── Combined targets ──────────────────────────────────────────────────────────

.PHONY: dev
dev: elm-dev ## Generate data + start Vite dev server

.PHONY: build
build: elm-build wc-build ## Generate data + production build → build/

.PHONY: dist-ci
dist-ci: build/.elm-stamp-ci build/.wc-stamp-ci ## CI build: generator (Nix binary) + app + web components → build/

.PHONY: watch
watch: ## Watch Haskell sources and run Vite dev server
	make generate
	find src generator brick-frame.cabal -name "*.hs" -o -name "*.cabal" | entr -s 'make generate' &
	cd viewer-app && elm-tailwind-classes gen && $(VITE)

.PHONY: watch-docs
watch-docs: ## Start Vite for web-components docs playground watch mode
	$(MAKE) generate
	@echo "Open http://localhost:5173/docs/index.watch.html"
	cd viewer-app && $(VITE)

.PHONY: sync-ldraw
sync-ldraw: ## Sync official LDraw library into local assets + write manifest
	LDRAW_SYNC_URL="$(LDRAW_SYNC_URL)" \
	LDRAW_SYNC_DIR="$(LDRAW_SYNC_DIR)" \
	LDRAW_SYNC_LOCK="$(LDRAW_SYNC_LOCK)" \
	./scripts/sync-ldraw.sh

.PHONY: sync-ldraw-clean
sync-ldraw-clean: ## Remove synced LDraw assets and lockfile
	rm -rf "$(LDRAW_SYNC_DIR)" "$(LDRAW_SYNC_LOCK)"

.PHONY: ldraw-missing-generate
ldraw-missing-generate: ## Generate replacement parts from missing-part catalog
	LDRAW_SYNC_DIR="$(LDRAW_SYNC_DIR)" \
	./scripts/generate-missing-ldraw-parts.sh

.PHONY: ldraw-missing-check
ldraw-missing-check: ## Check model references resolve via synced + generated LDraw assets
	LDRAW_SYNC_DIR="$(LDRAW_SYNC_DIR)" \
	./scripts/check-missing-ldraw-parts.sh

.PHONY: ldraw-missing
ldraw-missing: ldraw-missing-generate ldraw-missing-check ## Generate + validate missing-part replacements

# ── Test & quality ────────────────────────────────────────────────────────────

.PHONY: test
test: check ## Run all tests
	cabal test
	$(MAKE) elm-test
	$(MAKE) test-lib

.PHONY: test-lib
test-lib: viewer-app/src/Data.elm ## Run brick-frame-simulator library unit tests
	cd packages/brick-frame-simulator && elm-test

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
	rm -rf build brick-data-generator .hpc viewer-app/.elm-tailwind viewer-app/elm-stuff viewer-app/src/Data.elm viewer-app/src/.data-nix-stamp build/.wc-stamp build/.wc-stamp-ci
