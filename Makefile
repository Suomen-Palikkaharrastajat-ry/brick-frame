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
WATCH_PORT ?= 5173

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
	ln -sfn ../vendor/master-builder/packages studio-app/packages
	ln -sfn ../../../vendor/master-builder/review/src/LlmAgent studio-app/review/src/LlmAgent
	ln -sfn ../vendor/master-builder/packages brick-play/packages
	ln -sfn ../../../vendor/master-builder/review/src/LlmAgent brick-play/review/src/LlmAgent

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
SHARED_ELM_SOURCES := $(shell find packages/brick-frame-viewport/src packages/brick-frame-simulation/src packages/brick-frame-simulator/src packages/brick-frame-ui/src packages/brick-frame-studio/src -name '*.elm' ! -name 'ColorData.elm')
VIEWER_APP_SOURCES := $(shell find viewer-app/src -name '*.elm') $(SHARED_ELM_SOURCES) packages/brick-frame-data/src/EmbeddedParts.elm
STUDIO_APP_SOURCES := $(shell find studio-app/src -name '*.elm') $(SHARED_ELM_SOURCES) packages/brick-frame-data/src/EmbeddedParts.elm
PLAY_APP_SOURCES := $(shell find brick-play/src -name '*.elm') $(SHARED_ELM_SOURCES) packages/brick-frame-data/src/EmbeddedParts.elm
VIEWPORT_JS_SOURCES := $(shell find packages/brick-frame-viewport/runtime -name '*.js') packages/brick-frame-viewport/geometry-worker.js
# Shared package code that is elm-format clean. The older simulator modules and
# the generated ColorData module are not validated yet.
SHARED_PACKAGE_DIRS := packages/brick-frame-data/src/EmbeddedParts.elm packages/brick-frame-viewport packages/brick-frame-simulation packages/brick-frame-ui packages/brick-frame-studio packages/brick-frame-simulator/src/LDraw/Document.elm packages/brick-frame-simulator/src/LDraw/Export.elm packages/brick-frame-simulator/src/LDraw/Export packages/brick-frame-simulator/src/LDraw/Format.elm packages/brick-frame-simulator/src/LDraw/Writer.elm packages/brick-frame-simulator/src/Render/Pick.elm packages/brick-frame-simulator/src/Render/Shader.elm packages/brick-frame-simulator/src/Render/Style.elm packages/brick-frame-simulator/tests/LDraw/DocumentTest.elm packages/brick-frame-simulator/tests/LDraw/ExportTest.elm packages/brick-frame-simulator/tests/LDraw/Fixtures.elm packages/brick-frame-simulator/tests/Render/PickTest.elm packages/brick-frame-simulator/tests/Render/StyleTest.elm packages/brick-frame-simulator/src/LDraw/Flex.elm packages/brick-frame-simulator/src/LDraw/Steps.elm packages/brick-frame-simulator/src/Gear/Joint.elm packages/brick-frame-simulator/src/Gear/Physics.elm packages/brick-frame-simulator/src/LDraw/Connectivity.elm packages/brick-frame-simulator/src/LDraw/ColorAvailability.elm packages/brick-frame-simulator/tests/LDraw/StepsTest.elm packages/brick-frame-simulator/tests/LDraw/FlexTest.elm packages/brick-frame-simulator/tests/LDraw/ConnectivityTest.elm packages/brick-frame-simulator/tests/LDraw/ColorAvailabilityTest.elm packages/brick-frame-simulator/tests/Gear/JointTest.elm packages/brick-frame-simulator/tests/Gear/PhysicsTest.elm packages/brick-frame-simulator/src/LDraw/Catalog.elm packages/brick-frame-simulator/src/LDraw/BOM.elm packages/brick-frame-simulator/tests/LDraw/BOMTest.elm
ELM_PACKAGE_SOURCES := $(shell find vendor/master-builder/packages -name '*.elm' -o -name '*.css' 2>/dev/null)
WC_SOURCES := $(shell find viewer-app/web-components packages/brick-frame-viewport/runtime -name '*.js' 2>/dev/null)

brick-data-generator: $(HS_SOURCES)
	cabal build generator
	cp $$(cabal list-bin generator) $@

GENERATED_ELM := packages/brick-frame-data/src/Data.elm packages/brick-frame-simulator/src/LDraw/ColorData.elm

.PHONY: generate
generate: packages/brick-frame-data/src/Data.elm ## Run Haskell generator to produce generated Elm data modules

# Both generated modules come out of a single generator run; Data.elm stands in
# for the pair as the make target.
packages/brick-frame-data/src/Data.elm: brick-data-generator viewer-app/public/ldraw/LDConfig.ldr
	./brick-data-generator
	elm-format --yes $(GENERATED_ELM)

packages/brick-frame-data/src/.data-nix-stamp:
	$(GENERATOR_NIX)
	touch $@

.PHONY: elm-dev
elm-dev: packages/brick-frame-data/src/Data.elm ## Start Elm + Vite dev server (hot reload)
	cd viewer-app && $(VITE) --host 0.0.0.0 --port $(WATCH_PORT)


.PHONY: studio-dev
studio-dev: packages/brick-frame-data/src/Data.elm studio-app/.elm-tailwind/.stamp ## Start the studio's Vite dev server (http://localhost:WATCH_PORT)
	cd studio-app && $(VITE) --host 0.0.0.0 --port $(WATCH_PORT)


.PHONY: play-dev
play-dev: packages/brick-frame-data/src/Data.elm brick-play/.elm-tailwind/.stamp ## Start the brick-play Vite dev server (http://localhost:WATCH_PORT)
	cd brick-play && $(VITE) --host 0.0.0.0 --port $(WATCH_PORT)


.PHONY: elm-tailwind-gen
elm-tailwind-gen: viewer-app/.elm-tailwind/.stamp studio-app/.elm-tailwind/.stamp brick-play/.elm-tailwind/.stamp ## Generate typed Tailwind Elm modules into viewer-app, studio-app, brick-play

studio-app/.elm-tailwind/.stamp: studio-app/elm.json studio-app/vite.config.mjs studio-app/main.css $(STUDIO_APP_SOURCES) $(ELM_PACKAGE_SOURCES) packages/brick-frame-data/src/Data.elm
	cd studio-app && elm-tailwind-classes gen
	mkdir -p studio-app/.elm-tailwind
	touch $@

viewer-app/.elm-tailwind/.stamp: viewer-app/elm.json viewer-app/vite.config.mjs viewer-app/main.css $(VIEWER_APP_SOURCES) $(ELM_PACKAGE_SOURCES) packages/brick-frame-data/src/Data.elm
	cd viewer-app && elm-tailwind-classes gen
	mkdir -p viewer-app/.elm-tailwind
	touch $@

brick-play/.elm-tailwind/.stamp: brick-play/elm.json brick-play/vite.config.mjs brick-play/main.css $(PLAY_APP_SOURCES) $(ELM_PACKAGE_SOURCES) packages/brick-frame-data/src/Data.elm
	cd brick-play && elm-tailwind-classes gen
	mkdir -p brick-play/.elm-tailwind
	touch $@

build/.elm-stamp: viewer-app/.elm-tailwind/.stamp $(VIEWER_APP_SOURCES) $(ELM_PACKAGE_SOURCES) $(VIEWPORT_JS_SOURCES) viewer-app/elm.json viewer-app/vite.config.mjs viewer-app/index.html viewer-app/main.js viewer-app/main.css packages/brick-frame-ui/theme.css
	cd viewer-app && $(VITE) build
	touch $@

build/.elm-stamp-ci: packages/brick-frame-data/src/.data-nix-stamp viewer-app/.elm-tailwind/.stamp $(VIEWER_APP_SOURCES) $(ELM_PACKAGE_SOURCES) $(VIEWPORT_JS_SOURCES) viewer-app/elm.json viewer-app/vite.config.mjs viewer-app/index.html viewer-app/main.js viewer-app/main.css packages/brick-frame-ui/theme.css
	cd viewer-app && $(VITE) build
	touch $@

# The viewer build empties build/, so the other apps are rebuilt whenever it runs.
build/.studio-stamp: build/.elm-stamp studio-app/.elm-tailwind/.stamp $(STUDIO_APP_SOURCES) $(ELM_PACKAGE_SOURCES) $(VIEWPORT_JS_SOURCES) studio-app/elm.json studio-app/vite.config.mjs studio-app/index.html studio-app/main.js studio-app/main.css packages/brick-frame-ui/theme.css
	cd studio-app && $(VITE) build
	touch $@

build/.studio-stamp-ci: build/.elm-stamp-ci packages/brick-frame-data/src/.data-nix-stamp studio-app/.elm-tailwind/.stamp $(STUDIO_APP_SOURCES) $(ELM_PACKAGE_SOURCES) $(VIEWPORT_JS_SOURCES) studio-app/elm.json studio-app/vite.config.mjs studio-app/index.html studio-app/main.js studio-app/main.css packages/brick-frame-ui/theme.css
	cd studio-app && $(VITE) build
	touch $@

build/.play-stamp: build/.elm-stamp brick-play/.elm-tailwind/.stamp $(PLAY_APP_SOURCES) $(ELM_PACKAGE_SOURCES) $(VIEWPORT_JS_SOURCES) brick-play/elm.json brick-play/vite.config.mjs brick-play/index.html brick-play/main.js brick-play/main.css packages/brick-frame-ui/theme.css
	cd brick-play && $(VITE) build
	touch $@

build/.play-stamp-ci: build/.elm-stamp-ci packages/brick-frame-data/src/.data-nix-stamp brick-play/.elm-tailwind/.stamp $(PLAY_APP_SOURCES) $(ELM_PACKAGE_SOURCES) $(VIEWPORT_JS_SOURCES) brick-play/elm.json brick-play/vite.config.mjs brick-play/index.html brick-play/main.js brick-play/main.css packages/brick-frame-ui/theme.css
	cd brick-play && $(VITE) build
	touch $@

.PHONY: studio-build
studio-build: build/.studio-stamp ## Production build of the studio → build/studio/

.PHONY: play-build
play-build: build/.play-stamp ## Production build of brick-play → build/play/

.PHONY: redirect-build
redirect-build: ## Copy the old Brick Contraptions address → build/contraptions/, which forwards to Brick Play
	mkdir -p build/contraptions
	cp redirects/contraptions/index.html build/contraptions/index.html

.PHONY: elm-build
elm-build: build/.elm-stamp ## Production build of Elm SPA → build/

build/.wc-stamp: packages/brick-frame-data/src/Data.elm viewer-app/elm.json viewer-app/vite.web-components.config.mjs viewer-app/src/Main.elm viewer-app/src/Ports.elm packages/brick-frame-viewport/geometry-worker.js viewer-app/main.css $(WC_SOURCES)
	cd viewer-app && $(VITE) build --config vite.web-components.config.mjs
	touch $@

build/.wc-stamp-ci: packages/brick-frame-data/src/.data-nix-stamp viewer-app/elm.json viewer-app/vite.web-components.config.mjs viewer-app/src/Main.elm viewer-app/src/Ports.elm packages/brick-frame-viewport/geometry-worker.js viewer-app/main.css $(WC_SOURCES)
	cd viewer-app && $(VITE) build --config vite.web-components.config.mjs
	touch $@

.PHONY: wc-build
wc-build: build/.wc-stamp ## Production build of Web Components bundle

.PHONY: elm-test
elm-test: elm-tailwind-gen packages/brick-frame-data/src/Data.elm ## Run Elm unit tests (viewer, studio and play apps)
	cd viewer-app && elm-test
	cd studio-app && elm-test
	cd brick-play && elm-test

.PHONY: elm-check
elm-check: ## Check Elm formatting (no changes)
	cd viewer-app && elm-format --validate src/ tests/
	cd studio-app && elm-format --validate src/ tests/
	cd brick-play && elm-format --validate src/ tests/
	elm-format --validate $(SHARED_PACKAGE_DIRS)

.PHONY: elm-format
elm-format: ## Auto-format Elm source files
	cd viewer-app && elm-format --yes src/ tests/
	cd studio-app && elm-format --yes src/ tests/
	cd brick-play && elm-format --yes src/ tests/
	elm-format --yes $(SHARED_PACKAGE_DIRS)

.PHONY: elm-review
elm-review: ## Run elm-review LLM-agent rules (no changes) on all apps and the shared packages
	cd viewer-app && elm-review
	cd studio-app && elm-review
	cd brick-play && elm-review

.PHONY: elm-review-fix
elm-review-fix: ## Run elm-review with auto-fix
	cd viewer-app && elm-review --fix
	cd studio-app && elm-review --fix
	cd brick-play && elm-review --fix

# ── Combined targets ──────────────────────────────────────────────────────────

.PHONY: dev
dev: elm-dev ## Generate data + start Vite dev server

.PHONY: build
build: elm-build wc-build studio-build play-build redirect-build ## Generate data + production build of all apps → build/

.PHONY: preview
preview: build ## Serve production build of all apps locally on port 4173 (http://localhost:4173)
	cd viewer-app && $(VITE) preview --outDir ../build --host 0.0.0.0 --port 4173

.PHONY: serve
serve: preview ## Alias for preview

.PHONY: dist-ci
dist-ci: build/.elm-stamp-ci build/.wc-stamp-ci build/.studio-stamp-ci build/.play-stamp-ci redirect-build ## CI build: generator (Nix binary) + viewer + web components + studio + play + the Contraptions redirect → build/

.PHONY: watch
watch: ## Watch Haskell sources and run Vite dev server (WATCH_PORT, default 5173)
	make generate
	find src generator brick-frame.cabal -name "*.hs" -o -name "*.cabal" | entr -s 'make generate' &
	cd viewer-app && elm-tailwind-classes gen && $(VITE) --host 0.0.0.0 --port $(WATCH_PORT)

.PHONY: watch-docs
watch-docs: ## Start Vite for web-components docs playground watch mode (WATCH_PORT, default 5173)
	$(MAKE) generate
	@echo "Open http://localhost:$(WATCH_PORT)/docs/index.watch.html"
	cd viewer-app && $(VITE) --port $(WATCH_PORT)

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

.PHONY: ldraw-index
ldraw-index: ## Build the studio's part index (parts-index.json) from the synced library
	node scripts/build-part-index.mjs "$(LDRAW_SYNC_DIR)"

.PHONY: ldraw-index-test
ldraw-index-test: ## Test the part index builder
	node --test scripts/*.test.mjs packages/brick-frame-viewport/runtime/pathtracer/*.test.mjs

# The index runs last so it lists the generated replacement parts too.
.PHONY: ldraw-missing
ldraw-missing: ldraw-missing-generate ldraw-missing-check ldraw-index ## Generate + validate missing-part replacements, then rebuild the part index

# ── Test & quality ────────────────────────────────────────────────────────────

.PHONY: test
test: check ## Run all tests
	cabal test
	$(MAKE) ldraw-index-test
	$(MAKE) elm-test
	$(MAKE) test-lib

.PHONY: test-lib
test-lib: packages/brick-frame-data/src/Data.elm ## Run the shared package unit tests (simulator, viewport, simulation, ui, studio)
	cd packages/brick-frame-simulator && elm-test
	cd packages/brick-frame-viewport && elm-test
	cd packages/brick-frame-simulation && elm-test
	cd packages/brick-frame-ui && elm-test
	cd packages/brick-frame-studio && elm-test

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
	rm -rf build brick-data-generator .hpc viewer-app/.elm-tailwind viewer-app/elm-stuff studio-app/.elm-tailwind studio-app/elm-stuff brick-play/.elm-tailwind brick-play/elm-stuff packages/brick-frame-studio/elm-stuff packages/brick-frame-data/src/Data.elm packages/brick-frame-data/src/.data-nix-stamp build/.wc-stamp build/.wc-stamp-ci build/.studio-stamp build/.studio-stamp-ci build/.play-stamp build/.play-stamp-ci
