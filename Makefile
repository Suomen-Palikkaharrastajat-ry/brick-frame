VITE ?= vite --configLoader native
LDRAW_SYNC_URL ?= https://library.ldraw.org/library/updates/complete.zip
LDRAW_SYNC_DIR ?= viewer-app/public/ldraw
LDRAW_SYNC_LOCK ?= ldraw-sync.lock.json
WATCH_PORT ?= 5173
PREVIEW_PORT ?= 4173

.DEFAULT_GOAL := help

.PHONY: help
help: ## Show available targets
	@grep -hE '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; !seen[$$1]++ {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

# ── Vendor / submodules ──────────────────────────────────────────────────────

.PHONY: vendor
vendor: ## Init and update all git submodules and configure symlinks
	@[ -z "$$CI" ] || git config --global url."https://github.com/".insteadOf "git@github.com:"
	@if [ -d .git ]; then \
		if [ -z "$$CI" ] && [ -d ../brick-studio/.git ] && [ -d ../master-builder/.git ]; then \
			git -c submodule."vendor/brick-studio".url="$(abspath ../brick-studio)" \
			    -c submodule."vendor/master-builder".url="$(abspath ../master-builder)" \
			    -c protocol.file.allow=always submodule update --init && \
			git -C vendor/brick-studio \
			    -c submodule."vendor/master-builder".url="$(abspath ../master-builder)" \
			    -c protocol.file.allow=always submodule update --init; \
		else \
			git submodule update --init --recursive; \
		fi; \
	fi
	mkdir -p viewer-app/review/src
	ln -sfn ../vendor/master-builder/packages viewer-app/packages
	ln -sfn ../../../vendor/master-builder/review/src/LlmAgent viewer-app/review/src/LlmAgent

# ── Development environment ──────────────────────────────────────────────────

.PHONY: shell
shell: ## Enter devenv shell
	devenv shell

.PHONY: develop
develop: devenv.local.nix devenv.local.yaml ## Bootstrap devenv shell + VS Code
	devenv shell --profile=devcontainer -- code .

# ── Sources ──────────────────────────────────────────────────────────────────

STUDIO_PACKAGES := vendor/brick-studio/packages
SHARED_ELM_SOURCES := $(shell find $(STUDIO_PACKAGES) -name '*.elm' 2>/dev/null)
VIEWER_APP_SOURCES := $(shell find viewer-app/src -name '*.elm') $(SHARED_ELM_SOURCES)
VIEWPORT_JS_SOURCES := $(shell find $(STUDIO_PACKAGES)/brick-frame-viewport/runtime -name '*.js' 2>/dev/null) $(STUDIO_PACKAGES)/brick-frame-viewport/geometry-worker.js
ELM_PACKAGE_SOURCES := $(shell find vendor/master-builder/packages -name '*.elm' -o -name '*.css' 2>/dev/null)
WC_SOURCES := $(shell find viewer-app/web-components $(STUDIO_PACKAGES)/brick-frame-viewport/runtime -name '*.js' 2>/dev/null)

# ── Development & Build ──────────────────────────────────────────────────────

.PHONY: elm-tailwind-gen
elm-tailwind-gen: viewer-app/.elm-tailwind/.stamp ## Generate typed Tailwind Elm modules into viewer-app

viewer-app/.elm-tailwind/.stamp: viewer-app/elm.json viewer-app/vite.config.mjs viewer-app/main.css $(VIEWER_APP_SOURCES) $(ELM_PACKAGE_SOURCES)
	cd viewer-app && elm-tailwind-classes gen
	mkdir -p viewer-app/.elm-tailwind
	touch $@

.PHONY: elm-dev
elm-dev: elm-tailwind-gen ## Start Elm + Vite dev server (http://localhost:WATCH_PORT)
	cd viewer-app && $(VITE) --host 0.0.0.0 --port $(WATCH_PORT)

.PHONY: frame-dev
frame-dev: elm-dev ## Start the viewer-app Vite dev server (http://localhost:WATCH_PORT)

.PHONY: dev
dev: elm-dev ## Start Vite dev server (WATCH_PORT)

.PHONY: watch
watch: dev ## Alias for dev (start Vite dev server on WATCH_PORT)

build/.elm-stamp: viewer-app/.elm-tailwind/.stamp $(VIEWER_APP_SOURCES) $(ELM_PACKAGE_SOURCES) $(VIEWPORT_JS_SOURCES) viewer-app/elm.json viewer-app/vite.config.mjs viewer-app/index.html viewer-app/main.js viewer-app/main.css $(STUDIO_PACKAGES)/brick-frame-ui/theme.css
	mkdir -p build
	cd viewer-app && $(VITE) build --outDir ../build
	touch $@

.PHONY: elm-build
elm-build: build/.elm-stamp ## Production build of Elm SPA → build/

build/.wc-stamp: viewer-app/elm.json viewer-app/vite.web-components.config.mjs viewer-app/src/Main.elm viewer-app/src/Ports.elm $(STUDIO_PACKAGES)/brick-frame-viewport/geometry-worker.js viewer-app/main.css $(WC_SOURCES)
	mkdir -p build
	cd viewer-app && $(VITE) build --config vite.web-components.config.mjs
	touch $@

.PHONY: wc-build
wc-build: build/.wc-stamp ## Production build of Web Components bundle

.PHONY: frame-build
frame-build: elm-build wc-build ## Production build of viewer-app and Web Components bundle → build/

.PHONY: build
build: frame-build ## Build viewer-app and Web Components bundle → build/

.PHONY: all
all: clean build ## Clean and rebuild everything

.PHONY: preview
preview: build ## Preview production build locally (PREVIEW_PORT)
	cd viewer-app && $(VITE) preview --outDir ../build --host 0.0.0.0 --port $(PREVIEW_PORT)

.PHONY: serve
serve: preview ## Alias for preview

.PHONY: watch-docs
watch-docs: ## Start Vite for web-components docs playground watch mode
	@echo "Open http://localhost:$(WATCH_PORT)/docs/index.watch.html"
	cd viewer-app && $(VITE) --port $(WATCH_PORT)

# ── LDraw Asset Sync & Missing Parts ─────────────────────────────────────────

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

# ── Test & Quality ───────────────────────────────────────────────────────────

JS_TESTS := $(wildcard *.test.mjs viewer-app/*.test.mjs viewer-app/web-components/*.test.mjs)

.PHONY: elm-test
elm-test: elm-tailwind-gen ## Run Elm unit tests (viewer-app)
	cd viewer-app && elm-test

.PHONY: js-test
js-test: ## Run JS unit tests (*.test.mjs), if any
	$(if $(JS_TESTS),node --test $(JS_TESTS),@echo "No JS unit tests in viewer-app")

.PHONY: test
test: check elm-test js-test ## Run check, Elm tests and JS tests

.PHONY: elm-check
elm-check: ## Check Elm formatting (no changes)
	cd viewer-app && elm-format --validate src/ tests/

.PHONY: elm-format
elm-format: ## Auto-format Elm source files
	cd viewer-app && elm-format --yes src/ tests/

.PHONY: elm-review
elm-review: ## Run elm-review LLM-agent rules on viewer-app
	cd viewer-app && elm-review

.PHONY: review
review: elm-review ## Alias for elm-review

.PHONY: elm-review-fix
elm-review-fix: ## Run elm-review with auto-fix
	cd viewer-app && elm-review --fix

.PHONY: review-fix
review-fix: elm-review-fix ## Alias for elm-review-fix

.PHONY: check
check: elm-check elm-review ## Run elm-check and elm-review

.PHONY: format
format: elm-format ## Auto-format source files
	treefmt

.PHONY: fmt
fmt: format ## Alias for format

# ── Cleanup ───────────────────────────────────────────────────────────────────

.PHONY: clean
clean: ## Remove build artifacts and generated output
	rm -rf build viewer-app/.elm-tailwind viewer-app/elm-stuff viewer-app/.vite build/.elm-stamp build/.wc-stamp
