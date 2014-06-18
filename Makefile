REPO		?= automator
PKG_REVISION		?= $(shell git describe --tags)
PKG_VERSION		?= $(shell git describe --tags | tr - .)
PKG_ID			= automator-$(PKG_VERSION)
PKG_BUILD		= 1
BASE_DIR		= $(shell pwd)
ERLANG_BIN		= $(shell dirname $(shell which erl))
REBAR			?= $(BASE_DIR)/bin/rebar
OVERLAY_VARS		?=
SANDBOX_CONFIG		?=

.PHONY: rel deps all compile

all: compile

compile: deps
	@(./bin/rebar compile)

deps:
	@./bin/rebar get-deps

clean:
	@./bin/rebar clean

lock: deps compile
	@./bin/rebar lock-deps

locked-all: locked-deps compile

locked-deps:
	@echo "Using rebar.config.lock file to fetch deps"
	@./bin/rebar -C rebar.config.lock get-deps

distclean:
	@./bin/rebar delete-deps
	@rm -rf $(PKG_ID).tar.gz

rel: deps compile
	@./bin/rebar generate skip_deps=true $(OVERLAY_VARS)

relclean:
	rm -rf rel/automator

sandbox: sandboxclean rel
	@rel/automator/bin/automator stop || :
	@mkdir -p sandbox/automator
	@cp -r rel/automator sandbox/.
	@cp $(SANDBOX_CONFIG) sandbox/automator/etc/automator.conf
	@./sandbox/automator/bin/automator stop || :
	@./sandbox/automator/bin/automator start

sandboxclean:
	@./sandbox/automator/bin/automator stop || :
	@rm -rf sandbox/automator

.PHONY: package
export PKG_VERSION PKG_ID PKG_BUILD BASE_DIR ERLANG_BIN REBAR OVERLAY_VARS RELEASE

package.src: deps
	mkdir -p package
	rm -rf package/$(PKG_ID)
	git archive --format=tar --prefix=$(PKG_ID)/ $(PKG_REVISION)| (cd package && tar -xf -)
	${MAKE} -C package/$(PKG_ID) deps
	for dep in package/$(PKG_ID)/deps/*; do \
		echo "Processing dep: $${dep}"; \
		mkdir -p $${dep}/priv; \
		git --git-dir=$${dep}/.git describe --tags >$${dep}/priv/vsn.git; \
	done
	find package/$(PKG_ID) -depth -name ".git" -exec rm -rf {} \;
	tar -C package -czf package/$(PKG_ID).tar.gz $(PKG_ID)

dist: package.src
	cp package/$(PKG_ID).tar.gz .

package: package.src
	${MAKE} -C package -f $(PKG_ID)/deps/node_package/Makefile

pkgclean: distclean
	rm -rf package
