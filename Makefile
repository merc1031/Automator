DIRS_TOPS := deps automator

DIRS_APPS := $(filter-out %.txt %.config, $(foreach top, $(DIRS_TOPS), $(wildcard $(top)/*)))
REBAR := ./bin/rebar
REL := ./rel/automator

SRCS_APPS := $(foreach app, $(DIRS_APPS), $(wildcard $(app)/src/*.app))
BINS_APPS :=  $(subst /src/,/ebin/, $(SRCS_APPS))

test:	get_deps check_env tab-test all

test-all:	check_env tab-test all

rebuild:	check_env clean all

get_deps:
	$(REBAR) get-deps

#Note: the grep character is a tab literal
tab-test:
	@echo "Checking for tab characters..."
	@if grep -nr '	' automator --include=\*.erl; then echo "ERROR: !!! TABS FOUND !!!"; false; else true; fi

all: check_env generate \
		$(addsuffix /gen, $(DIRS_APPS))

compile: check_env
	$(REBAR) compile

clean:	check_env
	$(REBAR) clean
	rm -rf ${REL}
	rm -rf rel/automator_*
	rm -f */*/gen/* erl_crash.dump logs/*

clean-gen-beam:	check_env
	$(REBAR) clean
	rm -f */*/gen/* erl_crash.dump logs/*

clean-release: check_env
	@rm -rf ${REL}

generate: check_env clean-release compile
	$(REBAR) generate

%/gen: check_env
	mkdir -p $@

define apps_rule
$(subst /src/,/ebin/, $(1)):   check_env $(1)
	@echo "Installing $(1)"
	@cp $(1) $$@
endef

$(foreach app, $(SRCS_APPS), $(eval $(call apps_rule, $(app))))

check_env:
	@test "${ERL_LIBS}" != "" || (echo 'Erlang development environment not set. Try this:' && echo './environment.source make [target]' && exit 1)

