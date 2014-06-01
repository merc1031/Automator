DIRS_TOPS := deps automator

DIRS_APPS := $(filter-out %.txt %.config, $(foreach top, $(DIRS_TOPS), $(wildcard $(top)/*)))
REBAR := ./bin/rebar
REL := ./rel/automator

SRCS_APPS := $(foreach app, $(DIRS_APPS), $(wildcard $(app)/src/*.app))
BINS_APPS :=  $(subst /src/,/ebin/, $(SRCS_APPS))

AUTOMATOR_PLT := .automator_plt

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

production-compile: check_env
	$(REBAR) -DNOTEST compile

build-plt: check_env
	@echo 'You may as well go get a coffee.....'
	@dialyzer --build_plt --output_plt ${AUTOMATOR_PLT} --apps erts kernel stdlib sasl crypto ssl inets tools xmerl runtime_tools compiler syntax_tools hipe mnesia

check-plt: check_env
	@dialyzer --check_plt --plt ${AUTOMATOR_PLT}

dialyzer: check_env check-plt clean production-compile
	@find . -name ebin | grep -v ${REL} | xargs dialyzer -Wno_undefined_callbacks --plt ${AUTOMATOR_PLT} -r

define apps_rule
$(subst /src/,/ebin/, $(1)):   check_env $(1)
	@echo "Installing $(1)"
	@cp $(1) $$@
endef

$(foreach app, $(SRCS_APPS), $(eval $(call apps_rule, $(app))))

check_env:
	@test "${ERL_LIBS}" != "" || (echo 'Erlang development environment not set. Try this:' && echo './environment.source make [target]' && exit 1)

