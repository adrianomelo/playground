REBAR=rebar
ELVIS=$(PWD)/deps/elvis/elvis
RM=rm -fr

all: deps compile

deps: get-deps $(ELVIS) compile-all

get-deps:
	$(REBAR) get-deps

$(ELVIS):
	cd $(PWD)/deps/elvis; make escript;

compile-all:
	$(REBAR) compile

compile:
	$(REBAR) skip_deps=true compile
# $(REBAR) skip_deps=true xref

clean:
	$(REBAR) clean
	$(RM) deps

tests: elvis
	$(REBAR) compile eunit

elvis:
	$(ELVIS) rock

$(DEPS_PLT):
	@echo Building local plt at $(DEPS_PLT)
	@echo
	dialyzer --output_plt $(DEPS_PLT) --build_plt --apps $(DEPS) -r deps

dialyzer: $(DEPS_PLT) compile
	dialyzer --fullpath --plt $(DEPS_PLT) -Wrace_conditions -r ./ebin

shell: compile run-shell

run-shell:
	erl -pa deps/*/ebin/ ebin/

run: compile
	erl -pa deps/*/ebin ebin/ -noshell -eval "application:ensure_all_started(websocket), websocket_app:run()."
