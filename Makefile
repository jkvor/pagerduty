# -*- mode: Makefile; fill-column: 80; comment-column: 75; -*-

ERL = $(shell which erl)

ERLFLAGS= -pa $(CURDIR)/.eunit -pa $(CURDIR)/ebin -pa $(CURDIR)/*/ebin

REBAR=./rebar

PAGERDUTY_PLT=$(CURDIR)/.depsolver_plt

.PHONY: dialyzer typer clean distclean test

compile:
	@$(REBAR) get-deps compile

$(PAGERDUTY_PLT):
	dialyzer --output_plt $(PAGERDUTY_PLT) --build_plt \
		--apps erts kernel stdlib crypto public_key -r deps --fullpath

dialyzer: $(PAGERDUTY_PLT)
	dialyzer --plt $(PAGERDUTY_PLT) -pa deps/* --src src

typer: $(PAGERDUTY_PLT)
	typer --plt $(PAGERDUTY_PLT) -r ./src

clean:
	$(REBAR) clean

distclean: clean
	rm $(PAGERDUTY_PLT)
	rm -rvf $(CURDIR)/deps/*

test: REBAR := REBAR_TEST_DEPS=1 $(REBAR)
test: compile
	@$(REBAR) ct
