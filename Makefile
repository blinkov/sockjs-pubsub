REBAR = ./rebar
DIALYZER = dialyzer

DIALYZER_WARNINGS = -Wunmatched_returns -Werror_handling \
                    -Wrace_conditions -Wunderspecs


all: app

compile:
	@$(REBAR) compile

app: get-deps compile

get-deps:
	@$(REBAR) get-deps

update-deps:
	@$(REBAR) update-deps

clean:
	@$(REBAR) clean
	rm -f ./erl_crash.dump ./src/erl_crash.dump

dist-clean: clean

generate:
	rm -rf ./rel/dev
	@$(REBAR) generate
	
rel: generate

build-plt:
	@$(DIALYZER) --build_plt --output_plt .dialyzer_plt \
	    --apps kernel stdlib

test: compile
	@$(REBAR) eunit skip_deps=true

dialyze: compile
	@$(DIALYZER) --src src --plt .dialyzer_plt $(DIALYZER_WARNINGS) | \
	    fgrep -vf .dialyzer-ignore-warnings
