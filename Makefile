REBAR 	  	:= ./rebar3
REBAR_FILE	:= rebar3
REBAR_URL 	:= https://s3.amazonaws.com/rebar3/
ERL       	?= erl
SOURCE_URL	:= https://raw.githubusercontent.com/ua-parser/uap-core/master/
SOURCE_FILE := regexes.yaml
TESTS_URL	:= https://raw.githubusercontent.com/ua-parser/uap-core/master/tests/
TESTS_FILES := test_device.yaml test_os.yaml test_ua.yaml

define download
	@printf ""
	@[[ -f $(3)/$(1) ]] && (rm -f $(3)/$(1) && echo "Removed existing file.") || printf ""
	@printf "Downloading $(2)$(1) to $(3)/$(1)... "
	@$(ERL) -noshell -s inets -s ssl \
	  -eval '{ok, saved_to_file} = httpc:request(get, {"$(2)$(1)", []}, [], [{stream, "$(3)/$(1)"}])' \
	  -s init stop
	@[[ -f $(3)/$(1) ]] && echo "done." || echo "failed."
endef

.PHONY: sources compile test help

all: help

compile: $(REBAR)  ## compile erlang source code
	@$(REBAR) as developer compile

shell: $(REBAR)  ## start an erlang shell from here
	@$(REBAR) as developer shell

test: $(REBAR) compile ## run test suite
	@$(REBAR) as developer eunit

clean: $(REBAR)  ## clean project build artefacts
	@$(REBAR) as developer clean

distclean:  ## clean all build artefacts
	@rm -rf _build
	@rm -rf ebin
	@rm -f $(REBAR)
	@rm -f ./priv/*.yaml
	@[[ ! -f $(REBAR) && ! -d _build && ! -d ebin ]] && echo "All build artefacts removed"

sources:  ## obtain source files from uap-core
	$(call download,$(SOURCE_FILE),$(SOURCE_URL),./priv)
	$(foreach var,$(TESTS_FILES),$(call download,$(var),$(TESTS_URL),./priv))

./rebar3:  ## download rebar3 executable
	$(call download,$(REBAR_FILE),$(REBAR_URL),.)
	@chmod +x ./rebar3

help: ## display this help
	@echo "Options:"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-18s\033[0m %s\n", $$1, $$2}'
