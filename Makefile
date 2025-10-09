DESCRIPTION = YAML parser
BUILD_DEPS = libyaml
LIBYAML_VSN ?= 0.2.5
dep_libyaml = git https://github.com/yaml/libyaml $(LIBYAML_VSN)

include erl.mk

erl.mk:
ifneq ($(ERL_MK_FILENAME),)
	cp $(ERL_MK_FILENAME) $@
else
	curl -f -s -O https://raw.githubusercontent.com/mbj4668/erl.mk/main/$@
endif

# we need to build the libyaml c-code w/ -fPIC even in the .a file.
dep_patch_libyaml::
	cd $(DEPS_DIR)/libyaml && \
	autoreconf -Wall -vif && \
	./configure CFLAGS="-g -O2 -fPIC" \
		--prefix $(DEPS_DIR)/libyaml/installed && \
	make && make install
