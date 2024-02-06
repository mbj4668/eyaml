BUILD_DEPS = libyaml
LIBYAML_VSN ?= 0.2.5
dep_libyaml = git https://github.com/yaml/libyaml $(LIBYAML_VSN)

include erl.mk

erl.mk:
	curl -s -O https://raw.githubusercontent.com/mbj4668/erl.mk/main/$@

# we need to build the libyaml c-code w/ -fPIC even in the .a file.
dep_patch_libyaml::
	cd $(DEPS_DIR)/libyaml && \
	autoreconf -Wall -vif && \
	./configure CFLAGS="-g -O2 -fPIC" \
		--prefix $(DEPS_DIR)/libyaml/installed && \
	make && make install
