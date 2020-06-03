PROJECT = eyaml
PROJECT_VERSION = 0.1

DEPS     = libyaml
LIBYAML_VSN ?= 0.2.5
dep_libyaml = git https://github.com/yaml/libyaml $(LIBYAML_VSN)

C_SRC_DIR = ./c_src
C_SRC_OUTPUT = ./priv/eyaml_nif

CFLAGS = -I $(DEPS_DIR)/libyaml/installed/include
LDLIBS = $(DEPS_DIR)/libyaml/installed/lib/libyaml.a


include erlang.mk

# we need to build the libyaml c-code w/ -fPIC even in the .a file.
# perhaps there's a better way?
autopatch-libyaml::
	cd $(DEPS_DIR)/libyaml && \
	autoreconf -Wall -vif && \
	./configure CFLAGS="-g -O2 -fPIC" \
		--prefix $(DEPS_DIR)/libyaml/installed && \
	make && make install
