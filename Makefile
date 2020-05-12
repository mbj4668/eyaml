PROJECT = eyaml
PROJECT_VERSION = 0.1

C_SRC_DIR = ./c_src
C_SRC_OUTPUT = ./priv/eyaml_nif

LDLIBS = -lyaml

include erlang.mk
