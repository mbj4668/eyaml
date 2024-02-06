NIF = ../priv/eyaml_nif.so

all: $(NIF)

include c_src.mk

c_src.mk:
	$(MAKE) -f ../erl.mk $@

CFLAGS += -I $(DEPS_DIR)/libyaml/installed/include
LIBS = $(DEPS_DIR)/libyaml/installed/lib/libyaml.a

SOURCES := $(wildcard *.c)
OBJS := $(SOURCES:%.c=./%.o)

$(NIF): $(OBJS)
	$(CC) $(LDFLAGS_NIF) -o $@ $^ $(LIBS)

debug:
	$(MAKE) DEBUG=true all

clean:
	rm -f ../priv/*.so ./*.so ./*.o *.o .*.d
