# SRC=$(shell find ./ -name "*.ss")
# SRC += $(shell find ./ -name "*.sls")
# OBJ=$(SRC:.ss=.so)
# OBJ+=$(SRC:.sls=.so)

# SCHEME_FLAGS := --optimize-level 3
CFLAGS := -I/usr/local/opt/openssl@1.1/include -L/usr/local/opt/openssl@1.1/lib -lssl -lcrypto -Wall
all: libchezuv.dylib

clean:
	rm -rf *.dylib
# %.so: %.ss
# 	@echo '(parameterize ([compile-profile (quote source)]) (compile-file "$<"))' | scheme -q $(SCHEME_FLAGS) --libdirs vendor:.

# %.so: %.sls
# 	@echo '(parameterize ([compile-profile (quote source)]) (compile-file "$<"))' | scheme -q $(SCHEME_FLAGS) --libdirs vendor:.

# clean:
# 	find ./ -name "*.so" -exec rm {} \;
# 	rm -f *.html

libchezuv.dylib: lib.c
	$(CC) $(CFLAGS) -dynamiclib -o libchezuv.dylib lib.c

