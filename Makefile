SRC=$(shell find ./ -name "*.ss")
SRC += $(shell find ./ -name "*.sls")
OBJ=$(SRC:.ss=.so)
OBJ+=$(SRC:.sls=.so)

SCHEME_FLAGS := --optimize-level 3

all: $(OBJ)

%.so: %.ss
	@echo '(parameterize ([compile-profile (quote source)]) (compile-file "$<"))' | scheme -q $(SCHEME_FLAGS) --libdirs vendor:.

%.so: %.sls
	@echo '(parameterize ([compile-profile (quote source)]) (compile-file "$<"))' | scheme -q $(SCHEME_FLAGS) --libdirs vendor:.

clean:
	find ./ -name "*.so" -exec rm {} \;
	rm -f *.html
