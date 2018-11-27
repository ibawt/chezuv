SCHEME := scheme -q
SCHEME_LIB_DIRS := vendor:.

clean:
	rm -f *.so *.html *.svg

test:
	LD_LIBRARY_PATH=. $(SCHEME) --libdirs $(SCHEME_LIB_DIRS) --debug-on-exception --program ./test.ss

