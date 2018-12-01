SCHEME := scheme -q
SCHEME_LIB_DIRS := vendor:.

.PHONY: test docker_image clean

clean:
	rm -f *.so *.html *.svg

test:
	@$(SCHEME) --libdirs $(SCHEME_LIB_DIRS) --debug-on-exception --program ./test.ss

docker_image:
	docker build ./ -f test/Dockerfile -t chezuv

