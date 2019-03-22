SCHEME := scheme -q
SCHEME_LIB_DIRS := vendor:src:test

RUN_SCHEME := $(SCHEME) --libdirs $(SCHEME_LIB_DIRS) --debug-on-exception --program

.PHONY: test docker_image clean

test: test/fixtures/nginx/cert.pem
	@$(SCHEME) --libdirs $(SCHEME_LIB_DIRS) --debug-on-exception --program ./test/test.ss

docker_image:
	@docker build ./ -f test/Dockerfile -t chezuv

server:
	@$(RUN_SCHEME) bin/run.ss

test/fixtures/nginx/key.pem:
	@openssl genrsa -out test/fixtures/nginx/key.pem 2048 2> /dev/null

test/fixtures/nginx/cert.pem: test/fixtures/nginx/key.pem
	@openssl req -new -x509 -key test/fixtures/nginx/key.pem -out fixtures/nginx/cert.pem -days 3650 -subj="/C=CA/ST=Ottawa/L=Ottawa/O=Foobar/OU=Foobar/CN=example.com"

clean:
	@rm -f *.so *.html *.svg fixtures/nginx/cert.pem fixtures/nginx/key.pem
