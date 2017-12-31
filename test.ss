;; -*- geiser-scheme-implementation: chez -*-
(import (chezscheme)
        (srfi s64 testing)
        (uv))

(test-begin "url-parsing")
(let ([url (uv/string->url "http://google.ca")])
  (test-equal "google.ca" (uv/url-host url))
  (test-eqv 80 (uv/url-port url))
  (test-equal "/" (uv/url-path url)))

(let ([url (uv/string->url "https://google.ca:8080/foo/bar")])
  (test-equal "google.ca" (uv/url-host url))
  (test-equal 'https (uv/url-protocol url))
  (test-eqv 8080 (uv/url-port url))
  (test-equal "/foo/bar" (uv/url-path url)))
(test-end "url-parsing")

(include "utils.ss")

(test-begin "string split")
(let ([splits (string-split "0.0.0.0:4343" #\:)])
  (format #t "splits: ~a\n" splits)
  (test-equal '("0.0.0.0" "4343") splits))
(test-end "string split")
