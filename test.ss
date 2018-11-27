;; -*- geiser-scheme-implementation: chez -*-
(import (chezscheme)
        (srfi s64 testing)
        (uv))

(test-begin "chezuv")

(define-syntax it
  (syntax-rules ()
    ((_ s . body)
     (dynamic-wind
       (lambda () (test-begin s))
       (lambda () . body)
       (lambda () (test-end s))))))

(define-syntax describe
  (syntax-rules ()
    ((_ s . body)
     (test-group s . body))))

(define (with-nginx fn)
  (let-values (([to-stdin from-stdout from-stderr process-id] (open-process-ports "nginx")))
    (fn)))


(describe "http requests"
 (it "should make a simple http request"
     (let ([url (uv/string->url "http://localhost:8080")])
       (call/cc (lambda (k)
                  (uv/with-loop
                   (lambda (loop)
                     (let/async ([resp (<- (uv/make-http-request loop url))])
                                (test-equal "response code" 200 (cadar resp))
                                (k)))))))))

(describe
 "url functions"
 (it "should parse a simple url"
     (let ([url (uv/string->url "http://google.ca")])
       (test-equal "google.ca" (uv/url-host url))
       (test-eqv 80 (uv/url-port url))
       (test-equal "/" (uv/url-path url))))
 (it "should parse a complex url"
     (let ([url (uv/string->url "https://google.ca:8080/foo/bar")])
       (test-equal "google.ca" (uv/url-host url))
       (test-equal 'https (uv/url-protocol url))
       (test-eqv 8080 (uv/url-port url))
       (test-equal "/foo/bar" (uv/url-path url)))))

(include "utils.ss")

(describe "string-split"
          (it "should split on a delimiter"
           (let ([splits (string-split "0.0.0.0:4343" #\:)])
             (test-equal '("0.0.0.0" "4343") splits))))

(test-end "chezuv")
