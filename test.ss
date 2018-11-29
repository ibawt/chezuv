;; -*- geiser-scheme-implementation: chez -*-
(import (chezscheme)
        (srfi s64 testing)
        (uv))

(define (my-simple-runner)
  (let ((runner (test-runner-null))
        (num-passed 0)
        (num-failed 0))
    (test-runner-on-test-end! runner
                              (lambda (runner)
                                (case (test-result-kind runner)
                                  ((pass xpass) (set! num-passed (+ num-passed 1)))
                                  ((fail xfail) (set! num-failed (+ num-failed 1)))
                                  (else #t))))
    (test-runner-on-final! runner
                           (lambda (runner)
                             (format #t "Passing tests: ~d.~%Failing tests: ~d.~%"
                                     num-passed num-failed)
                             (when (positive? num-failed)
                               (exit 1))))
    runner))

(test-runner-factory
 (lambda () (my-simple-runner)))

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

(define-syntax with-nginx
  (syntax-rules ()
    ((_ . body)
     (dynamic-wind
         (lambda ()
           (system "nginx -c nginx.conf -p fixtures/nginx > /dev/null 2>&1")
           (sleep (make-time 'time-duration 500000 1)))
         (lambda () . body)
         (lambda ()
           (system "nginx -c nginx.conf -s quit -p fixtures/nginx >/dev/null 2>&1"))))))

(define http-test-request
  (lambda (url-string)
    (let ([url (uv/string->url url-string)])
      (call/cc
       (lambda (k)
         (uv/with-loop
          (lambda (loop)
            (let/async ([resp (<- (uv/make-http-request loop url))])
                       (k resp)))))))))

(define https-test-request
  (lambda (url-string)
    (let ([url (uv/string->url url-string)])
      (call/cc
       (lambda (k)
         (uv/call-with-ssl-context #f #f #t
            (lambda (ctx)
              (uv/with-loop
              (lambda (loop)
                (let/async ([resp (<- (uv/make-https-request loop ctx url))])
                            (k resp)))))))))))

(describe "http requests"
  (with-nginx
    (it "should make a simple http request"
        (let ((resp (http-test-request "http://localhost:8080")))
          (test-equal 200 (cadar resp))))
    (it "should make a simple https request"
        (let ([resp (https-test-request "https://localhost:9090")])
          (test-equal 200 (cadar resp))))))

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

