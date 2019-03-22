;; -*- geiser-scheme-implementation: chez -*-
(import (chezscheme)
        (srfi s64 testing)
        (log)
        (utils)
        (url)
        (openssl)
        (uv))

(define (my-simple-runner)
  (let ((runner (test-runner-null))
        (num-passed 0)
        (num-failed 0))
    (test-runner-on-test-end! runner
                              (lambda (runner)
                                (case (test-result-kind runner)
                                  ((pass xpass) (set! num-passed (+ num-passed 1)))
                                  ((fail xfail) (begin
                                                  (format #t "[FAILED] ~{~a~^->~}\n\tExpected: ~a\n\tActual: ~a\n"  (test-runner-group-stack runner)
                                                          (test-result-ref runner 'expected-value)
                                                          (test-result-ref runner 'actual-value)))
                                                  (set! num-failed (+ num-failed 1)))
                                  (else #t))))
    (test-runner-on-final! runner
                           (lambda (runner)
                             (format #t "Passing tests: ~d.~%Failing tests: ~d.~%"
                                     num-passed num-failed)
                             (when (positive? num-failed)
                               (exit 1))))
    runner))

(test-runner-factory my-simple-runner)

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
           (system "nginx -c nginx.conf -p fixtures/nginx")
           (sleep (make-time 'time-duration 10 1)))
         (lambda () . body)
         (lambda ()
           (system "nginx -c nginx.conf -s quit -p fixtures/nginx")
           )))))

;; (define http-test-request
;;   (lambda (url-string)
;;     (let ([url (uv/string->url url-string)])
;;       (call/cc
;;        (lambda (k)
;;          (uv/call-with-loop
;;           (lambda (loop)
;;             (let/async ([resp (<- (uv/make-http-request loop url))])
;;                        (k resp)))))))))

;; (define https-test-request
;;   (lambda (url-string cert)
;;     (let ([url (uv/string->url url-string)])
;;       (call/cc
;;        (lambda (k)
;;          (ssl/call-with-context cert #f #t
;;             (lambda (ctx)
;;               (uv/call-with-loop
;;                (lambda (loop)
;;                  (let/async ([resp (<- (uv/make-https-request loop ctx url))])
;;                             (info "https-test-request: ~a" resp)
;;                             (k resp)))))))))))

(define run-command
  (lambda (cmd)
    (format #t "run-command?")
    (let ([plist (process cmd)])
      (let loop ([lines '()]
                 [line (get-line (car plist))])
        (format #t "line: ~a~%" line)
        (if (eof-object? line)
            (apply string-append (reverse lines))
            (loop (cons line lines) (get-line (car plist))))))))

(define curl-test-request
  (lambda (url)
    (string->number (run-command (format #f "curl --silent --write-out \"%{http_code}\" ~a" url)))))

;; (describe "serving http"
;;   (it "should serve a simple http requests"
;;       (let ([resp (call/cc
;;                    (lambda (k)
;;                      (uv/call-with-loop
;;                       (lambda (loop)
;;                         (let/async ([(status server client) (<- (uv/tcp-listen loop "127.0.0.1:8181"))]
;;                                     [status (<- (uv/serve-http client))])
;;                                    (uv/close-stream client)) 
;;                         (let/async ([resp (<- (uv/make-http-request loop (uv/string->url "http://localhost:8181")))])
;;                                    (k resp))))))])
;;         (test-equal 200 (cadar resp)))))

;; (describe "serving https"
;;   (it "should serve a simple https request"
;;       (let ([resp
;;               (call/cc
;;                (lambda (k)
;;                  (uv/call-with-loop
;;                   (lambda (loop)
;;                     (ssl/call-with-context "./fixtures/nginx/cert.pem" "./fixtures/nginx/key.pem" #f
;;                       (lambda (ctx)
;;                         (let/async ([(status server client) (<- (uv/tcp-listen loop "127.0.0.1:9191"))]
;;                                     [status (<- (uv/serve-https ctx client))])
;;                                    #f)
;;                         (ssl/call-with-context "./fixtures/nginx/cert.pem" #f #t
;;                                                (lambda (ctx)
;;                                                  ((uv/make-https-request loop ctx (uv/string->url "https://localhost:9191")) k)))))))))])
;;         (info "wat")
;;         (test-equal 200 (cadar resp)))))

;; (describe "http2 basic"
;;   (it "should do something related to http2"
;;       (let ([resp (call/cc
;;                     (lambda (k)
;;                       (uv/call-with-loop
;;                       (lambda (loop)
;;                         (ssl/call-with-context "./fixtures/nginx/cert.pem" "./fixtures/nginx/key.pem" #f
;;                           (lambda (ctx)
;;                             (uv/tcp-listen loop "127.0.0.1:9191"
;;                                             (lambda (status server client)
;;                                               (uv/serve-https ctx client (lambda (status)
;;                                                                           #f))))))))
;;                       ))])

;;         (info "resp is: ~a" resp))))

;; (describe "http requests"
;;   (with-nginx
;;     (it "should make a simple http request"
;;         (let ((resp (http-test-request "http://localhost:8080")))
;;           (info "test1 done")
;;           (test-equal 200 (cadar resp))))
;;     (it "should make a simple https request (verified)"
;;         (let ([resp (https-test-request "https://localhost:9090" "./fixtures/nginx/cert.pem")])
;;           (test-equal 200 (cadar resp))))
;;     ;; (it "should fail with a non verified cert"
;;     ;;     (test-error #t (https-test-request "https://localhost:9090" #f))))

;;   ))

(describe
 "url functions"
 (it "should parse a simple url"
     (let ([url (string->url "http://google.ca")])
       (test-equal "google.ca" (url-host url))
       (test-eqv 80 (url-port url))
       (test-equal "/" (url-path url))))
 (it "should parse a complex url"
     (let ([url (string->url "https://google.ca:8080/foo/bar")])
       (test-equal "google.ca" (url-host url))
       (test-equal 'https (url-protocol url))
       (test-eqv 8080 (url-port url))
       (test-equal "/foo/bar" (url-path url))))
 (it "should barf on an invalid url"
     (test-error url-error? (string->url "not an url"))))

(describe "string-split"
  (it "should split on a delimiter"
    (let ([splits (string-split "0.0.0.0:4343" #\:)])
      (test-equal '("0.0.0.0" "4343") splits))))

(test-end "chezuv")

