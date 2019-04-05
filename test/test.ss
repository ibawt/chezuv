;; -*- geiser-scheme-implementation: chez -*-
(import (chezscheme)
        (srfi :64 testing)
        (log)
        (utils)
        (url)
        (tls)
        (ansi)
        (uv)
        (http))

(define (my-simple-runner)
  (let ((runner (test-runner-simple))
        (num-passed 0)
        (num-failed 0))
    (test-runner-on-test-end! runner
                              (lambda (runner)
                                (let ([result
                                       (case (test-result-kind runner)
                                         ((pass xpass) (begin
                                                         (set! num-passed (+ num-passed 1))
                                                         (color green "PASS")))
                                         ((fail xfail)
                                          (begin
                                            (set! num-failed (+ num-failed 1))
                                            (color red "FAIL")))
                                         (else (error 'unknown-result-kind (test-result-kind runner))))])
                                  (format #t "[~a] ~a Expected: ~a, Actual: ~a == ~a\n" result
                                          (test-runner-test-name runner)
                                          (test-result-ref runner 'expected-value)
                                          (test-result-ref runner 'actual-value)
                                          (test-result-ref runner 'source-form)))))
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
    ((_ s body ...)
     (begin
       (test-group s body ...)))))

(define-syntax describe
  (syntax-rules ()
    ((_ s body ...)
     (test-group s body ...))))

(define-syntax with-nginx
  (syntax-rules ()
    ((_ . body)
     (dynamic-wind
         (lambda ()
           (system "nginx -c nginx.conf -p test/fixtures/nginx >/dev/null 2>&1")
           (sleep (make-time 'time-duration 50 0)))
         (lambda () . body)
         (lambda ()
           (system "nginx -c nginx.conf -s quit -p test/fixtures/nginx >/dev/null 2>&1"))))))

(define-syntax define-async-test
  (syntax-rules ()
    ((_ name (ctx done) body ...)
     (describe 'name
               (call/cc
                (lambda (done)
                  (uv/with-context ctx
                                   body ...)))))))

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

(define (waitgroup n on-done)
  (define times 0)
  (lambda _
    (set! times (+ times 1))
    (when (>= times n) (on-done))))

(define-async-test simple-https-server (ctx done)
  (define wg (waitgroup 2 done))
  (let/async ([tls-ctx (make-tls-context "test/fixtures/nginx/cert.pem" "test/fixtures/nginx/key.pem" #f)]
              [(status server client) (<- (uv/tcp-listen ctx "127.0.0.1:8181"))]
              [status (<- (serve-https ctx tls-ctx client))]
              [_ (<- (uv/close-stream ctx client))])
             (info "do we get here")
             (uv/close-handle server wg))
  (let/async ([tls-ctx (make-tls-context "test/fixtures/nginx/cert.pem" "test/fixtures/nginx/key.pem" #t)]
              [resp (<- (http-do ctx (make-http-request "https://localhost:8181" tls-ctx)))])
             (test-equal "simple-https-server" 200 (http-response-code resp))
             (info "how about here?")
             (wg)))

(define-async-test simple-http-server (ctx done)
  (define wg (waitgroup 2 done))
  (let/async ([(status server client) (<- (uv/tcp-listen ctx "127.0.0.1:8181"))]
              [status (<- (serve-http ctx client))]
              [_ (<- (uv/close-stream ctx client))])
             (uv/close-handle server wg))
  (let/async ([resp (<- (http-do ctx (make-http-request "http://localhost:8181")))])
             (test-equal "http-server" 200 (http-response-code resp))
             (wg)))

(with-nginx
 (define-async-test client-http-requests (ctx done)
   (let ([wg (waitgroup 3 done)])
     (it "should make a simple http request"
         (let/async ([resp (<- (http-do ctx (make-http-request "http://localhost:8080")))])
                    (test-equal "http request" 200 (http-response-code resp))
                    (wg)))
     (it "should make a simple https request (verified)"
         (let/async ([tls-ctx (make-tls-context "test/fixtures/nginx/cert.pem" #f #t)]
                     [resp (<- (http-do ctx (make-http-request "https://localhost:9090" tls-ctx)))])
                    (test-equal "https request" 200 (http-response-code resp))
                    (wg)))
     (it "should fail with a non verified cert"
         (let ([tls-ctx (make-tls-context)])
           (test-error "non verified cert" #t ((http-do (make-http-request "https://localhost:9090") (lambda (resp) #f))))
           (wg)))

     )))

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

(define-async-test simple-ping-pong (ctx done)
  (define wg (waitgroup 2 done))
  (let/async ([(status server client) (<- (uv/tcp-listen ctx "127.0.0.1:8181"))]
              [(_ msg) (<- (uv/stream-read->bytevector ctx client))]
              [msg (utf8->string msg)])
             (if (string=? "PING" msg)
                 (let/async ([n (<- (uv/stream-write ctx client "PONG"))]
                             [_ (<- (uv/close-stream ctx client))])
                            (uv/close-handle server (lambda (_) (info "closed")))
                            (wg))
                 (begin
                   (let/async ([_ (<- (uv/close-stream ctx client))])
                              (uv/close-handle server (lambda (_) (info "closed")))
                              (test-assert "tcp ping pong" #f)
                              (wg)))))
  (let/async ([addr (uv/ipv4->sockaddr "127.0.0.1:8181")]
              [socket (<- (uv/tcp-connect ctx addr))]
              [n (<- (uv/stream-write ctx socket "PING"))]
              [(n msg) (<- (uv/stream-read->bytevector ctx socket))]
              [_ (<- (uv/close-stream ctx socket))])
             (info "~a ~a" n (utf8->string msg))
             (test-equal "should get PONG back" "PONG" (utf8->string msg))
             (wg)))

(define-async-test tls-ping-pong (ctx done)
  (define wg (waitgroup 2 (lambda () (info "exiting") (done))))
  (let ([tls-ctx (make-tls-context "test/fixtures/nginx/cert.pem" "test/fixtures/nginx/key.pem" #f)])
    (let/async ([(status server socket) (<- (uv/tcp-listen ctx "127.0.0.1:9191"))]
                [stream (<- (tls-accept ctx tls-ctx socket))]
                [buf (make-bytevector 2048)]
                [(bv n) (<- ((tls-stream-reader stream) buf 0 2048))]
                [msg (utf8->string (truncate-bytevector! bv n))])
               (if (string=? "PING" msg)
                   (let/async ([n (<- ((tls-stream-writer stream) "PONG"))]
                               [_ (<- (close-tls-stream ctx stream socket))]
                               [_ (<- (uv/close-stream ctx socket))])
                              (uv/close-handle server (lambda _ (info "SERVER CLOSED HANDLE")))
                              (test-assert "got the string PING and sent PONG" #t)
                              (wg))
                   (begin
                     (uv/close-stream socket)
                     (uv/close-handle server (lambda (_) (info "closed")))
                     (test-assert "didn't get a ping" #f)
                     (wg)))))

  (let ([tls-ctx (make-tls-context "test/fixtures/nginx/cert.pem" "test/fixtures/nginx/key.pem" #t)])
    (let/async ([socket (<- (uv/tcp-connect ctx (uv/ipv4->sockaddr "127.0.0.1:9191")))]
                [stream (<- (tls-connect ctx tls-ctx socket))]
                [n (<- ((tls-stream-writer stream) "PING"))]
                [buf (make-bytevector 2048)]
                [(bv n) (<- ((tls-stream-reader stream) buf 0 2048))]
                [msg (utf8->string (truncate-bytevector! bv n))]
                [n (<- (close-tls-stream ctx stream socket))]
                [_ (<- (uv/close-stream ctx socket))])
               (test-equal "tls client got PONG" "PONG" msg)
               (wg))))

(test-end "chezuv")

