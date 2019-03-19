;; -*- geiser-scheme-implementation: chez -*-
(import (chezscheme)
        (openssl)
        (log)
        (uv))

(define (run-with-cost)
  (let ([cs (make-cost-center)])
    (with-cost-center #t cs
                        run)
    (format #t "instruction count: ~d\n" (cost-center-instruction-count cs))
    (format #t "allocation-count: ~d\n" (cost-center-allocation-count cs))
    (format #t "time: ~a\n" (cost-center-time cs))))

(define display-http-request
  (lambda (status headers body)
    (format #t "Status:\n ~a\n" status)
    (format #t "Headers:\n")
    (for-each (lambda (h)
                (format #t "~a: ~a\n" (car h) (cadr h)))
              headers)
    (newline)
    (format #t "Body:\n")
    (format #t "~a\n" (utf8->string body))))

(define iterations 1)
(define (run)
  (format #t "Running\n")
  (call/cc
   (lambda (done)
     (time
      (uv/call-with-loop
       (lambda (loop)
         (ssl/call-with-context "fixtures/nginx/cert.pem"  "fixtures/nginx/key.pem" #f
                                   (lambda (ctx)
                                     (guard (e [else (info "i caught it wow: ~a" e)])
                                      (uv/tcp-listen loop "127.0.0.1:8181"
                                                     (lambda (status server client)
                                                       (uv/serve-https ctx client
                                                                       (lambda (d)
                                                                         (format #t "d: ~a\n" d)
                                                                         (format #t "closing client: ~a\n" client)
                                                                         (format #t "server: ~a\n" server)
                                                                         (uv/close-stream client)
                                                                         ;; (done)
                                                                         )))))))))))))
(run)
