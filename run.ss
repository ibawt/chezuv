;; -*- geiser-scheme-implementation: chez -*-
(import (chezscheme)
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
         (uv/call-with-ssl-context #f #f #t
                                   (lambda (ctx)
                                     (let ([rx 0]
                                           [url (uv/string->url "https://www.google.ca/")])
                                       (let top ((n 0))
                                         (let/async ([r (<- (uv/make-https-request loop ctx url))])
                                                    (set! rx (+ 1 rx))
                                                    (if (> rx iterations)
                                                        (begin
                                                          (apply display-http-request r)
                                                          (format #t "iterations: ~a, rx: ~a\n" iterations rx)
                                                          (done))
                                                        (top (+ 1 n))))))))))))))
(run)
