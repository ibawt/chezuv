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
    (format #t "[~a] Status: ~a\n" 0 status)
    (for-each (lambda (h)
                (format #t "~a: ~a\n" (car h) (cadr h)))
              headers)
    (newline)
    (format #t "~a\n" (utf8->string body))))

(define iterations 10)
(define (run)
  (format #t "Running\n")
  (time
   (uv/with-loop
    (lambda (loop)
      (uv/call-with-ssl-context #f #f #t
                                (lambda (ctx on-done)
                                  (let ([rx 0]
                                        [url (uv/string->url "https://localhost:9090")])
                                    (let top ((n 0))
                                      (format #t "top\n")
                                      (uv/make-https-request loop ctx url
                                                             (lambda (err ok)
                                                               (format #t "err: ~a\n" err)
                                                               (format #t "ok: ~a\n" ok)
                                                               (format #t "rx: ~a\n" rx)
                                                               (set! rx (+ 1 rx))
                                                               (if (or err (> rx iterations))
                                                                   (begin
                                                                     (format #t "iterations: ~a, rx: ~a\n" iterations rx)
                                                                     (on-done err ok)))))
                                         (if (< n iterations)
                                             (top (+ 1 n)))))))))
      ;; (uv/call-with-ssl-context "cert.pem" "key.pem" #f
      ;;  (lambda (ctx on-done)
      ;;    (uv/tcp-listen loop "127.0.0.1:8443"
      ;;                   (lambda (err . value)
      ;;                     (uv/serve-https ctx (cadr value)
      ;;                                     (lambda (err ok)
      ;;                                       (format #t "in run.ss\n")
      ;;                                       (uv/close-stream (cadr value))))))))
      ;; (uv/tcp-listen loop "127.0.0.1:8080"
      ;;                (lambda (err . value)
      ;;                  (uv/serve-http (cadr value)
      ;;                                 (lambda (err ok)
      ;;                                   (uv/close-stream (cadr value))))))

      )
    )
