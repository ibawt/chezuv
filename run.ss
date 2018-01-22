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

(define (run)
  (format #t "Running\n")
  (time
   (uv/with-loop
    (lambda (loop)
      (uv/call-with-ssl-context "cert.pem" "key.pem"
       (lambda (ctx on-done)
         (uv/tcp-listen loop "0.0.0.0:6565"
                        (lambda (err . value)
                          (uv/serve-https ctx (cadr value)
                                          (lambda (err ok)
                                            (uv/close-stream (cadr value))))))))

      (format #t "listening on 0.0.0:6565~n")
      ;; (call/cc
      ;;  (lambda (done)
      ;;    (let ([rx 0]
      ;;          [url (string->url "http://localhost:8080")])
      ;;      (let top ((n 0))
      ;;        (ideal-http-request loop url
      ;;                            (lambda (err ok)
      ;;                              (format #t "err: ~a\n" err)
      ;;                              (format #t "ok: ~a\n" ok)
      ;;                              (set! rx (+ 1 rx))
      ;;                              (if (>= rx 1000)
      ;;                                  (begin
      ;;                                    (format #t "stopping loop at 1000\n")
      ;;                                    (stop loop)
      ;;                                    (done loop)))))
      ;;        (if (< n 1000)
      ;;            (top (+ 1 n)))))))
      ))))
