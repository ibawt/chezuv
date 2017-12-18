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
    (format #t "[~a] Status: ~a~n" n status)
    (for-each (lambda (h)
                (format #t "~a: ~a\n" (car h) (cadr h)))
              headers)
    (newline)
    (format #t "~a\n" (utf8->string body))))


(define (run)
  (time
   (with-uvloop
    (lambda (loop)
      ;; ((getaddrinfo loop "google.ca")
      ;;  (lambda (ok)
      ;;    (format #t "ok\n"))
      ;;  (lambda (fail)
      ;;    (format #t "fail\n")))
      (format #t "this was: ~a\n" (ideal-http-request loop "google.ca"))
      ;; (lambda (n ... ) n) (lambda (y ...) y))
      ;; (format #t "this was: ~a\n" (ideal-http-request loop "google.ca"))
       ;; (let top ((n 0))
      ;;   (format #t "at top\n")
      ;;   (if (< n 1)

      ;;       (let ([cc (make-http-request loop "http://google.ca")])
      ;;         (format #t "cc: ~a\n" cc)
      ;;         (if (procedure? cc)
      ;;             (begin
      ;;               (format #t "procedure\n"))
      ;;             (begin
      ;;               (format #t "idk: ~a\n" cc)))
      ;;         ;; (when (pair? cc)
      ;;         ;;     (display-http-request (car cc) (caar cc) (caaar cc)))
      ;;         (top (+ 1 n)))
      ;;       (close loop)))
      (format #t "running loop\n")
      (uv-run loop 0)
      (format #t "exiting...~n")))))
