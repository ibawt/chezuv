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

(define (run)
  (time
   (with-uvloop
    (lambda (loop)
      (let top ((n 0))
        (if (< n 100)
            (make-http-request loop "http://google.ca"
                               (lambda (status headers body)
                                 ;; (format #t "[~a] Status: ~a~n" n status)
                                 ;; (for-each (lambda (h)
                                 ;;             (format #t "~a: ~a\n" (car h) (cadr h)))
                                 ;;           headers)
                                 ;; (newline)
                                 ;; (format #t "~a\n" (utf8->string body))
                                 (top (+ 1 n))))
            (close loop)))
      (uv-run loop 0)
      (format #t "exiting...~n")))))
