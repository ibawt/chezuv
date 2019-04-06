(library (bufferpool)
  (export
   buf-size
   get-buf
   free-buf)
  (import (chezscheme)
          (utils))

  (define buf-pool '())
  (define buf-size (* 16 1024))

  (define (get-buf)
    (if (null? buf-pool)
        (foreign-alloc buf-size)
        (let ([x (car buf-pool)])
          (set! buf-pool (cdr buf-pool))
          x)))

  (define (free-buf buf)
    (set! buf-pool (cons buf buf-pool))))
