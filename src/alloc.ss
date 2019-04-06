(library (alloc)
  (export tracked-alloc
          tracked-free
          alloc-stats
          tracked-in-use)
  (import (chezscheme))

  (define table (make-eq-hashtable))

  (define num-allocs 0)
  (define num-frees 0)

  (define (tracked-alloc size id)
    (let ([x (foreign-alloc size)])
      (eq-hashtable-set! table x id)
      (set! num-allocs (+ 1 num-allocs))
      x))

  (define (tracked-free x)
    (let ([id (eq-hashtable-ref table x #f)])
      (if id
          (begin
            (set! num-frees (+ 1 num-frees))
            (foreign-free x)
            (eq-hashtable-delete! table x))
          (error 'invalid-free "I don't have that" id))))

  (define (alloc-stats)
    `((allocs ,num-allocs) (frees ,num-frees)))

  (define (tracked-in-use)
    (hash-table-for-each table
                         (lambda (key value)
                           (format #t "~a: ~a\n" key value)))))
