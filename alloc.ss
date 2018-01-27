(library (alloc)
  (export tracked-alloc
          tracked-free
          tracked-in-use)
  (import (chezscheme))

  (define table (make-eq-hashtable))

  (define (tracked-alloc size id)
    (let ([x (foreign-alloc size)])
      (eq-hashtable-set! table x id)
      x))

  (define (tracked-free x)
    (let ([id (eq-hashtable-ref table x #f)])
      (if id
          (begin
            (foreign-free x)
            (eq-hashtable-delete! table x))
          (error 'invalid-free "I don't have that" id))))

  (define (tracked-in-use)
    (hash-table-for-each table
                         (lambda (key value)
                           (format #t "~a: ~a\n" key value)))))
