(define async? procedure?)

(define (make-async task)
  (lambda (c)
    (let ([ok (lambda (a) (c #f a))]
          [fail (lambda (a) (c a #f))])
      (task ok fail))))

(define (async-return value)
  (make-async
   (lambda (ok fail) (ok value))))

(define (async-bind async f)
  (make-async
   (lambda (ok fail)
     (async (lambda (e v)
              (if e
                  (fail e)
                  ((f v) (lambda (ee vv)
                           (if ee
                               (fail ee)
                               (ok vv))))))))))
(define-syntax async-do
  (syntax-rules ()
    [(_ e ...) (monad-do (async-return async-bind async?) e ... )]))

(define-syntax async-do*
  (syntax-rules (<-)
    [(_ e) e]
    [(_ (<- var e1) e2 ...)
     (async-bind e1 (lambda (var) (async-do* e2 ...)))]
    [(_ e1 e2 ...)
     (async-bind e1 (lambda (_) (async-do* e2 ...)))]))

(define-syntax ->async
  (syntax-rules ()
    [(_ e)
     (let ([e-result e])
       (if (async? e-result)
           e-result
           (async-return e-result)))]))

(define-syntax async-do**
  (syntax-rules (<-)
    [(_ e)
     (->async e)]
    [(_ (<- var e1) e2 ...)
     (async-bind (->async e1) (lambda (var) (async-do** e2 ...)))]
    [(_ e1 e2 ...)
     (async-bind (->async e1) (lambda (_) (async-do** e2 ...)))]))

(define-syntax ->monad
  (syntax-rules ()
    [(_ (return monad?) e)
     (let ([e-result e])
       (if (monad? e-result)
           e-result
           (return e-result)))]))

(define-syntax monad-do
  (syntax-rules (<-)
    [(_ (return >>= monad?) e)
     (->monad (return monad?) e)]
    [(_ (return >>= monad?) (<- var e1) e2 ...)
     (>>= (->monad (return monad?) e1) 
          (lambda (var) (monad-do (return >>= monad?) e2 ...)))]
    [(_ (return >>= monad?) e1 e2 ...)
     (>>= (->monad (return monad?) e1) 
          (lambda (_) (monad-do (return >>= monad?) e2 ...)))]))
