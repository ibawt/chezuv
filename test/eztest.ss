(import (chezscheme)
        (log))

(define-record-type test
  (fields setup teardown function name (mutable assertions) skipped? only? (mutable error)))

(define-record-type assertion
  (fields message expected (mutable actual) (mutable passed?)))

(define-record-type test-runner)

(define test-equal?
  (case-lambda
   ([expected actual]
    (test-equal? "" expected actual))
   ([message expected actual]
    (test-assertions-set! current-test (cons (make-assertion
                                              message
                                              expected
                                              actual
                                              (equal? expected actual))
                                             (test-assertions current-test))))))

(define current-test #f)

(define-syntax define-test
  (syntax-rules (setup teardown)
    ((_ name
        (setup setup-body ...)
        (teardown teardown-body ...)
        body ...)
     (set! all-tests
           (cons (make-test (lambda () setup-body ...) (lambda () teardown-body ...)
                            (lambda ()
                              body ...)
                            name '() #f #f #f)
                 all-tests)))
    ((_ name (setup set-body ...)
        body ...)
     (define-test name (setup set-body ...) (teardown #f)
       body ...))

    ((_ name body ...)
     (define-test name (setup #f) (teardown #f) body ...))

    ((_ name (teardown teardown-body ...) body ...)
     (define-test name (setup #f) (teardown teardown-body ...)
       body ...))))

(define all-tests '())

(define (p fmt . args)
  (apply format #t fmt args)
  (newline))

(define (test-passes t)
  (fold-left (lambda (acc a)
               (+ acc (if (assertion-passed? a) 1 0)))
             0 (test-assertions t)))

(define (test-fails t)
  (fold-left (lambda (acc a)
               (+ acc (if (assertion-passed? a) 0 1)))
             0 (test-assertions t)))

(define (run-handler a)
  (when a
    (a)))

(define (ansi-color c s)
  (format #f "~a[~am~a~a[0m" (integer->char 27) c s (integer->char 27)))

(define (green s)
  (ansi-color 32 s))

(define (red s)
  (ansi-color 31 s))

(define (run-tests)
  (p "Running ~a tests" (length all-tests))
  (for-each
   (lambda (t)
     (fluid-let ((current-test t))
       (run-handler (test-setup t))
       (guard (e [else (test-error-set! t e)])
              ((test-function t))
              (let lp ((a (test-assertions t))
                       (i 0))
                (when (pair? a)
                  (p "~a:~a Expected: ~a, Actual: ~a ~a"  i (assertion-message (car a))
                     (assertion-expected (car a))
                     (assertion-actual (car a))
                     (if (assertion-passed? (car a)) (green "PASS") "FAIL"))
                  (lp (cdr a) (+ i 1))))
              (test-assertions t))
              (p "Test ~a: Passes: ~a, Fails: ~a" (test-name t) (test-passes t) (test-fails t)))
       (run-handler (test-teardown t)))
   all-tests))

(define-test "simple test"
  (test-equal? 5 (+ 2 3)))
(run-tests)
