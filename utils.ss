(define-syntax macro
  (syntax-rules ()
    ((k (name . args) body ...)
     (macro name (lambda args body ...)))
    ((k name transformer)
     (define-syntax name
       (lambda (stx)
         (syntax-case stx ()
           ((l . sv)
            (let* ((v (syntax->datum (syntax sv)))
                   (e (apply transformer v)))
              (if (eq? (void) e)
                  (syntax (void))
                  (datum->syntax (syntax l) e))))))))))

(define bytevector-for-each
  (lambda (f bv)
    (let ([len (bytevector-length bv)])
      (let loop ([i 0])
        (when (< i len)
          (f (bytevector-u8-ref bv i) i)
          (loop (+ i 1)))))))

(define (inc x)
  (+ x 1))

(define (dec x)
  (- x 1))

(define memset
  (foreign-procedure "memset"
                     (void* int size_t)
                     void))

(define memcpy
  (foreign-procedure "memcpy"
                     (u8* void* int)
                     void*))

(define (logz fmt . args)
  (apply format #t fmt args)
  (newline))

(define (find-char-by str f pos)
  (let loop ([pos pos])
    (cond
     ((>= pos (string-length str)) #f)
     ((f (string-ref str pos)) pos)
     (else (loop (+ 1 pos))))))

(define (find-not-char str ch pos)
  (let loop ([pos pos])
    (cond
     ((>= pos (string-length str)) #f)
     ((not (char=? ch (string-ref str pos))) pos)
     (else (loop (+ 1 pos))))))

(define find-in-bytevector
  (lambda (item bv start)
    (let ([len (bytevector-length bv)])
      (let loop ([i start])
        (cond
         [(= item (bytevector-u8-ref bv i)) i]
         [(>= i len) #f]
         (else (loop (+ 1 i))))))))

(define slice-bytevector
  (lambda (bv start end)
    (if (= 0 start end)
        (make-bytevector 0)
        (let* ([start (if (> start 0) start 0)]
               [end (if (> end 0) end (bytevector-length bv))]
               [nb (make-bytevector (- end start))])
          (bytevector-copy! bv start nb 0 (- end start))
          nb))))

(define truncate-bytevector!
  (lambda (bv len)
    (if (= 0 len)
        (make-bytevector 0)
        (bytevector-truncate! bv len))))

(define (string-split s delim)
  (let loop ([pos 0]
             [start 0]
             [words '()])
    (cond
     ((>= pos (string-length s))
      (reverse (cons (substring s start (string-length s)) words)))
     ((not (char=? delim (string-ref s pos)))
      (loop (+ 1 pos)
            start
            words))
     (else
      (loop (+ 1 pos)
            (+ 1 pos)
            (cons (substring s start pos) words))))))

    ;; (if (>= pos (string-length s))
    ;;     (reverse (cons (substring s start (string-length s)) words))
    ;;     (if (not (char=? delim (string-ref s pos)))
    ;;         (loop (+ 1 pos)
    ;;               start
    ;;               words)
    ;;         (loop (+ 1 pos)
    ;;               (+ 1 pos)
    ;;               (cons (substring s start pos) words))))

(define (find-char str ch pos)
  (let loop ([pos pos])
    (cond
     ((>= pos (string-length str)) #f)
     ((char=? ch (string-ref str pos)) pos)
     (else (loop (+ 1 pos))))))
