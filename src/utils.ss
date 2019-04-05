(library (utils)
  (export
   macro
   let/async
   <-
   bytevector-for-each
   inc
   dec
   memcpy
   memcpy2
   print-stack-trace
   memset
   trim-newline!
   find-char-by
   find-not-char
   find-in-bytevector
   slice-bytevector
   truncate-bytevector!
   string-split
   from-c-string
   alloc-zero
   find-char
   )

  (import (chezscheme)
          (log))

  (define init
    (case (machine-type)
      ((ta6le a6le)
       (begin
         (load-shared-object "libc.so.6")))))

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

  (define <- #f) ;; dummy else it won't compile in the importing module
  (define-syntax let/async
    (syntax-rules (<-)
      ((_ () body ...)
       (let () body ...))

      ((_ (((name ...) (<- value)) next ...) body ...)
       (value (lambda (name ...)
                (let/async (next ...)
                           body ...))))

      ((_ ((name (<- value)) next ...) body ...)
       (value (lambda (name)
                (let/async (next ...)
                           body ...))))


      ((_ ((name value) next ...) body ...)
       (let ((name value))
         (let/async (next ...)
                    body ...)))))

  (define bytevector-for-each
    (lambda (f bv)
      (let ([len (bytevector-length bv)])
        (let loop ([i 0])
          (when (< i len)
            (f (bytevector-u8-ref bv i) i)
            (loop (+ i 1)))))))

  (define trim-newline!
    (lambda (bv num-read)
      (truncate-bytevector! bv
                            (if (= 13 (bytevector-u8-ref bv (- num-read 2)))
                                (- num-read 2) (- num-read 1)))))
  (define (inc x)
    (+ x 1))

  (define (dec x)
    (- x 1))

  (define alloc-zero
    (lambda (size)
      (let ([p (foreign-alloc size)])
        (memset p 0 size)
        p)))

  (define (print-stack-trace max-depth)
	  (call/cc (lambda (k)
		           ;; (slog tag (format "backtrace of [~a] as following:" obj))
		           (let loop ((cur (inspect/object k)) (i 0))
		             (if (and (> (cur 'depth) 1) (< i max-depth))
		                 (begin
			                 (call-with-values
			    	               (lambda () (cur 'source-path))
				                 (case-lambda
					                ((file line char)  (info "\tat ~a (~a:~a)" ((cur 'code) 'name) file line))
					                ((file line)  (info "\tat ~a (~a:~a)" ((cur 'code) 'name) file line))
					                (else (k))))
			                 (loop (cur 'link) (+ i 1))))))))

  (define memcpy
    ;; u8* easy conversion for bytevector
    (foreign-procedure "memcpy"
                       (u8* void* int)
                       void*))

  (define memcpy2
    ;; Same as above but the other way
    (foreign-procedure "memcpy"
                       (void* u8* int)
                       void*))

  (define memset
    (foreign-procedure "memset"
                       (void* int ssize_t)
                       void))

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
           [(>= i len) #f]
           [(= item (bytevector-u8-ref bv i)) i]
           (else (loop (+ 1 i))))))))

  (define from-c-string
    (lambda (bv)
      (let ([n (find-in-bytevector 0 bv 0)])
        (utf8->string (truncate-bytevector! bv n)))))

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

  (define (find-char str ch pos)
    (let loop ([pos pos])
      (cond
       ((>= pos (string-length str)) #f)
       ((char=? ch (string-ref str pos)) pos)
       (else (loop (+ 1 pos))))))
)
