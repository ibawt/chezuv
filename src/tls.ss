(library (tls)
  (export
   tls-stream
   tls-stream-reader
   tls-stream-writer
   tls-stream-ssl
   close-tls-stream
   make-tls-context
   tls-accept
   tls-connect)
  (import (chezscheme)
          (bufferpool)
          (uv)
          (utils)
          (log)
          (openssl)
          (libuv))

  ;; Glue between (uv) and (openssl)
  (define-record-type tls-stream
    (fields (mutable ssl) reader writer))

  (define (close-tls-stream ctx stream socket)
    (lambda (k)
      (let lp ()
        (let ([n (ssl/shutdown (tls-stream-ssl stream))])
         (cond
          ((= n 0) (k 1))
          ((= n 1)
           (begin
             (ssl/free-stream (tls-stream-ssl stream))
             (tls-stream-ssl-set! stream #f)
             (k n)))
           (else
            (let ([e (ssl/get-error (tls-stream-ssl stream) n)])
              (cond
               ((= e ssl-error-none) (k n))
               ((= e ssl-error-want-read) (begin
                                            (flush-ssl "close-tls-stream" ctx (tls-stream-ssl stream) socket lp)))
               (else (raise (ssl/library-error)))))))))))

  (define (ssl-output-buffer stream)
    (let* ([buf (make-ftype-pointer uv-buf (foreign-alloc (ftype-sizeof uv-buf)))]
           [base (get-buf)]
           [n (ssl/drain-output-buffer stream base buf-size)])
      (ftype-set! uv-buf (base) buf (make-ftype-pointer unsigned-8 base))
      (ftype-set! uv-buf (len) buf n)
      buf))

  (define (ssl-type s)
    (if (ssl/client? s) "CLIENT" "SERVER"))

  (define (check-ssl msg ctx client stream f)
    (lambda (k)
      (let lp ([n (f)])
        (let ([e (ssl/get-error client n)])
          (cond
           ((= e ssl-error-none) (k n))
           ((= e ssl-error-zero-return) (k n))
           ((= e ssl-error-want-read)
            (flush-ssl msg ctx client stream (lambda () (lp (f)))))
           (else (raise (ssl/library-error))))))))

  (define (ssl-drain msg ctx client stream)
    (lambda (k)
      (if (positive? (ssl/num-bytes client))
          (let ([buf (ssl-output-buffer client)])
            (info "ssl-outputbuffer has: ~a" (ftype-ref uv-buf (len) buf))
            ((uv/stream-write ctx stream buf)
             (lambda (n)
               (foreign-free (ftype-pointer-address buf)) ;; stream-write will release the base pointer
               (k))))
          (k))))

  (define (ssl-fill msg ctx client stream k)
    (info "[~a] ~a ssl-fill: want_read: ~a" (ssl-type client) msg (ssl/want client))
    (if (= 1 (ssl/want client))
        (k)
        (let/async ([(nb buf) (<- (uv/stream-read ctx stream))])
                   (info "nb: ~a" nb)
                   (cond
                    [(not nb) (error 'check-ssl "failed to read bytes" nb msg (ssl-type client))]
                    [(= UV_EOF nb) (k)]
                    [(positive? nb)
                     (let ([n (ssl/fill-input-buffer client (ftype-pointer-address (ftype-ref uv-buf (base) buf)) nb)])
                         (free-buf (ftype-pointer-address (ftype-ref uv-buf (base) buf)))
                         (if (= n nb)
                             (k)
                             (error 'check-ssl "probably need to loop but will be annoying " n)))]))))

  (define (flush-ssl msg ctx client stream k)
    ((ssl-drain msg ctx client stream)
     (lambda () (ssl-fill msg ctx client stream k))))

  (define make-tls-context
    (case-lambda
     ([cert key client?]
      (ssl/make-context cert key client?))
     ([]
      (make-tls-context #f #f #t))))


  (define (make-tls-writer ctx client stream)
    (lambda (sbuf)
      (lambda (k)
        (let ([buf (if (string? sbuf) (string->uv-buf sbuf) sbuf)])
          ((check-ssl "tls-write" ctx client stream (lambda ()
                                                      (ssl/write client
                                                                 (ftype-pointer-address (ftype-ref uv-buf (base) buf))
                                                                 (ftype-ref uv-buf (len) buf))))
           (lambda (n)
             (unless (= n (ftype-ref uv-buf (len) buf))
               (error 'tls-writer "error in writing" n (ftype-ref uv-buf (len) buf)))
             ((ssl-drain "tls-write" ctx client stream) (lambda () (k n)))))))))

  (define (make-tls-reader ctx client stream)
    (uv/make-reader stream
        (lambda (on-read)
          (let ([buf (get-buf)])
            ((check-ssl "tls-read" ctx client stream
                        (lambda () (ssl/read client buf buf-size)))
             (lambda (n)
               (unless (= 0 n)
                   (let ([bv (make-bytevector n)])
                     (memcpy bv buf n)
                     (free-buf buf)
                     (on-read stream bv)))))))))

  (define (tls-connect uv-ctx ssl-ctx stream)
    (let ([client (ssl/make-stream ssl-ctx #t)])
      (lambda (k)
        (let/async ([n (<- (check-ssl "tls-connect" uv-ctx client stream (lambda () (ssl/connect client))))])
          (unless (= 1 n) (error 'tls-connect "error in ssl_connect" n))
          (k (make-tls-stream client
                  (make-tls-reader uv-ctx client stream)
                  (make-tls-writer uv-ctx client stream)))))))

  (define (tls-accept uv-ctx ssl-ctx stream)
    (let ([client (ssl/make-stream ssl-ctx #f)])
      (lambda (k)
        (let/async ([n (<- (check-ssl "ssl-accept" uv-ctx client stream (lambda () (ssl/accept client))))] )
          (unless (= 1 n) (error 'tls-accept "error in ssl-accept" n))
          (let/async ([n (<- (check-ssl "do-handshake" uv-ctx client stream (lambda () (ssl/do-handshake client))))])
              (unless (= 1 n) (error 'tls-accept "error in ssl_do_handhshake" n))
              (k (make-tls-stream client
                                  (make-tls-reader uv-ctx client stream)
                                  (make-tls-writer uv-ctx client stream))))))))
)
