(library (tls)
  (export
   tls-stream
   tls-stream-reader
   tls-stream-writer
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
    (fields ssl reader writer))

  (define (close-tls-stream ctx stream socket)
    (lambda (k)
      (let/async ([n (<- (check-ssl ctx (tls-stream-ssl stream) socket
                                    (lambda ()
                                      (ssl/shutdown (tls-stream-ssl stream)))))])
                 (ssl/free-stream (tls-stream-ssl stream))
                 (k n))))

  (define (ssl-output-buffer stream)
    (let* ([buf (make-ftype-pointer uv-buf (foreign-alloc (ftype-sizeof uv-buf)))]
           [base (get-buf)]
           [n (ssl/drain-output-buffer stream base buf-size)])
      (ftype-set! uv-buf (base) buf (make-ftype-pointer unsigned-8 base))
      (ftype-set! uv-buf (len) buf n)
      buf))

  (define (ssl-type s)
    (if (ssl/client? s) "CLIENT" "SERVER"))

  (define (check-ssl ctx client stream f)
    (lambda (k)
      (let lp ([n (f)])
        (info "check-ssl: n = ~a" n)
        (if (ssl/error? n)
            (let ([e (ssl/get-error client n)])
              (info "e = ~a" e)
              ;; TODO: why do I not get any other type of error
              (cond
               ((= e ssl-error-want-read)
                (flush-ssl ctx client stream (lambda () (lp (f)))))
               (else (raise (ssl/library-error)))))
            (k n)))))

  (define (ssl-drain ctx client stream)
    (lambda (k)
      (if (positive? (ssl/num-bytes client))
          (let ([buf (ssl-output-buffer client)])
            ((uv/stream-write ctx stream buf)
             (lambda (n)
               (foreign-free (ftype-pointer-address buf)) ;; stream-write will release the base pointer
               (k))))
          (k))))

  (define (ssl-fill ctx client stream k)
    (let/async ([(nb buf) (<- (uv/stream-read ctx stream))])
      (if nb
          (let ([n (ssl/fill-input-buffer client (ftype-pointer-address (ftype-ref uv-buf (base) buf)) nb)])
            (free-buf (ftype-pointer-address (ftype-ref uv-buf (base) buf)))
            (if (= n nb)
                (k)
                (begin
                  (info "ERROR")
                  (error 'check-ssl "probably need to loop but will be annoying " n)))) ;; TODO: fix this I think at 16k
          (begin
            (info "ERRORz")
            (error 'check-ssl "failed to read bytes" nb)))))

  (define (make-tls-context cert key client?)
    (ssl/make-context cert key client?))

  (define (flush-ssl ctx client stream k)
    ((ssl-drain ctx client stream)
     (lambda () (ssl-fill ctx client stream k))))

  (define (tls-shutdown ctx tls stream)
    (lambda (k)
      ((check-ssl ctx tls stream (lambda () (ssl/shutdown tls))) k)))

  (define (make-tls-writer ctx client stream)
    (lambda (sbuf)
      (lambda (k)
        (let ([buf (if (string? sbuf) (string->uv-buf sbuf) sbuf)])
          ((check-ssl ctx client stream (lambda ()
                                          (ssl/write client
                                                     (ftype-pointer-address (ftype-ref uv-buf (base) buf))
                                                     (ftype-ref uv-buf (len) buf)))) k)))))

  (define (make-tls-reader ctx client stream)
    (uv/make-reader stream
        (lambda (on-read)
          (let ([buf (get-buf)])
            ((check-ssl ctx client stream
                        (lambda ()
                          (ssl/read client buf buf-size)))
              (lambda (n)
                (let ([bv (make-bytevector n)])
                  (memcpy bv buf n)
                  (free-buf buf)
                  (on-read stream bv))))))))

  (define (tls-connect uv-ctx ssl-ctx stream)
    (let ([client (ssl/make-stream ssl-ctx #t)])
      (lambda (k)
        (let/async ([n (<- (check-ssl uv-ctx client stream (lambda () (ssl/connect client))))])
                   (k (make-tls-stream client
                            (make-tls-reader uv-ctx client stream)
                            (make-tls-writer uv-ctx client stream)))))))

  (define (tls-accept uv-ctx ssl-ctx stream)
    (let ([client (ssl/make-stream ssl-ctx #f)])
      (lambda (k)
        (let/async ([n (<- (check-ssl uv-ctx client stream (lambda () (ssl/accept client))))]
                    [n (<- (check-ssl uv-ctx client stream (lambda () (ssl/do-handshake client))))])
                   (k (make-tls-stream client (make-tls-reader uv-ctx client stream) (make-tls-writer uv-ctx client stream))))))))
