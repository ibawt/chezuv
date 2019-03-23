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
          (openssl)
          (libuv))

  (define-record-type tls-stream
    (fields ssl reader writer))

  (define (close-tls-stream stream)
    (ssl/free-stream (tls-stream-ssl stream)))

  (define (ssl-output-buffer stream)
    (let* ([buf (make-ftype-pointer uv-buf (foreign-alloc (ftype-sizeof uv-buf)))]
           [base (get-buf)]
           [n (ssl/drain-output-buffer stream base buf-size)])
      (ftype-set! uv-buf (base) buf (make-ftype-pointer unsigned-8 base))
      (ftype-set! uv-buf (len) buf n)
      buf))

  (define (ssl-type s)
    (if (ssl/client? s) "CLIENT" "SERVER"))

  (define (check-ssl client stream f)
    (lambda (k)
      (let lp ([n (f)])
        (if (ssl/error? n)
            (let ([e (ssl/get-error client n)])
              ;; TODO: why do I not get any other type of error
              (cond
               ((= e ssl-error-want-read) (flush-ssl client stream (lambda () (lp (f)))))
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
    (uv/stream-read ctx stream
                    (lambda (nb buf)
                      (if nb
                          (let ([n (ssl/fill-input-buffer client (ftype-pointer-address (ftype-ref uv-buf (base) buf)) nb)])
                            (free-buf (ftype-pointer-address (ftype-ref uv-buf (base) buf)))
                            (if (= n nb)
                                (k)
                                (error 'check-ssl "probably need to loop but will be annoying " n))) ;; TODO: fix this I think at 16k
                          (error 'check-ssl "failed to read bytes" nb)))))

  (define (make-tls-context cert key client?)
    (ssl/make-context cert key client?))

  (define (flush-ssl ctx client stream k)
    (ssl-drain client stream (lambda () (ssl-fill client stream k))))

  (define (tls-shutdown ctx tls stream)
    (lambda (k)
      ((check-ssl tls stream (lambda () (ssl/shutdown tls))) k)))

  (define (make-tls-writer ctx client stream)
    (lambda (buf)
      (lambda (k)
        (let ([buf (if (string? buf) (string->uv-buf buf) buf)])
          ((check-ssl client stream (lambda ()
                                      (ssl/write client
                                                 (ftype-pointer-address (ftype-ref uv-buf (base) buf))
                                                 (ftype-ref uv-buf (len) buf))))
           (lambda (n)
             (flush-ssl client stream (lambda () (k n)))))))))

  (define (make-tls-reader client stream)
    (uv/make-reader stream
                    (lambda (on-read)
                      (let ([buf (get-buf)])
                        ((check-ssl client stream
                                    (lambda ()
                                      (ssl/read client buf buf-size)))
                         (lambda (n)
                           (let ([bv (make-bytevector n)])
                             (memcpy bv buf n)
                             (free-buf buf)
                             (on-read stream bv))))))))

  (define (tls-connect ctx stream)
    (let ([client (ssl/make-stream ctx #t)])
      (lambda (k)
        (let/async ([n (<- (check-ssl client stream (lambda () (ssl/connect client))))])
                   (k (list client
                            (make-tls-reader client stream)
                            (make-tls-writer client stream)))))))

  (define (tls-accept uv-ctx ssl-ctx stream)
    (let ([client (ssl/make-stream ssl-ctx #f)])
      (lambda (k)
        (let/async ([n (<- (check-ssl client stream (lambda () (ssl/accept client))))]
                    [n (<- (check-ssl client stream (lambda () (ssl/do-handshake client))))])
                   (k (make-tls-stream client (make-tls-reader client stream) (make-tls-writer client stream))))))))