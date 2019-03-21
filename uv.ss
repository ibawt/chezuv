;; -*- geiser-scheme-implementation: chez -*-
(library (uv)
  (export uv/call-with
          uv/call
          uv/url-port
          uv/close
          uv/stop
          uv/make-reader
          uv/close-stream
          uv/tcp-listen
          uv/stream-write
          uv/tcp-connect
          uv/make-idle
          uv/serve-http
          uv/call-after
          <- ;; keyword for let/async
          let/async
          uv/serve-https
          uv/make-http-request
          uv/make-https-request
          uv/call
          uv/getaddrinfo)
  (import (chezscheme)
          (inet)
          (hpack)
          (utils)
          (log)
          (libuv)
          (alloc)
          (irregex))

  (define-record-type uv-context
    (fields loop (mutable callbacks)))

  (define-record-type uv-stream
    (fields context stream reader writer))

  (define (uv-context-push-callback! uv f)
    (uv-context-callbacks-set! uv (cons f (uv-context-callbacks uv))))

  (define (uv-context-pump-callbacks! uv)
    (for-each (lambda (x) (x)) (uv-context-callbacks uv))
    (uv-context-callbacks-set! uv '()))

  (define-condition-type &uv/error &condition make-uv-error uv/error?
    (code uv-error-code)
    (message uv-error-message))

  (define (nothing . args)
    #f)

  (define-syntax check
    (syntax-rules ()
      ((_ e)
       (let ([err e])
         (unless (= 0 err)
           (let ([errstr (if (uv-error? err)
                             (uv-err-name err)
                             (strerror (abs err)))])
             (raise (make-uv-error err errstr))))))))

  (define buf-pool '())
  (define buf-size 65535)

  (define (get-buf)
    (if (null? buf-pool)
        (foreign-alloc buf-size)
        (let ([x (car buf-pool)])
          (set! buf-pool (cdr buf-pool))
          x)))

  (define (free-buf buf)
    (set! buf-pool (cons buf buf-pool)))

  (define strerror
    (lambda (x)
      (let* ([buf (make-bytevector 2048)]
             [n (strerror_r x buf 2048)])
        (if n
            n
            (error 'not-found "errno not found" x)))))

  (define (handle-type-name handle)
    (uv-handle-type-name (uv-handle-get-type handle)))

  (define trim-newline!
    (lambda (bv num-read)
      (truncate-bytevector! bv
                            (if (= 13 (bytevector-u8-ref bv (- num-read 2)))
                                (- num-read 2) (- num-read 1)))))

  (define (register-signal-handlers uv)
    (letrec ([sig-handle (make-handler UV_SIGNAL)]
             [code (foreign-callable
                    (lambda (sig-handle signum)
                      (when (= signum SIGINT)
                        (close-all-handles uv-loop)
                        (uv-stop (uv-loop uv))
                        (unlock-object code)))
                    (void* int)
                    void)])
      (lock-object code)
      (check (uv-signal-init (uv-loop uv) sig-handle))
      (check (uv-signal-start-one-shot sig-handle
                                       (foreign-callable-entry-point code)
                                       SIGINT))))
  (define alloc-buffer
    (foreign-callable-entry-point
     (let ([alloc (foreign-callable
                   (lambda (handle suggested-size b)
                     (let ([x (get-buf)])
                       (ftype-set! uv-buf (base) b (make-ftype-pointer unsigned-8 x))
                       (ftype-set! uv-buf (len) b suggested-size)))
                   (void* size_t (* uv-buf))
                   void)])
       (lock-object alloc)
       alloc)))

  (define (make-req t)
    (alloc-zero (uv-req-size t)))

  (define (make-handler t)
    (alloc-zero (uv-handle-size t)))

  (define (string->buf s)
    (let* ([buf (make-ftype-pointer uv-buf (foreign-alloc (ftype-sizeof uv-buf)))]
           [bytes (string->utf8 s)]
           [b (get-buf)])
      (bytevector-for-each
       (lambda (x i)
         (foreign-set! 'unsigned-8 b i x))
       bytes)
      (ftype-set! uv-buf (base) buf (make-ftype-pointer unsigned-8 b))
      (ftype-set! uv-buf (len) buf (bytevector-length bytes))
      buf))


  (define uv/close-handle handle-close)

  (define (alloc-uv-buf nb)
    (let ([bytes (make-ftype-pointer unsigned-8 (foreign-alloc nb))]
          [buf (make-ftype-pointer uv-buf (foreign-alloc (ftype-sizeof uv-buf)))])
      (ftype-set! uv-buf (base) buf bytes)
      (ftype-set! uv-buf (len) buf nb)
      buf))

 (define (free-uv-buf buf)
   (foreign-free (ftype-pointer-address (ftype-ref uv-buf (base) buf)))
   (foreign-free (ftype-pointer-address buf)))

 (define (uv/read-lines reader on-line)
   (reader (make-bytevector 4096) 0 'eol
           (lambda (bv num-read)
             (if (and bv (on-line (trim-newline! bv num-read)))
                 (begin
                   (uv/read-lines reader on-line))
                 (begin
                   (on-line #f))))))


  (define (uv/close uv)
    (uv-stop (uv-loop uv))
    (close-all-handles (uv-loop uv)))

  (define (uv/stop uv)
    (uv-stop (uv-loop uv)))

  (define (uv/call-with-context f)
    (define ctx (make-uv-context (uvloop-create) '()))
    (register-signal-handlers ctx)
    (f uv)
    (let lp ((r (uv-run (uv-context ctx) 1))) ;; run event loop for one iteration
      (info "r = ~a" r)
      (uv-context-pump-callbacks! ctx) ;; we call the callbacks here to avoid stale foreign contexts
      (when (positive? r)
        (lp (uv-run (uv-context-loop ctx) 1))))
    (uvloop-close (uv-context-loop loop))
    (uvloop-destroy (uv-context-loop loop)))

  (define (uv/getaddrinfo ctx name)
    (lambda (k)
      (letrec ([hint (make-ftype-pointer addrinfo (alloc-zero (ftype-sizeof addrinfo)))]
               [req (make-req UV_GETADDRINFO)]
               [ex (current-exception-state)]
               [code (foreign-callable
                      (lambda (req status addr)
                        (foreign-free req)
                        (foreign-free (ftype-pointer-address hint))
                        (unlock-object code)
                        (uv-push-callback! uv
                                           (lambda ()
                                            (current-exception-state ex)
                                            (if (= 0 status)
                                                (k addr)
                                                (error 'getaddrinfo "invalid status" status))
                                            (uv-freeaddrinfo addr))))
                      (void* int (* addrinfo))
                      void)])
        (lock-object code)
        (ftype-set! addrinfo (ai_family) hint AF_INET)
        (ftype-set! addrinfo (ai_socktype) hint SOCK_STREAM)
        (check (uv-getaddrinfo (uv-loop uv) req (foreign-callable-entry-point code)
                               name #f hint)))))

  (define (uv/tcp-connect uv addr)
    (lambda (k)
      (letrec* ([conn (make-handler UV_STREAM)]
                [ex (current-exception-state)]
                [socket (make-handler UV_TCP)]
                [code (foreign-callable
                       (lambda (conn status)
                         (info "uv/tcp-connect: status: ~a" status)
                         (unlock-object code)
                         (uv-push-callback! uv
                                            (lambda ()
                                             (current-exception-state ex)
                                             (if (= 0 status)
                                                 (k socket)
                                                 (error 'tcp-connect "error in connect" status (uv-err-name status))))))
                       (void* int)
                       void)])
        (lock-object code)
        (info "connecting")
        (check (uv-tcp-init (uv-loop uv) socket))
        (info "tcp-init")
        (check (uv-tcp-connect conn socket addr
                               (foreign-callable-entry-point code))))))


  (define (uv/make-idle uv)
    (lambda (k)
      (letrec* ([idle (make-handler UV_IDLE)]
                [ex (current-exception-state)]
                [code (foreign-callable (lambda (ll)
                                          (uv-push-callback! uv
                                           (lambda ()
                                             (current-exception-state ex)
                                             (if (k)
                                                 #t
                                                 (begin
                                                   (unlock-object code)
                                                   (uv-idle-stop idle)
                                                   (foreign-free idle))))))
                                        (void*)
                                        void)])
        (check (uv-idle-init (uv-loop uv) idle))
        (lock-object code)
        (check (uv-idle-start idle (foreign-callable-entry-point code))))))

  (define (uv/stream-write ctx stream s)
     (lambda (k)
       (letrec ([buf (if (string? s) (string->buf s) s)]
                [write-req (make-req UV_WRITE)]
                [ex (current-exception-state)]
                [code (foreign-callable
                       (lambda (req status)
                         (free-buf (ftype-pointer-address (ftype-ref uv-buf (base) buf)))
                         (foreign-free write-req)
                         (unlock-object code)
                         (add-to-callbacks (lambda ()
                                             (current-exception-state ex)
                                             (if (= 0 status)
                                                 (k status)
                                                 (error 'stream-write "uv/stream-write" status (uv-err-name status))))))
                       (void* int)
                       void)])
         (lock-object code)
         (check (uv-write write-req stream buf 1 (foreign-callable-entry-point code))))))

  (define (uv/stream-read-raw ctx stream cb)
    (letrec ([ex (current-exception-state)]
             [code (foreign-callable
                    (lambda (s nb buf)
                      (unlock-object code)
                      (check (uv-read-stop stream))
                      (add-to-callbacks (lambda ()
                                          (current-exception-state ex)
                                          (if (negative? nb)
                                              (begin
                                                (free-buf (ftype-pointer-address (ftype-ref uv-buf (base) buf)))
                                                (cb #f #f))
                                              (cb nb buf)))))
                    (void* ssize_t (* uv-buf))
                    void)])
      (lock-object code)
      (check (uv-read-start stream alloc-buffer (foreign-callable-entry-point code)))))

  (define (uv/stream-read ctx stream cb)
    (uv/stream-read-raw stream (lambda (nb buf)
                                 (if (and nb buf)
                                     (let ([p (make-bytevector nb)])
                                       (memcpy p (ftype-pointer-address (ftype-ref uv-buf (base) buf)) nb)
                                       (free-buf (ftype-pointer-address (ftype-ref uv-buf (base) buf)))
                                       (cb stream p))
                                     (cb #f #f)))))

  (define (uv/make-reader stream reader)
    (define buf #f)
    (define read-pos 0)
    (define buf-len 0)
    (define (send-buf bv start len on-read)
      (if (eq? 'eol len)
          (let ([newline-pos (find-in-bytevector 10 buf read-pos)])
            (if newline-pos
                (send-buf bv start (- (inc newline-pos) read-pos) on-read)
                (on-read #f #f)))
          (let ([len (if (<= buf-len (+ read-pos len))
                         (- buf-len read-pos)
                         len)])
            (info "bv len: ~a, buf-len: ~a, read-pos: ~a" (bytevector-length bv) buf-len read-pos)
            (bytevector-copy! buf read-pos bv start len)
            (set! read-pos (+ read-pos len))
            (when (>= read-pos buf-len)
              (set! buf #f))

            (on-read bv len))))
    (lambda (bv start len on-read)
      (if buf
          (begin
            (send-buf bv start len on-read))
          (begin
            (reader (lambda (s b)
                     (if s
                         (begin
                           (set! buf b)
                           (set! read-pos 0)
                           (set! buf-len (bytevector-length b))
                           (send-buf bv start len on-read))
                         (on-read #f #f))))))))


  (define (uv/read-fully reader n on-done)
    (reader (make-bytevector n) 0 n (lambda (bv len)
                                      (on-done bv))))

  (define (header-value headers key)
    (let ([v (assoc key headers)])
      (if v
          (cadr v)
          #f)))

  (define (header->number headers key)
    (let ([v (header-value headers key)])
      (if v (string->number v) v)))

  (define (uv/read-http-response reader)
    (lambda (k)
      (read-headers reader
                    (lambda (headers)
                      (if (pair? headers)
                          (let* ([status (parse-status (car headers))]
                                 [headers (parse-headers (cdr headers))]
                                 [content-length (header->number headers "Content-Length")])
                            (if content-length
                                (if (= 0 content-length)
                                    (k (list status headers #f))
                                    (begin
                                      (uv/read-fully reader content-length
                                                     (lambda (body)
                                                       (k (list status headers body))))))
                                (k (list status headers #t))))
                          (error 'eof "read to end of line"))))))



  (define (uv/close-stream ctx stream)
    (letrec ([shutdown-req (make-req UV_SHUTDOWN)]
             [code (foreign-callable (lambda (req status)
                                       (unlock-object code)
                                       (foreign-free shutdown-req)
                                       (handle-close stream nothing))
                                     (void* int)
                                     void)])
      (lock-object code)
      (check (uv-read-stop stream))
      (check (uv-shutdown shutdown-req stream (foreign-callable-entry-point code)))))

  (define (addr->sockaddr addr port)
    (info "addr->sockaddr")
    (let ([s (ftype-ref addrinfo (ai_addr) addr)])
      (sockaddr-set-port s port)
      s))

  (define (uv/ipv4->sockaddr addr)
    (let* ([split (string-split addr #\:)]
           [ip (if (pair? split) (car split) addr)]
           [port (if (pair? split) (string->number (cadr split)) 0)]
           [s (make-ftype-pointer sockaddr_in (foreign-alloc (ftype-sizeof sockaddr_in)))]
           [r (uv-ip4-addr ip port s)])
      (if (= 0 r)
          s
          (begin
            (foreign-free (ftype-pointer-address s))
            (error 'ipv4->sockaddr "error in making ipv4" r)))))

  (define (uv/tcp-listen ctx addr)
    (lambda (k)
      (letrec ([s-addr (uv/ipv4->sockaddr addr)]
              [h (make-handler UV_TCP)]
              [ex (current-exception-state)]
              [code (foreign-callable
                     (lambda (server status)
                       (info "tcp-listen callback")
                       (uv-push-callback! uv (lambda ()
                                           (current-exception-state ex)
                                           (if (= 0 status)
                                               (let ([client (make-handler UV_TCP)])
                                                 (check (uv-tcp-init loop client))
                                                 (check (uv-accept server client))
                                                 (k #f server client))
                                               (k status server #f)))))
                     (void* int)
                     void)])
       (lock-object code)
       (check (uv-tcp-init (uv-loop uv) h))
       (check (uv-tcp-bind h (ftype-pointer-address s-addr) 0))
       (check (uv-listen h 511 (foreign-callable-entry-point code)))
       (foreign-free (ftype-pointer-address s-addr))
       h)))

  (define (bytevector->uv-buf bv)
    (let ([buf (make-ftype-pointer uv-buf (foreign-alloc (ftype-sizeof uv-buf)))]
          [b (get-buf)])
      (memcpy2 b bv (bytevector-length bv))
      (ftype-set! uv-buf (base) buf (make-ftype-pointer unsigned-8 b))
      (ftype-set! uv-buf (len) buf (bytevector-length bv))
      buf))

  (define (uv/serve-http stream)
    (lambda (on-done)
      (serve-http (uv/make-reader stream (lambda (b) (uv/stream-read stream b)))
                  (lambda (s) (uv/stream-write stream s))
                  on-done)))


  (define (uv/call loop f)
    (letrec ([a (make-handler UV_ASYNC)]
             [ex (current-exception-state)]
             [code (foreign-callable
                    (lambda (h)
                      (unlock-object code)
                      (handle-close a nothing)
                      (add-to-callbacks (lambda ()
                                          (current-exception-state ex)
                                          (f))))
                    (void*)
                    void)])
      (lock-object code)
      (check (uv-async-init loop a (foreign-callable-entry-point code)))
      (check (uv-async-send a))))

  (define (uv/call-after loop timeout repeat cb)
    (letrec ([a (make-handler UV_TIMER)]
             [ex (current-exception-state)]
             [code (foreign-callable
                    (lambda (h)
                      (add-to-callbacks (lambda ()
                                          (current-exception-state ex)
                                          (unless (cb)
                                            (unlock-object code)
                                            (handle-close a nothing)))))
                    (void*)
                    void)])
      (lock-object code)
      (check (uv-timer-init loop a))
      (check (uv-timer-start a (foreign-callable-entry-point code) timeout repeat))))
  )
