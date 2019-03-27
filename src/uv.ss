;; -*- geiser-scheme-implementation: chez -*-
(library (uv)
  (export uv/call-with-context
          uv/with-context
          uv/call
          uv/close
          uv/stop
          uv/make-reader
          uv/close-stream
          uv/close-handle
          uv/tcp-listen
          uv/stream-write
          uv/stream-read
          uv/stream-read->bytevector
          uv/tcp-connect
          uv/make-idle
          uv/call-after
          uv/ipv4->sockaddr
          <- ;; keyword for let/async
          let/async
          uv/call
          uv/getaddrinfo)
  (import (chezscheme)
          (inet)
          (bufferpool)
          (hpack)
          (utils)
          (log)
          (libuv)
          (alloc)
          (irregex))

  (define-syntax uv/with-context
    (syntax-rules ()
      [(_ ctx body ...)
       (uv/call-with-context
        (lambda (ctx)
          body ...))]))

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

  (define strerror
    (lambda (x)
      (let* ([buf (make-bytevector 2048)]
             [n (strerror_r x buf 2048)])
        (if n
            n
            (error 'not-found "errno not found" x)))))

  (define (register-signal-handlers ctx)
    (letrec ([sig-handle (make-handler UV_SIGNAL)]
             [code (foreign-callable
                    (lambda (sig-handle signum)
                      (when (= signum SIGINT)
                        (close-all-handles (uv-context-loop ctx))
                        (uv-stop (uv-context-loop ctx))
                        (unlock-object code)))
                    (void* int)
                    void)])
      (lock-object code)
      (check (uv-signal-init (uv-context-loop ctx) sig-handle))
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

  (define uv/close-handle close-handle)

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
    (uv-stop (uv-context-loop uv))
    (close-all-handles (uv-context-loop uv)))

  (define (uv/stop uv)
    (uv-stop (uv-context-loop uv)))

  (define (uv/call-with-context f)
    (define ctx (make-uv-context (make-uv-loop) '()))
    (register-signal-handlers ctx)
    (f ctx)
    (let lp ((r (uv-run (uv-context-loop ctx) 1))) ;; run event loop for one iteration
      (uv-context-pump-callbacks! ctx) ;; we call the callbacks here to avoid stale foreign contexts
      (when (positive? r)
        (lp (uv-run (uv-context-loop ctx) 1))))
    (uv-loop-close (uv-context-loop ctx))
    (uv-loop-destroy (uv-context-loop ctx)))

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
                        (uv-context-push-callback! ctx
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
        (check (uv-getaddrinfo (uv-context-loop ctx) req (foreign-callable-entry-point code)
                               name #f hint)))))

  (define (uv/tcp-connect uv addr)
    (lambda (k)
      (letrec* ([conn (make-handler UV_STREAM)]
                [ex (current-exception-state)]
                [socket (make-handler UV_TCP)]
                [code (foreign-callable
                       (lambda (conn status)
                         (unlock-object code)
                         (uv-context-push-callback! uv
                                                    (lambda ()
                                                      (current-exception-state ex)
                                                      (if (= 0 status)
                                                          (k socket)
                                                          (error 'tcp-connect "error in connect" status (uv-err-name status))))))
                       (void* int)
                       void)])
        (lock-object code)
        (check (uv-tcp-init (uv-context-loop uv) socket))
        (check (uv-tcp-connect conn socket (make-ftype-pointer sockaddr (ftype-pointer-address addr))
                               (foreign-callable-entry-point code))))))


  (define (uv/make-idle uv)
    (lambda (k)
      (letrec* ([idle (make-handler UV_IDLE)]
                [ex (current-exception-state)]
                [code (foreign-callable (lambda (ll)
                                          (uv-context-push-callback! uv
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
        (check (uv-idle-init (uv-context-loop uv) idle))
        (lock-object code)
        (check (uv-idle-start idle (foreign-callable-entry-point code))))))

  (define (uv/stream-write ctx stream s)
     (lambda (k)
       (letrec ([buf (if (string? s) (string->uv-buf s) s)]
                [write-req (make-req UV_WRITE)]
                [ex (current-exception-state)]
                [code (foreign-callable
                       (lambda (req status)
                         (free-buf (ftype-pointer-address (ftype-ref uv-buf (base) buf)))
                         (foreign-free write-req)
                         (unlock-object code)
                         (uv-context-push-callback! ctx (lambda ()
                                                      (current-exception-state ex)
                                                      (if (= 0 status)
                                                          (k status)
                                                          (error 'stream-write "uv/stream-write" status (uv-err-name status))))))
                       (void* int)
                       void)])
         (lock-object code)
         (check (uv-write write-req stream buf 1 (foreign-callable-entry-point code))))))

  (define (uv/stream-read ctx stream)
    (lambda (k)
     (letrec ([ex (current-exception-state)]
              [code (foreign-callable
                     (lambda (s nb buf)
                       (unlock-object code)
                       (check (uv-read-stop stream))
                       (uv-context-push-callback! ctx (lambda ()
                                                        (current-exception-state ex)
                                                        (if (negative? nb)
                                                            (begin
                                                              (free-buf (ftype-pointer-address (ftype-ref uv-buf (base) buf)))
                                                              (k #f #f))
                                                            (k nb buf)))))
                     (void* ssize_t (* uv-buf))
                     void)])
       (lock-object code)
       (check (uv-read-start stream alloc-buffer (foreign-callable-entry-point code))))))

  (define (uv/stream-read->bytevector ctx stream)
    (lambda (k)
      ((uv/stream-read ctx stream) (lambda (num-bytes buf)
                                     (if (and num-bytes buf)
                                         (let ([p (make-bytevector num-bytes)])
                                           (memcpy p (ftype-pointer-address (ftype-ref uv-buf (base) buf)) num-bytes)
                                           (free-buf (ftype-pointer-address (ftype-ref uv-buf (base) buf)))
                                           (k stream p))
                                         (k #f #f))))))

  (define (uv/make-reader stream reader)
    ;; TODO: why does this need stream >.>
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
            (bytevector-copy! buf read-pos bv start len)
            (set! read-pos (+ read-pos len))
            (when (>= read-pos buf-len)
              (set! buf #f))

            (on-read bv len))))
    (lambda (bv start len)
      (lambda (k)
        (if buf
            (begin
              (send-buf bv start len k))
            (begin
              (reader (lambda (s b)
                        (if s
                            (begin
                              (set! buf b)
                              (set! read-pos 0)
                              (set! buf-len (bytevector-length b))
                              (send-buf bv start len k))
                            (k #f #f)))))))))


  (define (uv/read-fully reader n on-done)
    (reader (make-bytevector n) 0 n (lambda (bv len)
                                      (on-done bv))))

  (define (uv/close-stream stream)
    (letrec ([shutdown-req (make-req UV_SHUTDOWN)]
             [code (foreign-callable (lambda (req status)
                                       (unlock-object code)
                                       (foreign-free shutdown-req)
                                       (close-handle stream nothing))
                                     (void* int)
                                     void)])
      (lock-object code)
      (check (uv-read-stop stream))
      (check (uv-shutdown shutdown-req stream (foreign-callable-entry-point code)))))

  (define (addr->sockaddr addr port)
    (let ([s (ftype-ref addrinfo (ai_addr) addr)])
      (sockaddr-set-port s port)
      s))

  (define (uv/ipv4->sockaddr addr)
    (let* ([split (string-split addr #\:)]
           [ip (if (pair? split) (car split) addr)]
           [port (if (pair? split) (string->number (cadr split)) 0)]
           [s (make-ftype-pointer sockaddr_in (malloc-gc (ftype-sizeof sockaddr_in)))]
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
                       (uv-context-push-callback! ctx (lambda ()
                                           (current-exception-state ex)
                                           (if (= 0 status)
                                               (let ([client (make-handler UV_TCP)])
                                                 (check (uv-tcp-init (uv-context-loop ctx) client))
                                                 (check (uv-accept server client))
                                                 (k status server client))
                                               (k status server #f)))))
                     (void* int)
                     void)])
       (lock-object code)
       (check (uv-tcp-init (uv-context-loop ctx) h))
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

  (define (uv/call ctx f)
    (letrec ([a (make-handler UV_ASYNC)]
             [ex (current-exception-state)]
             [code (foreign-callable
                    (lambda (h)
                      (unlock-object code)
                      (close-handle a nothing)
                      (uv-context-push-callback! ctx (lambda ()
                                          (current-exception-state ex)
                                          (f))))
                    (void*)
                    void)])
      (lock-object code)
      (check (uv-async-init (uv-context-loop ctx) a (foreign-callable-entry-point code)))
      (check (uv-async-send a))))

  (define (uv/call-after ctx timeout repeat cb)
    (letrec ([a (make-handler UV_TIMER)]
             [ex (current-exception-state)]
             [code (foreign-callable
                    (lambda (h)
                      (uv-context-push-callback! ctx (lambda ()
                                          (current-exception-state ex)
                                          (unless (cb)
                                            (unlock-object code)
                                            (close-handle a nothing)))))
                    (void*)
                    void)])
      (lock-object code)
      (check (uv-timer-init (uv-context-loop ctx)  a))
      (check (uv-timer-start a (foreign-callable-entry-point code) timeout repeat))))

  (define malloc-gc)
  (let ([malloc-guardian (make-guardian)])
    (set! malloc-gc
          (lambda (size)
            (let ([m (foreign-alloc size)])
              (malloc-guardian m)
              m)))
    (collect-request-handler
     (lambda ()
       (collect)
       (let f ()
         (let ([m (malloc-guardian)])
           (when m
             (foreign-free m)
             (f)))))))

  )
