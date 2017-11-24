;; -*- geiser-scheme-implementation: chez -*-
(library (uv)
  (export with-uvloop
          stream-read
          sockaddr->ip-address
          stream-write
          uv-run
          display-addrinfo
          sockaddr-set-port
          addrinfo
          tcp-connect
          getaddrinfo)
  (import (chezscheme))

  (define init
    (begin
      (load-shared-object "libuv.dylib")))

  (define AF_INET 2)
  (define SOCK_STREAM 1)
  (define SOCK_DGRAM 2)

  (define-ftype uv-buf
    (struct
        (base (* unsigned-8))
      (len size_t)))

  (define-ftype sa_family_t unsigned-8)
  (define-ftype in_port_t unsigned-16)

  (define-ftype socklen_t unsigned-int)
  (define-ftype sockaddr_un
    (struct (sun_family sa_family_t)
      (sun_data (array 108 char))))

  (define-ftype in_addr_t unsigned-32)
  (define-ftype in_addr
    (struct (s_addr in_addr_t)))

  (define-ftype sockaddr_in
    (struct
      (sin_len unsigned-8)
      (sin_family sa_family_t)
      (sin_port in_port_t)
      (sin_addr in_addr)
      (sin_zero (array 8 unsigned-8))))

  (define-ftype sockaddr
    (struct
      (len unsigned-8)
      (sa_family sa_family_t)
      (sa_data (array 14 unsigned-8))))

  (define-ftype addrinfo
    (struct
      (ai_flags int)
      (ai_family int)
      (ai_socktype int)
      (ai_protocol int)
      (ai_addrlen socklen_t)
      (ai_canonname (* char))
      (ai_addr (* sockaddr))
      (ai_next (* addrinfo))))

  (define INADDR_ANY 0)

  (define int->ip-address
    (lambda (in-addr)
      (list (fxand in-addr 255)
            (fxand (fxsra in-addr 8) 255)
            (fxand (fxsra in-addr 16) 255)
            (fxsra in-addr 24))))

  (define sockaddr->ip-address
    (lambda (s)
      (if (= AF_INET (ftype-ref sockaddr (sa_family) s))
          (int->ip-address
           (ftype-ref sockaddr_in (sin_addr s_addr)
                      (make-ftype-pointer sockaddr_in
                                          (ftype-pointer-address s)))))))

  (define UV_UNKNOWN_HANDLE 0)
  (define UV_ASYNC 1)
  (define UV_CHECK 2)
  (define UV_FS_EVENT 3)
  (define UV_FS_POLL 4)
  (define UV_HANDLE 5)
  (define UV_IDLE 6)
  (define UV_NAMED_PIPE 7)
  (define UV_POLL 8)
  (define UV_PREPARE 9)
  (define UV_PROCESS 10)
  (define UV_STREAM 11)
  (define UV_TCP 12)
  (define UV_TIMER 13)
  (define UV_TTY 14)
  (define UV_UDP 15)
  (define UV_SIGNAL 16)
  (define UV_FILE 17)
  (define UV_HANDLE_TYPE_MAX 18)

  (define uvloop-size
    (foreign-procedure "uv_loop_size"
                       ()
                       int))

  (define memset
    (foreign-procedure "memset"
                       (void* int size_t)
                       void))

  (define alloc-zero
    (lambda (size)
      (let ([p (foreign-alloc size)])
        (memset p 0 size)
        p)))


  (define uvloop-create
    (lambda ()
      (define uvloop-init
        (foreign-procedure "uv_loop_init"
                           (void*)
                           int))
      (let ((l (alloc-zero (uvloop-size))))
        (uvloop-init l)
        l)))

  (define uvloop-close
    (foreign-procedure "uv_loop_close"
                       (void*)
                       void))

  (define uv-run
    (foreign-procedure "uv_run"
                       (void* int)
                       int))

  (define (uvloop-destroy uv)
    (uvloop-close uv)
    (foreign-free uv))

  (define uv-stop
    (foreign-procedure "uv_stop"
                       (void*)
                       void))

  (define uv-idle-init
    (foreign-procedure "uv_idle_init"
                       (void* void*)
                       int))

  (define with-uvloop
    (lambda (f)
      (let ([l (uvloop-create)])
        (dynamic-wind
          (lambda () #f)
          (lambda () (f l))
          (lambda ()
            (uvloop-close l))))))

  (define req-size
    (foreign-procedure "uv_req_size"
                       (int)
                       size_t))

  (define UV_UNKNOWN_REQ 0)
  (define UV_REQ 1)
  (define UV_CONNECT 2)
  (define UV_WRITE 3)
  (define UV_SHUTDOWN 4)
  (define UV_GETADDRINFO 8)

  (define make-req
    (lambda (t)
      (alloc-zero (req-size t))))

  (define getaddrinfo
    (lambda (loop name cb)
      (define uv-getaddrinfo
        (foreign-procedure "uv_getaddrinfo"
                           (void* void* void* string string (* addrinfo))
                           int))
      (define uv-freeaddrinfo
        (foreign-procedure "uv_freeaddrinfo"
                           ((* addrinfo))
                           void))
      (let* ([hint (make-ftype-pointer addrinfo (alloc-zero (ftype-sizeof addrinfo)))]
             [req (make-req UV_GETADDRINFO)]
             [code (foreign-callable
                    (lambda (req status addr)
                      (cb status addr)
                      (uv-freeaddrinfo addr)
                      (foreign-free req)
                      (foreign-free (ftype-pointer-address hint)))
                    (void* int (* addrinfo))
                    void)])
        (lock-object code)
        (ftype-set! addrinfo (ai_family) hint AF_INET)
        (ftype-set! addrinfo (ai_socktype) hint SOCK_STREAM)
        (uv-getaddrinfo loop req (foreign-callable-entry-point code)
                        name #f hint))))

  (define handle-size
    (foreign-procedure "uv_handle_size"
                       (int)
                       int))

  (define make-handler
    (lambda (t)
      (foreign-alloc (handle-size t))))

  (define uv-idle-start
    (foreign-procedure "uv_idle_start"
                       (void* void*)
                       int))

  (define uv-idle-stop
    (foreign-procedure "uv_idle_stop"
                       (void*)
                       int))

  (define make-idle
    (lambda (l cb)
      (let* ([idle (make-handler UV_IDLE)]
             [code (foreign-callable (lambda (ll)
                                       (if (cb)
                                           #t
                                           (begin
                                             (uv-idle-stop idle))))
                                     (void*)
                                    void)])
        (uv-idle-init l idle)
        (lock-object code)
        (uv-idle-start idle (foreign-callable-entry-point code)))))

  (define alloc-buffer
    (foreign-callable-entry-point
     (let ([alloc (foreign-callable
                   (lambda (handle suggested-size b)
                     (let ([x (foreign-alloc suggested-size)])
                       (ftype-set! uv-buf (base) b (make-ftype-pointer unsigned-8 x))
                       (ftype-set! uv-buf (len) b suggested-size)))
                   (void* size_t (* uv-buf))
                   void)])
       (lock-object alloc)
       alloc)))

  (define uv-read-start
    (foreign-procedure "uv_read_start"
                       (void* void* void*)
                       int))


  (define (on-read cb)
    (foreign-callable-entry-point
     (let ([on-read (foreign-callable
                     (lambda (client num-read buf)
                       (display "on-read!")
                       (cb client num-read buf))
                     (void* ssize_t (* uv-buf))
                     void)])
       (lock-object on-read)
       on-read)))

  (define htons
    (foreign-procedure "htons"
                       (unsigned-short)
                       unsigned-short))

  (define (sockaddr-set-port s p)
    (ftype-set! sockaddr_in (sin_port)
                (make-ftype-pointer sockaddr_in (ftype-pointer-address s))
                (htons p)))

  (define bytevector-for-each
    (lambda (f bv)
      (let ([len (bytevector-length bv)])
        (let loop ([i 0])
                (when (< i len)
                  (f (bytevector-u8-ref bv i) i)
                  (loop (+ i 1)))))))

  (define (string->buf s)
    (let* ([buf (make-ftype-pointer uv-buf (foreign-alloc (ftype-sizeof uv-buf)))]
           [bytes (string->utf8 s)]
           [b (foreign-alloc (bytevector-length bytes))])
      (bytevector-for-each
       (lambda (x i)
         (foreign-set! 'unsigned-8 b i x))
       bytes)
      (ftype-set! uv-buf (base) buf (make-ftype-pointer unsigned-8 b))
      (ftype-set! uv-buf (len) buf (bytevector-length bytes))
      buf))

  (define uv-write
    (foreign-procedure "uv_write"
                       (void* void* (* uv-buf) unsigned-int void*)
                       int))

  (define (stream-write stream s cb)
    (letrec ([buf (string->buf s)]
             [code (foreign-callable
                    (lambda (req status)
                      (cb stream status)
                      (foreign-free (ftype-pointer-address buf))
                      (unlock-object code))
                    (void* int)
                    void)]
             [write-req (make-req UV_WRITE)])
      (lock-object code)
      (check (uv-write write-req stream buf 1 (foreign-callable-entry-point code)))))

  (define uv-read-stop
    (foreign-procedure "uv_read_stop"
                       (void*)
                       int))

  (define (handle-close h cb)
    (define uv-close
      (foreign-procedure "uv_close"
                         (void* void*)
                         void))
    (letrec ([code (foreign-callable
                    (lambda (h)
                      (cb h)
                      (foreign-free h)
                      (unlock-object code))
                    (void*)
                    void)])
      (lock-object code)
      (uv-close h (foreign-callable-entry-point code))))

  (define uv-strerror
    (foreign-procedure "uv_strerror"
                       (int)
                       (* char)))

  (define uv-err-name
    (foreign-procedure "uv_err_name"
                       (int)
                       string))
  (define strerror
    (lambda (x)
      (define strerror_r
        (foreign-procedure "strerror_r"
                           (int (* char) size_t)
                           int))
      (let ([buf (make-bytevector 2048)])
        (strerror_r x buf 2048))))

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

  (define uv-error? positive?)

  (macro (check expr)
    `(let ([r ,expr])
       (unless (= 0 r)
         (let ([errstr (if (uv-error? r)
                           (uv-err-name r)
                           (strerror (abs r)))])
           (error ',expr "error in uv" r))
         #t)))

  (define (stream-read stream cb)
    (letrec ([code (foreign-callable
                  (lambda (s nb buf)
                    (let ([p (make-bytevector nb)])
                      (let loop ([i 0])
                        (when (< i nb)
                          (bytevector-u8-set! p i (ftype-ref uv-buf (base i) buf))
                          (loop (+ i 1))))
                      (cb stream p)
                      (check (uv-read-stop stream))
                      (unlock-object code)))
                  (void* ssize_t (* uv-buf))
                  void)])
      (lock-object code)
      (check (uv-read-start stream alloc-buffer (foreign-callable-entry-point code)))))

  (define (tcp-connect loop addr on-conn)
    (define uv-tcp-init
      (foreign-procedure "uv_tcp_init"
                         (void* void*)
                         int))
    (define uv-tcp-connect
      (foreign-procedure "uv_tcp_connect"
                         (void* void* (* sockaddr) void*)
                         int))
    (letrec* ([conn (alloc-zero (handle-size UV_STREAM))]
              [socket (alloc-zero (handle-size UV_TCP))]
              [code (foreign-callable
                     (lambda (conn status)
                       (on-conn socket status)
                       (unlock-object code))
                     (void* int)
                     void)])
      (lock-object code)
      (check (uv-tcp-init loop socket))
      (check (uv-tcp-connect conn socket addr
                             (foreign-callable-entry-point code)))))

  (define (for-each-addr a f)
    (let ([next (ftype-ref addrinfo (ai_next) a)])
      (f a)
      (if (ftype-pointer-null? next)
          #f
          (for-each-addr next f))))

  (define display-addrinfo
    (lambda (a)
      (define display
        (lambda (a)
          (format #t "ai_flags: ~A~n" (ftype-ref addrinfo (ai_flags) a))
          (format #t "ai_family: ~A~n" (ftype-ref addrinfo (ai_family) a))
          (format #t "ai_sockettype: ~A~n" (ftype-ref addrinfo (ai_socktype) a))
          (format #t "ai_socklen: ~A~n" (ftype-ref addrinfo (ai_addrlen) a))
          (format #t "ai_addr: ~A~n" (sockaddr->ip-address (ftype-ref addrinfo (ai_addr) a)))
          (format #t "ai_canonname: ~A~n"  (ftype-ref addrinfo (ai_canonname) a))
          (format #t "ai_next: ~A~n" (ftype-ref addrinfo (ai_next) a))))
      (for-each-addr a display))))
