;; -*- geiser-scheme-implementation: chez -*-
(library (uv)
  (export with-uvloop
          make-http-request
          close
          stream-read
          make-reader
          read-http-request
          read-headers
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

  (define (nothing . args)
    #f)

  (define buf-pool '())

  (define (get-buf)
    (if (null? buf-pool)
        (foreign-alloc 65536)
        (let ([x (car buf-pool)])
          (set! buf-pool (cdr buf-pool))
          x)))

  (define (free-buf buf)
    (set! buf-pool (cons buf buf-pool)))

  (define-ftype uv-buf
    (struct
        (base (* unsigned-8))
      (len size_t)))

  (include "inet.ss")
  (include "utils.ss")

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

  (define SIGHUP 1)
  (define SIGINT 2)
  (define SIGTERM 15)

  (define uvloop-size
    (foreign-procedure "uv_loop_size"
                       ()
                       int))

  (define alloc-zero
    (lambda (size)
      (let ([p (foreign-alloc size)])
        (memset p 0 size)
        p)))

  (define uv-handle-get-type
    (foreign-procedure "uv_handle_get_type"
                       (void*)
                       int))

  (define uv-handle-type-name
    (foreign-procedure "uv_handle_type_name"
                       (int)
                       string))

  (define (handle-type-name handle)
    (uv-handle-type-name (uv-handle-get-type handle)))

  (define uv-signal-init
    (foreign-procedure "uv_signal_init"
                       (void* void*)
                       int))

  (define uv-signal-start-one-shot
    (foreign-procedure "uv_signal_start_oneshot"
                       (void* void* int)
                       int))

  (define uv-signal-stop
    (foreign-procedure "uv_signal_stop"
                       (void*)
                       int))

  (define (close-all-handles uv-loop)
    (walk-handles uv-loop
                  (lambda (handle arg)
                    (handle-close handle nothing))))


  (define (walk-handles uv-loop on-handle)
    (define uv-walk
      (foreign-procedure "uv_walk"
                         (void* void* void*)
                         int))
    (let ([code (foreign-callable on-handle (void* void*) void)])
      (lock-object code)
      (uv-walk uv-loop (foreign-callable-entry-point code) 0)))

  (define (register-signal-handlers uv-loop)
    (letrec ([sig-handle (make-handler UV_SIGNAL)]
             [code (foreign-callable
                    (lambda (sig-handle signum)
                      (close-all-handles uv-loop)
                      (when (= signum SIGINT)
                        (uv-stop uv-loop))
                      (unlock-object code))
                    (void* int)
                    void)])
      (lock-object code)
      (check (uv-signal-init uv-loop sig-handle))
      (check (uv-signal-start-one-shot sig-handle
                                       (foreign-callable-entry-point code)
                                       SIGINT))))

  (define (close loop)
    (uv-stop loop)
    (close-all-handles loop))

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
                       int))

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
        (register-signal-handlers l)
        (dynamic-wind
          (lambda () #f)
          (lambda () (f l))
          (lambda ()
            (format #t "about to close~n"))))))

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
      (letrec ([hint (make-ftype-pointer addrinfo (alloc-zero (ftype-sizeof addrinfo)))]
               [req (make-req UV_GETADDRINFO)]
               [code (foreign-callable
                      (lambda (req status addr)
                      (unlock-object code)
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

  (define (make-handler t)
    (define handle-size
      (foreign-procedure "uv_handle_size"
                         (int)
                         int))
      (let ([mem (foreign-alloc (handle-size t))])
        mem))


  (define (make-idle l cb)
    (define uv-idle-start
      (foreign-procedure "uv_idle_start"
                         (void* void*)
                         int))

    (define uv-idle-stop
      (foreign-procedure "uv_idle_stop"
                         (void*)
                         int))
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
      (uv-idle-start idle (foreign-callable-entry-point code))))

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

  (define uv-read-start
    (foreign-procedure "uv_read_start"
                       (void* void* void*)
                       int))

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

  (define (alloc-uv-buf nb)
    (let ([bytes (make-ftype-pointer unsigned-8 (foreign-alloc nb))]
          [buf (make-ftype-pointer uv-buf (foreign-alloc (ftype-sizeof uv-buf)))])
      (ftype-set! uv-buf (base) buf bytes)
      (ftype-set! uv-buf (len) buf nb)
      buf))

  (define (free-uv-buf buf)
    (foreign-free (ftype-pointer-address (ftype-ref uv-buf (base) buf)))
    (foreign-free (ftype-pointer-address buf)))

  (define (stream-write stream s cb)
    (letrec ([buf (string->buf s)]
             [code (foreign-callable
                    (lambda (req status)
                      (unlock-object code)
                      (cb stream status)
                      (free-uv-buf buf))
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
    (define uv-is-closing
      (foreign-procedure "uv_is_closing"
                         (void*)
                         int))
    (when (= 0 (uv-is-closing h))
      (letrec ([code (foreign-callable
                      (lambda (h)
                        (cb h)
                        (foreign-free h)
                        (unlock-object code))
                      (void*)
                      void)])

        (lock-object code)
        (uv-close h (foreign-callable-entry-point code)))))

  (define uv-strerror
    (foreign-procedure "uv_strerror"
                       (int)
                       string))

  (define uv-err-name
    (foreign-procedure "uv_err_name"
                       (int)
                       string))
  (define strerror
    (lambda (x)
      (define strerror_r
        (foreign-procedure "strerror_r"
                           (int u8* size_t)
                           int))
      (let* ([buf (make-bytevector 2048)]
             [n (strerror_r x buf 2048)])
        (utf8->string (slice-bytevector buf 0 n)))))


  (define uv-error? positive?)

  (macro (check expr)
    `(let ([r ,expr])
       (unless (= 0 r)
         (let ([errstr (if (uv-error? r)
                           (uv-err-name r)
                           (strerror (abs r)))])
           (error ,(format #f "~a" expr) errstr r))
         #t)))

  (define (stream-read stream cb)
    (letrec ([code (foreign-callable
                  (lambda (s nb buf)
                    (let ([p (make-bytevector nb)])
                      (memcpy p (ftype-pointer-address (ftype-ref uv-buf (base) buf)) nb)
                      (cb stream p)
                      (check (uv-read-stop stream))
                      (unlock-object code)
                      (free-buf (ftype-pointer-address (ftype-ref uv-buf (base) buf)))))
                  (void* ssize_t (* uv-buf))
                  void)])
      (lock-object code)
      (check (uv-read-start stream alloc-buffer (foreign-callable-entry-point code)))))

  (define (make-reader stream)
    (define buf #f)
    (define read-pos 0)
    (define buf-len 0)
    (define (send-buf bv start len on-read)
      (if (eq? 'eol len)
          (let ([newline-pos (find-in-bytevector 10 buf read-pos)])
            (if newline-pos
                (send-buf bv start (- (inc newline-pos) read-pos) on-read)
                (error 'newline-pos "line is really big and I didn't handle this case")))
          (let ([len (if (<= buf-len (+ read-pos len))
                         (- buf-len read-pos)
                         len)])
            (bytevector-copy! buf read-pos bv start len)
            (set! read-pos (+ read-pos len))
            (when (>= read-pos buf-len)
              (set! buf #f))
            (on-read bv len))))
    (lambda (bv start len on-read )
      (if buf
          (send-buf bv start len on-read)
          (stream-read stream
                       (lambda (s b)
                         (set! buf b)
                         (set! read-pos 0)
                         (set! buf-len (bytevector-length b))
                         (send-buf bv start len on-read))))))

  (define (read-lines reader on-line)
    (reader (make-bytevector 1024) 0 'eol
            (lambda (bv num-read)
              (let ([trim (if (= 13 (bytevector-u8-ref bv (- num-read 2)))
                              (- num-read 2)
                              (- num-read 1))])
                (if (on-line (truncate-bytevector! bv trim))
                    (read-lines reader on-line))))))

  (define (parse-url url)
    (let* ([colon-pos (find-char url #\: 0)]
           [proto (substring url 0 colon-pos)])
      (let* ([path-start (or (find-char url #\/ (+ 3 colon-pos)) (string-length url))])
        (let ([host (substring url (+ colon-pos 3) path-start)])
          `((path "/") (protocol ,proto) (host ,host) (port 80))))))

  (define (parse-headers raw)
    (define (split-header s)
      (if (= 0 (string-length s))
          '()
          (let ([pos (find-char s #\: 0)])
            (if (= -1 pos) (error 'split-header "can't find :")
                (let ([next (find-not-char s #\space (+ 1 pos))])
                  `(,(substring s 0 pos) ,(substring s (or next (+ 1 pos)) (string-length s))))))))
    (map (lambda (x)
           (split-header (utf8->string x)))
         raw))

  (define (parse-status status-line)
    (let ([splits (string-split (utf8->string status-line) #\space)])
      (cadr splits)))

  (define (read-fully reader n on-done)
    (let ([bv (make-bytevector n)])
      (reader bv 0 n (lambda (bv len)
                       (on-done bv)))))

  (define (read-http-request reader done)
    (read-headers reader
                  (lambda (headers)
                    (let* ([req-line (car headers)]
                           [headers-raw (cdr headers)]
                           [status (parse-status req-line)]
                           [headers (parse-headers headers-raw)]
                           [content-length (string->number (cadr (assoc "Content-Length" headers)))])
                      (read-fully reader content-length
                                  (lambda (body)
                                    (done status headers body)))))))

  (define (close-stream stream)
    (define uv-shutdown
      (foreign-procedure "uv_shutdown"
                         (void* void* void*)
                         int))
    (letrec ([shutdown-req (make-req UV_SHUTDOWN)]
             [code (foreign-callable (lambda (req status)
                                       (unlock-object code)
                                       (handle-close stream nothing))
                                     (void* int)
                                     void)])
      (lock-object code)
      (check (uv-shutdown shutdown-req stream (foreign-callable-entry-point code)))))

  (define (read-headers reader done)
    (let ([lines '()])
      (read-lines reader
                  (lambda (line)
                    (if (= 0 (bytevector-length line))
                        (begin
                          (done (reverse lines))
                          #f)
                        (begin
                          (set! lines (cons line lines))
                          #t))))))

  (define (tcp-connect loop addr on-conn)
    (define uv-tcp-init
      (foreign-procedure "uv_tcp_init"
                         (void* void*)
                         int))
    (define uv-tcp-connect
      (foreign-procedure "uv_tcp_connect"
                         (void* void* (* sockaddr) void*)
                         int))
    (letrec* ([conn (make-handler UV_STREAM)]
              [socket (make-handler UV_TCP)]
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

  (define (make-http-request loop url on-done)
    (let ([url (parse-url url)])
      (getaddrinfo loop (cadr (assoc 'host url))
                   (lambda (status addr)
                     (when (= 0 status)
                       (let ([s (ftype-ref addrinfo (ai_addr) addr)])
                         (sockaddr-set-port s (cadr (assoc 'port url)))
                         (tcp-connect loop s
                                      (lambda (conn status)
                                        (stream-write conn (format #f "GET ~a HTTP/1.0\r\n\r\n" (cadr (assoc 'path url)))
                                                      (lambda (s status)
                                                        (read-http-request (make-reader s)
                                                                           (lambda (status headers body)
                                                                             (close-stream s)
                                                                             (on-done status headers body))))))))))))))

