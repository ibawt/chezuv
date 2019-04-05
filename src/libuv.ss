(library (libuv)
  (export
   ;; from uv.h
   uv-buf
   UV_EOF
   UV_EBUSY
   UV_UNKNOWN_HANDLE
   UV_ASYNC
   UV_CHECK
   UV_FS_EVENT
   UV_FS_POLL
   UV_HANDLE
   UV_IDLE
   UV_NAMED_PIPE
   UV_POLL
   UV_PREPARE
   UV_PROCESS
   UV_STREAM
   UV_TCP
   UV_TIMER
   UV_TTY
   UV_UDP
   UV_SIGNAL
   UV_FILE
   UV_HANDLE_TYPE_MAX
   UV_UNKNOWN_REQ
   UV_REQ
   UV_CONNECT
   UV_WRITE
   UV_SHUTDOWN
   UV_WORK
   UV_GETADDRINFO
   SIGHUP
   SIGINT
   SIGTERM
   uv-loop-set-data
   uv-loop-get-data
   uv-handle-get-type
   uv-handle-type-name
   uv-signal-init
   uv-signal-start-one-shot
   uv-signal-stop
   uv-loop-init
   uv-loop-close
   uv-run
   uv-loop-destroy
   uv-stop
   uv-idle-init
   uv-walk
   uv-req-size
   uv-getaddrinfo
   uv-freeaddrinfo
   uv-handle-size
   uv-idle-start
   uv-idle-stop
   uv-read-start
   uv-write
   uv-read-stop
   uv-close
   uv-is-closing
   uv-err-name
   uv-shutdown
   uv-tcp-init
   uv-tcp-connect
   uv-tcp-bind
   uv-async-init
   uv-async-send
   uv-timer-init
   uv-timer-start
   uv-timer-stop
   uv-timer-again
   uv-listen
   uv-accept
   uv-ip4-addr

   ;; helpers
   string->uv-buf
   make-uv-loop
   uv-error?
   make-uv-loop
   close-handle
   strerror_r
   alloc-zero
   close-all-handles
   make-req
   make-handler)

  (import (chezscheme)
          (inet)
          (bufferpool)
          (utils))

  (define init
    (case (machine-type)
      ((ta6le a6le) (load-shared-object "libuv.so.1"))))

  (define-ftype uv-buf
    (struct
        (base (* unsigned-8))
      (len size_t)))

  (define uv-error? positive?)

  (define UV_EOF -4095)
  (define UV_EBUSY -16)

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


  (define UV_UNKNOWN_REQ 0)
  (define UV_REQ 1)
  (define UV_CONNECT 2)
  (define UV_WRITE 3)
  (define UV_SHUTDOWN 4)
  (define UV_WORK 7)
  (define UV_GETADDRINFO 8)

  (define SIGHUP 1)
  (define SIGINT 2)
  (define SIGTERM 15)

  (define (handle-type-name handle)
    (uv-handle-type-name (uv-handle-get-type handle)))

  (define (make-req t)
    (alloc-zero (uv-req-size t)))

  (define (make-handler t)
    (alloc-zero (uv-handle-size t)))

  (define uv-loop-size
    (foreign-procedure "uv_loop_size"
                       ()
                       int))

  (define uv-handle-get-type
    (foreign-procedure "uv_handle_get_type"
                       (void*)
                       int))

  (define uv-handle-type-name
    (foreign-procedure "uv_handle_type_name"
                       (int)
                       string))
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

  (define uv-loop-init
    (foreign-procedure "uv_loop_init"
                       (void*)
                       int))

  (define uv-loop-close
    (foreign-procedure "uv_loop_close"
                       (void*)
                       int))

  (define uv-run
    (foreign-procedure "uv_run"
                       (void* int)
                       int))

  (define uv-loop-set-data
    (foreign-procedure "uv_loop_set_data"
                       (void* void*)
                       void))

  (define uv-loop-get-data
    (foreign-procedure "uv_loop_get_data"
                       (void*)
                       void*))

  (define uv-stop
    (foreign-procedure "uv_stop"
                       (void*)
                       void))

  (define uv-idle-init
    (foreign-procedure "uv_idle_init"
                       (void* void*)
                       int))



  (define uv-walk
    (foreign-procedure "uv_walk"
                       (void* void* void*)
                       int))

  (define uv-req-size
    (foreign-procedure "uv_req_size"
                       (int)
                       size_t))

  (define uv-getaddrinfo
    (foreign-procedure "uv_getaddrinfo"
                       (void* void* void* string string (* addrinfo))
                       int))
  (define uv-freeaddrinfo
    (foreign-procedure "uv_freeaddrinfo"
                       ((* addrinfo))
                       void))

  (define uv-handle-size
    (foreign-procedure "uv_handle_size"
                       (int)
                       int))

  (define uv-idle-start
    (foreign-procedure "uv_idle_start"
                       (void* void*)
                       int))

  (define uv-idle-stop
    (foreign-procedure "uv_idle_stop"
                       (void*)
                       int))

  (define uv-read-start
    (foreign-procedure "uv_read_start"
                       (void* void* void*)
                       int))
  (define uv-write
    (foreign-procedure "uv_write"
                       (void* void* (* uv-buf) unsigned-int void*)
                       int))

  (define uv-read-stop
    (foreign-procedure "uv_read_stop"
                       (void*)
                       int))

  (define uv-close
    (foreign-procedure "uv_close"
                       (void* void*)
                       void))
  (define uv-is-closing
    (foreign-procedure "uv_is_closing"
                       (void*)
                       int))

  (define uv-strerror
    (foreign-procedure "uv_strerror"
                       (int)
                       string))

  (define uv-err-name
    (foreign-procedure "uv_err_name"
                       (int)
                       string))

  (define uv-shutdown
    (foreign-procedure "uv_shutdown"
                       (void* void* void*)
                       int))

  (define uv-tcp-init
    (foreign-procedure "uv_tcp_init"
                       (void* void*)
                       int))
  (define uv-tcp-connect
    (foreign-procedure "uv_tcp_connect"
                       (void* void* (* sockaddr) void*)
                       int))

  (define strerror_r
    (foreign-procedure "strerror_r"
                       (int u8* size_t)
                       string))

  (define uv-tcp-bind
    (foreign-procedure "uv_tcp_bind"
                       (void* void* unsigned-int)
                       int))

  (define uv-listen
    (foreign-procedure "uv_listen"
                       (void* int void*)
                       int))

  (define uv-accept
    (foreign-procedure "uv_accept"
                       (void* void*)
                       int))

  (define uv-ip4-addr
    (foreign-procedure "uv_ip4_addr"
                       (string int (* sockaddr_in))
                       int))

  (define uv-async-init
    (foreign-procedure "uv_async_init"
                       (void* void* void*)
                       int))

  (define uv-async-send
    (foreign-procedure "uv_async_send"
                       (void*)
                       int))

  (define uv-timer-init
    (foreign-procedure "uv_timer_init"
                       (void* void*)
                       int))

  (define uv-timer-start
    (foreign-procedure "uv_timer_start"
                       (void* void* integer-64 integer-64)
                       int))

  (define uv-timer-stop
    (foreign-procedure "uv_timer_stop"
                       (void*)
                       int))

  (define uv-timer-again
    (foreign-procedure "uv_timer_again"
                       (void*)
                       int))

  (define (string->uv-buf s)
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

  (define (uv-loop-destroy uv)
    (uv-loop-close uv)
    (foreign-free uv))

  (define (make-uv-loop)
    (let ((l (alloc-zero (uv-loop-size))))
      (uv-loop-init l)
      l))

  (define (walk-handles uv-loop on-handle)
    (let ([code (foreign-callable on-handle (void* void*) void)])
      (lock-object code)
      (uv-walk uv-loop (foreign-callable-entry-point code) 0)
      (unlock-object code)))

  (define (close-all-handles uv-loop)
    (walk-handles uv-loop
                  (lambda (handle arg)
                    (close-handle handle (lambda (x) #f)))))

  (define (close-handle h cb)
    (if (= 0 (uv-is-closing h))
      (letrec ([code (foreign-callable
                      (lambda (h)
                        (cb h)
                        (foreign-free h)
                        (unlock-object code))
                      (void*)
                      void)])

        (lock-object code)
        (uv-close h (foreign-callable-entry-point code)))
      (cb h))))
