
(define-ftype uv-buf
  (struct
      (base (* unsigned-8))
    (len size_t)))

(define UV_EOF -4095)

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
(define UV_GETADDRINFO 8)

(define SIGHUP 1)
(define SIGINT 2)
(define SIGTERM 15)

(define uvloop-size
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

(define (close-all-handles uv-loop)
  (walk-handles uv-loop
                (lambda (handle arg)
                  (handle-close handle nothing))))
(define uvloop-init
  (foreign-procedure "uv_loop_init"
                     (void*)
                     int))

(define uvloop-create
  (lambda ()
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
                     int))

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
