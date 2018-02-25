;; -*- geiser-scheme-implementation: chez -*-
(library (uv)
  (export uv/with-loop
          uv/make-http-request
          uv/string->url
          uv/url-host
          uv/url-protocol
          uv/url-path
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
          uv/serve-https
          uv/make-http-request
          uv/make-https-request
          uv/call-with-ssl-context
          uv/static-file-handler
          uv/getaddrinfo)
  (import (chezscheme)
          (inet)
          (libuv)
          (alloc)
          (irregex))

  (define init
    (begin
      (load-shared-object "libchezuv.dylib")
      ((foreign-procedure "init_ssl" () void))))

  ;; --------------------------------------------------------------------------------
  ;; Private
  ;; --------------------------------------------------------------------------------

  (include "monad.ss")
  (include "utils.ss")

  (define (nothing . args)
    #f)

  (define uv-error? positive?)

  (define (info . args)
    (apply format #t args)
    (newline))

  (macro (check expr)
    `(let ([r ,expr])
       (unless (= 0 r)
         (let ([errstr (if (uv-error? r)
                           (uv-err-name r)
                           (strerror (abs r)))])
           (error ,(format #f "~a" expr) errstr r))
         #t)))

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
        (if (= 0 n)
            (utf8->string (bytevector-truncate! buf (find-in-bytevector 0 buf 0)))
            (error 'not-found "errno not found")))))

  (define (handle-type-name handle)
    (uv-handle-type-name (uv-handle-get-type handle)))


  (define (register-signal-handlers uv-loop)
    (letrec ([sig-handle (make-handler UV_SIGNAL)]
             [code (foreign-callable
                    (lambda (sig-handle signum)
                      (when (= signum SIGINT)
                        (close-all-handles uv-loop)
                        (uv-stop uv-loop)
                        (unlock-object code)))
                    (void* int)
                    void)])
      (lock-object code)
      (check (uv-signal-init uv-loop sig-handle))
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

  (define make-req
    (lambda (t)
      (alloc-zero (uv-req-size t))))

  (define (make-handler t)
    (let ([mem (foreign-alloc (uv-handle-size t))])
      mem))

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

 (define (read-headers reader done)
   (let ([lines '()])
     (uv/read-lines reader
                 (lambda (line)
                   (if (or (not line) (= 0 (bytevector-length line)))
                       (begin
                         (if line
                             (done (reverse lines)))
                         #f)
                       (begin
                         (set! lines (cons line lines))
                         #t))))))

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
     splits))

  (define (uv/close loop)
    (uv-stop loop)
    (close-all-handles loop))

  (define (uv/stop loop)
    (uv-stop loop))

  (define uv/with-loop
    (lambda (f)
      (let ([l (uvloop-create)])
        (register-signal-handlers l)
        (dynamic-wind
          (lambda () #f)
          (lambda ()
            (f l)
            (uv-run l 0))
          (lambda ()
            (uv-stop l))))))

  (define uv/getaddrinfo
    (lambda (loop name)
      (make-async
       (lambda (ok fail)
         (letrec ([hint (make-ftype-pointer addrinfo (alloc-zero (ftype-sizeof addrinfo)))]
                  [req (make-req UV_GETADDRINFO)]
                  [code (foreign-callable
                         (lambda (req status addr)
                           (unlock-object code)
                           (if (= 0 status)
                               (ok addr)
                               (fail addr))
                           (uv-freeaddrinfo addr)
                           (foreign-free req)
                           (foreign-free (ftype-pointer-address hint)))
                         (void* int (* addrinfo))
                         void)])
           (lock-object code)
           (ftype-set! addrinfo (ai_family) hint AF_INET)
           (ftype-set! addrinfo (ai_socktype) hint SOCK_STREAM)
           (uv-getaddrinfo loop req (foreign-callable-entry-point code)
                           name #f hint))))))



  (define (uv/make-idle l cb)
    (letrec* ([idle (make-handler UV_IDLE)]
              [code (foreign-callable (lambda (ll)
                                        (if (cb)
                                            #t
                                            (begin
                                              (unlock-object code)
                                              (uv-idle-stop idle)
                                              (foreign-free idle))))
                                      (void*)
                                      void)])
      (uv-idle-init l idle)
      (lock-object code)
      (uv-idle-start idle (foreign-callable-entry-point code))))

  (define (uv/stream-write stream s)
    (make-async
     (lambda (ok fail)
       (letrec ([buf (if (string? s) (string->buf s) s)]
                [write-req (make-req UV_WRITE)]
                [code (foreign-callable
                       (lambda (req status)
                         (free-buf (ftype-pointer-address (ftype-ref uv-buf (base) buf)))
                         (foreign-free write-req)
                         (unlock-object code)
                         (if (= 0 status)
                             (ok status)
                             (fail status)))
                       (void* int)
                       void)])
         (lock-object code)
         (check (uv-write write-req stream buf 1 (foreign-callable-entry-point code)))))))

  (define (uv/stream-read-raw stream cb)
    (letrec ([code (foreign-callable
                    (lambda (s nb buf)
                      (check (uv-read-stop stream))
                      (if (negative? nb)
                          (begin
                            (unlock-object code)
                            (free-buf (ftype-pointer-address (ftype-ref uv-buf (base) buf)))
                            (cb #f #f))
                          (begin
                            (unlock-object code)
                            (cb nb buf))))
                    (void* ssize_t (* uv-buf))
                    void)])
      (lock-object code)
      (check (uv-read-start stream alloc-buffer (foreign-callable-entry-point code)))))

  (define (uv/stream-read stream cb)
    (letrec ([code (foreign-callable
                    (lambda (s nb buf)
                      (check (uv-read-stop stream))
                      (unlock-object code)
                      (if (negative? nb)
                          (begin
                            (free-buf (ftype-pointer-address (ftype-ref uv-buf (base) buf)))
                            (cb #f #f))
                          (let ([p (make-bytevector nb)])
                            (memcpy p (ftype-pointer-address (ftype-ref uv-buf (base) buf)) nb)
                            (free-buf (ftype-pointer-address (ftype-ref uv-buf (base) buf)))
                            (cb stream p))))
                  (void* ssize_t (* uv-buf))
                  void)])
      (lock-object code)
      (check (uv-read-start stream alloc-buffer (foreign-callable-entry-point code)))))

  (define (uv/make-reader stream reader)
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
    (lambda (bv start len on-read)
      (if buf
          (send-buf bv start len on-read)
          (reader (lambda (s b)
                    (if s
                        (begin
                          (set! buf b)
                          (set! read-pos 0)
                          (set! buf-len (bytevector-length b))
                          (send-buf bv start len on-read))
                        (on-read #f #f)))))))

  (define trim-newline!
    (lambda (bv num-read)
      (truncate-bytevector! bv
                            (if (= 13 (bytevector-u8-ref bv (- num-read 2)))
                                (- num-read 2) (- num-read 1)))))

  (define (uv/read-lines reader on-line)
    (reader (make-bytevector 1024) 0 'eol
            (lambda (bv num-read)
              (if (and bv (on-line (trim-newline! bv num-read)))
                  (uv/read-lines reader on-line)
                  (on-line #f)))))

  (define (uv/read-fully reader n on-done)
    (let ([bv (make-bytevector n)])
      (reader bv 0 n (lambda (bv len)
                       (on-done bv)))))

  (define (header-value headers key)
    (let ([v (assoc key headers)])
      (if v
          (cadr v)
          #f)))

  (define (header->number headers key)
    (let ([v (header-value headers key)])
      (if v (string->number v) v)))

  (define (uv/read-http-response reader)
    (make-async
     (lambda (ok fail)
       (read-headers reader
                     (lambda (headers)
                       (if (pair? headers)
                        (let* ([status (parse-status (car headers))]
                               [headers (parse-headers (cdr headers))]
                               [content-length (header->number headers "Content-Length")])
                          (if content-length
                              (begin
                                (uv/read-fully reader content-length
                                              (lambda (body)
                                                (ok (list status headers body)))))
                              (ok (list status headers #t))))
                        (fail 'eof)))))))


  (define (uv/close-stream stream)
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

  (define (uv/tcp-connect loop addr)
    (make-async
     (lambda (ok fail)
       (letrec* ([conn (make-handler UV_STREAM)]
                 [socket (make-handler UV_TCP)]
                 [code (foreign-callable
                        (lambda (conn status)
                          (unlock-object code)
                          (if (= 0 status)
                              (ok socket)
                              (fail status)))
                        (void* int)
                        void)])
         (lock-object code)
         (check (uv-tcp-init loop socket))
         (check (uv-tcp-connect conn socket addr
                                (foreign-callable-entry-point code)))))))

  (define (addr->sockaddr addr port)
    (let ([s (ftype-ref addrinfo (ai_addr) addr)])
      (sockaddr-set-port s port)
      s))

  (define url-regex (irregex "(?<protocol>.+)://(?<host>[A-Za-z0-9._]+):?(?<port>\\d*)/?(?<path>.*)"))
  (define default-ports
    '((http 80)
      (https 443)))

  (define (uv/string->url u)
    (let ([m (irregex-search url-regex u)])
      (if m
          (let ([proto (string->symbol (irregex-match-substring m 'protocol))])
            `((host ,(irregex-match-substring m 'host))
              (protocol ,proto)
              (port ,(or (string->number (irregex-match-substring m 'port)) (cadr (assoc proto default-ports))))
              (path ,(string-append "/" (irregex-match-substring m 'path)))))
          #f)))

  (define (uv/url-host url)
    (cadr (assoc 'host url)))

  (define (uv/url-port url)
    (cadr (assoc 'port url)))

  (define (uv/url-protocol url)
    (cadr (assoc 'protocol url)))

  (define (uv/url-path url)
    (cadr (assoc 'path url)))

  (define (uv/ipv4->sockaddr addr)
    (let* ([split (string-split addr #\:)]
           [ip (if (pair? split) (car split) addr)]
           [port (if (pair? split) (string->number (cadr split)) 0)]
           [s (make-ftype-pointer sockaddr_in (foreign-alloc (ftype-sizeof sockaddr_in)))]
           [r (uv-ip4-addr ip port s)])
      (cond
       ((= r 0) s)
       (else
        (foreign-free (ftype-pointer-address s))
        #f))))

  (define (uv/tcp-listen loop addr on-conn)
    (letrec ([s-addr (uv/ipv4->sockaddr addr)]
             [h (make-handler UV_TCP)]
             [code (foreign-callable
                    (lambda (server status)
                      (if (= 0 status)
                        (let ([client (make-handler UV_TCP)])
                          (check (uv-tcp-init loop client))
                          (check (uv-accept server client))
                          (on-conn #f server client))
                        (begin
                          (on-conn status server #f))))
                    (void* int)
                    void)])
      (lock-object code)
      (check (uv-tcp-init loop h))
      (check (uv-tcp-bind h (ftype-pointer-address s-addr) 0))
      (check (uv-listen h 511 (foreign-callable-entry-point code)))
      (foreign-free (ftype-pointer-address s-addr))
      h))

  (define new-ssl-context
    (foreign-procedure "new_ssl_context"
                       (string string boolean)
                       void*))

  (define new-ssl-client
    (foreign-procedure "new_ssl_client"
                       (void* boolean)
                       void*))

  (define ssl-accept
    (foreign-procedure "ssl_accept"
                       (void*)
                       int))

  (define fill-input-buffer
    (foreign-procedure "fill_input_buffer"
                       (void* void* int)
                       int))

  (define drain-output-buffer
    (foreign-procedure "drain_output_buffer"
                       (void* void* int)
                       int))

  (define ssl-read
    (foreign-procedure "ssl_read"
                       (void* void* int)
                       int))

  (define ssl-write
    (foreign-procedure "ssl_write"
                       (void* void* int)
                       int))

  (define free-ssl-client
    (foreign-procedure "free_ssl_client"
                       (void*)
                       void))

  (define free-ssl-context
    (foreign-procedure "free_ssl_context"
                       (void*) void))

  (define (ssl-output-buffer client)
    (let* ([buf (make-ftype-pointer uv-buf (foreign-alloc (ftype-sizeof uv-buf)))]
           [base (get-buf)]
           [n (drain-output-buffer client base (* 64 1024))])
      (ftype-set! uv-buf (base) buf (make-ftype-pointer unsigned-8 base))
      (ftype-set! uv-buf (len) buf n)
      buf))

  (define ssl-connect
    (foreign-procedure "ssl_connect"
                       (void*)
                       int))

  (define DRAIN_OUTPUT_BUFFER -3)
  (define FILL_INPUT_BUFFER -2)

  (define (check-ssl client stream fn)
    (make-async
     (lambda (ok fail)
       (let lp ([n (fn)])
         (cond
          ((= 0 n) (fail "session closed"))
          ((or (= FILL_INPUT_BUFFER n)
               (= DRAIN_OUTPUT_BUFFER n))
           ;; OpenSSL seems to send back the wrong flag
           ;; so we'll just see if there's anything to output
           ;; if not then read something
           (let ([buf (ssl-output-buffer client)])
             (if (positive? (ftype-ref uv-buf (len) buf))
                 ((uv/stream-write stream buf)
                  (lambda (err ok)
                    (foreign-free (ftype-pointer-address buf)) ;; stream-write will release the base pointer
                    (if err (fail err)
                        (lp (fn)))))
                 (begin
                   (free-uv-buf buf)
                   (uv/stream-read-raw stream
                                       (lambda (nb buf)
                                         (if nb
                                             (let ([n (fill-input-buffer client (ftype-pointer-address (ftype-ref uv-buf (base) buf)) nb)])
                                               (free-buf (ftype-pointer-address (ftype-ref uv-buf (base) buf)))
                                               (if (= n nb)
                                                   (lp (fn))
                                                   (fail n)))
                                             (fail #f))))))))
           ((= -1 n) (fail n))
           ((> n 0) (ok n))
           (else
            (error 'idk "shouldn't get here!")))))))

  (define ssl-shutdown
    (foreign-procedure "ssl_shutdown"
                       (void*)
                       int))

  (define (tls-shutdown tls stream)
    (make-async
     (lambda (ok fail)
       ((check-ssl tls stream (lambda ()
                                (ssl-shutdown tls)))
        (lambda (err v)
          (if err (fail err)
              (ok v)))))))

  (define (make-tls-writer client stream)
    (lambda (buf)
      (make-async
       (lambda (ok fail)
         (let ([buf (if (string? buf) (string->buf buf) buf)])
           ((check-ssl client stream (lambda ()
                                       (ssl-write client
                                                  (ftype-pointer-address (ftype-ref uv-buf (base) buf))
                                                  (ftype-ref uv-buf (len) buf))))
            (lambda (err v)
              (ok v))))))))

  (define (make-tls-reader client stream)
    (uv/make-reader stream
                    (lambda (on-read)
                      (let ([buf (get-buf)])
                        ((check-ssl client stream
                                    (lambda ()
                                      (ssl-read client buf buf-size)))
                         (lambda (err n)
                           (if n
                               (let ([bv (make-bytevector n)])
                                 (memcpy bv buf n)
                                 (free-buf buf)
                                 (on-read stream bv))
                               (on-read #f #f))))))))

  (define (tls-connect ctx stream)
    (let ([client (new-ssl-client ctx #t)])
      (make-async
       (lambda (ok fail)
         ((check-ssl client stream (lambda () (ssl-connect client)))
          (lambda (err val)
            (if err (fail err)
                (ok (list client
                          (make-tls-reader client stream)
                          (make-tls-writer client stream))))))))))

  (define (tls-accept ctx stream)
    (let ([client (new-ssl-client ctx #f)])
      (make-async
       (lambda (ok fail)
         ((check-ssl client stream (lambda () (ssl-accept client)))
          (lambda (err val)
            (if err (fail err)
                (ok (list client
                          (make-tls-reader client stream)
                          (make-tls-writer client stream))))))))))


  (define (uv/static-file-handler path)
    (lambda (err ok)
      #f
      ))

  (define (uv/write-http-request writer req)
    (writer "HTTP/1.1 200 OK\r\nVia: ChezScheme\r\nContent-Length: 0\r\n\r\n"))

  (define (keep-alive? req)
    (let ([version (string=? "HTTP/1.1" (caddar req))]
          [conn (header-value (cadr req) "Connection")])
      (not (and (not version) conn (string=? "close" conn)))))

  (define (uv/call-with-ssl-context cert key client? fn)
    (let ([ctx (new-ssl-context cert key client?)])
        (fn ctx
            (lambda (err ok)
              (free-ssl-context ctx)))))

  (define (serve-http reader writer on-done)
    ((uv/read-http-response reader)
     (lambda (err req)
       (if err
           (on-done err req)
           ((uv/write-http-request writer req)
            (lambda (err status)
              (if (and (not err) (keep-alive? req))
                  (serve-http reader writer on-done)
                  (on-done err status))))))))

  (define (uv/serve-https ctx stream on-done)
    ((tls-accept ctx stream)
     (lambda (err tls)
       (serve-http (cadr tls) (caddr tls)
                   (lambda (err ok)
                     (free-ssl-client (car tls))
                     (on-done err ok))))))

  (define (partial fn x)
    (lambda (args ...)
      (apply fn x args)))

  (define (uv/serve-http stream on-done)
    (serve-http (uv/make-reader stream (partial uv/stream-read stream))
                (partial uv/stream-write stream)
                on-done))

  (define close-tls-client
    (lambda (client)
      (make-async
       (lambda (ok fail)
         (free-ssl-client client)
         (ok #t)))))

  (define (uv/make-https-request loop ctx url on-done)
    ((async-do
      (<- addr (uv/getaddrinfo loop (uv/url-host url)))
      (<- stream (uv/tcp-connect loop (addr->sockaddr addr (uv/url-port url))))
      (<- client (tls-connect ctx stream))
      (<- status ((caddr client) (format #f "GET ~a HTTP/1.1\r\nHost: ~a\r\nConnection: close\r\n\r\n" (uv/url-path url)
                                         (uv/url-host url))))
      (<- resp (uv/read-http-response (cadr client)))
      (<- done (tls-shutdown (car client) stream))
      (handle-close stream nothing)
      (free-ssl-client (car client))
      (async-return resp))
     on-done))

  (define (uv/make-http-request loop url on-done)
    ((async-do
      (<- addr (uv/getaddrinfo loop (uv/url-host url)))
      (<- stream (uv/tcp-connect loop (addr->sockaddr addr (uv/url-port url))))
      (<- status (uv/stream-write stream (format #f "GET ~a HTTP/1.1\r\nHost: ~a\r\nConnection: close\r\n" (uv/url-path url)
                                                 (uv/url-host url))))
      (<- resp (uv/read-http-response (uv/make-reader stream uv/stream-read)))
      (uv/close-stream stream)
      (async-return resp))
     on-done)))

