;; -*- geiser-scheme-implementation: chez -*-
(library (uv)
  (export uv/call-with-loop
          uv/string->url
          uv/url-host
          uv/url-protocol
          uv/url-path
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
          (utils)
          (log)
          (libuv)
          (openssl)
          (alloc)
          (irregex))

  (define <- #f) ;; dummy else it won't compile in the importing module
  (define-syntax let/async
    (syntax-rules (<-)
      ((_ () body ...)
       (let () body ...))

      ((_ ((name (<- value)) next ...) body ...)
       (value (lambda (name)
                (let/async (next ...)
                           body ...))))
      ((_ ((name value) next ...) body ...)
       (let ((name value))
         (let/async (next ...)
                    body ...)))))

  (define-record-type http-request
    (fields headers body method url))

  (define-record-type http-response
    (fields headers body status version code))

  (define-record-type url
    (fields protocol host path port))

  (define (nothing . args)
    #f)

  (define uv-error? positive?)

  (define-condition-type &uv/error &condition make-uv-error uv/error?
    (code uv-error-code)
    (message uv-error-message))

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

 (define (uv/read-lines reader on-line)
   (reader (make-bytevector 4096) 0 'eol
           (lambda (bv num-read)
             (info "read-lines callback: len: ~a" num-read)
             (if (and bv (on-line (trim-newline! bv num-read)))
                 (begin
                   (uv/read-lines reader on-line))
                 (begin
                   (on-line #f))))))

 (define (read-headers reader done)
   (let ([lines '()])
     (uv/read-lines reader
                 (lambda (line)
                   (info "read-headers line callback: ~a" (if line (utf8->string line) #f))
                   (info "lines: ~a" (map utf8->string lines))
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
   (let ([status (string-split (utf8->string status-line) #\space)])
     (list (car status) (string->number (cadr status)) (caddr status))))

  (define (uv/close loop)
    (uv-stop loop)
    (close-all-handles loop))

  (define (uv/stop loop)
    (uv-stop loop))

  (define uv/call-with-loop
    (lambda (f)
      (let ([l #f])
        (set! l (uvloop-create))
        (register-signal-handlers l)
        (f l)
        (uv-run l 0)
        (uv-stop l))))

  (define uv/getaddrinfo
    (lambda (loop name)
       (lambda (k)
         (letrec ([hint (make-ftype-pointer addrinfo (alloc-zero (ftype-sizeof addrinfo)))]
                  [req (make-req UV_GETADDRINFO)]
                  [code (foreign-callable
                         (lambda (req status addr)
                           (foreign-free req)
                           (foreign-free (ftype-pointer-address hint))
                           (unlock-object code)
                           (if (= 0 status)
                               (k addr)
                               (begin
                                 (uv-freeaddrinfo addr)
                                 (error 'getaddrinfo "invalid status" status)))
                           (uv-freeaddrinfo addr))
                         (void* int (* addrinfo))
                         void)])
           (lock-object code)
           (ftype-set! addrinfo (ai_family) hint AF_INET)
           (ftype-set! addrinfo (ai_socktype) hint SOCK_STREAM)
           (check (uv-getaddrinfo loop req (foreign-callable-entry-point code)
                                  name #f hint))))))

  (define (uv/tcp-connect loop addr)
    (lambda (k)
      (letrec* ([conn (make-handler UV_STREAM)]
                [socket (make-handler UV_TCP)]
                [code (foreign-callable
                       (lambda (conn status)
                         (unlock-object code)
                         (if (= 0 status)
                             (k socket)
                             (error 'tcp-connect "error in connect" status (uv-err-name status))))
                       (void* int)
                       void)])
        (lock-object code)
        (check (uv-tcp-init loop socket))
        (check (uv-tcp-connect conn socket addr
                               (foreign-callable-entry-point code))))))


  (define (uv/make-idle loop)
    (lambda (k)
      (letrec* ([idle (make-handler UV_IDLE)]
                [code (foreign-callable (lambda (ll)
                                          (if (k)
                                              #t
                                              (begin
                                                (unlock-object code)
                                                (uv-idle-stop idle)
                                                (foreign-free idle))))
                                        (void*)
                                        void)])
        (check (uv-idle-init loop idle))
        (lock-object code)
        (check (uv-idle-start idle (foreign-callable-entry-point code))))))

  (define (uv/stream-write stream s)
     (lambda (k)
       (letrec ([buf (if (string? s) (string->buf s) s)]
                [write-req (make-req UV_WRITE)]
                [code (foreign-callable
                       (lambda (req status)
                         (free-buf (ftype-pointer-address (ftype-ref uv-buf (base) buf)))
                         (foreign-free write-req)
                         (unlock-object code)
                         (if (= 0 status)
                             (k status)
                             (error 'stream-write "uv/stream-write" status (uv-err-name status))))
                       (void* int)
                       void)])
         (lock-object code)
         (check (uv-write write-req stream buf 1 (foreign-callable-entry-point code))))))

  (define (uv/stream-read-raw stream cb)
    (letrec ([code (foreign-callable
                    (lambda (s nb buf)
                      (unlock-object code)
                      (check (uv-read-stop stream))
                      (if (negative? nb)
                          (begin
                            (free-buf (ftype-pointer-address (ftype-ref uv-buf (base) buf)))
                            (cb #f #f))
                          (cb nb buf)))
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
                (on-read #f #f)))
          (let ([len (if (<= buf-len (+ read-pos len))
                         (- buf-len read-pos)
                         len)])
            (bytevector-copy! buf read-pos bv start len)
            (set! read-pos (+ read-pos len))
            (when (>= read-pos buf-len)
              (info "read-pos >= buf-len")
              (set! buf #f))
            (on-read bv len))))
    (lambda (bv start len on-read)
      (if buf
          (begin
            (info "sending old buf")
            (send-buf bv start len on-read))
          (begin
            (info "no buf sending read request...")
            (reader (lambda (s b)
                     (if s
                         (begin
                           (set! buf b)
                           (set! read-pos 0)
                           (set! buf-len (bytevector-length b))
                           (send-buf bv start len on-read))
                         (on-read #f #f))))))))


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
    (lambda (k)
      (read-headers reader
                    (lambda (headers)
                      (info "response headers: ~a" headers)
                      (if (pair? headers)
                          (let* ([status (parse-status (car headers))]
                                 [headers (parse-headers (cdr headers))]
                                 [content-length (header->number headers "Content-Length")])
                            (info "status: ~a" status)
                            (info "headers: ~a" headers)
                            (info "content-length: ~a" content-length)
                            (if content-length
                                (begin
                                  (info "reading full content length")
                                  (uv/read-fully reader content-length
                                                 (lambda (body)
                                                   (k (list status headers body)))))
                                (k (list status headers #t))))
                          (error 'eof "read to end of line"))))))

  (define (uv/read-http-request reader)
    (lambda (k)
      (read-headers reader
                    (lambda (headers)
                      (if (pair? headers)
                          (let* ([status (parse-status (car headers))]
                                 [headers (parse-headers (cdr headers))]
                                 [content-length (header->number headers "Content-Length")])
                            (if content-length
                                (begin
                                  (if (= 0 content-length)
                                      (k (list status headers #f))
                                      (uv/read-fully reader content-length
                                                     (lambda (body)
                                                       (info "read-full body: ~a" body)
                                                       (k (list status headers body))))))
                                (k (list status headers #t))))
                          (error 'eof "read to end of line"))))))


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
      (if (= 0 r)
          s
          (begin
            (foreign-free (ftype-pointer-address s))
            (error 'ipv4->sockaddr "error in making ipv4" r)))))

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

  (define (ssl-output-buffer client)
    (let* ([buf (make-ftype-pointer uv-buf (foreign-alloc (ftype-sizeof uv-buf)))]
           [base (get-buf)]
           [n (ssl/drain-output-buffer client base buf-size)])
      (ftype-set! uv-buf (base) buf (make-ftype-pointer unsigned-8 base))
      (ftype-set! uv-buf (len) buf n)
      buf))

  (define (check-ssl client stream fn)
    (lambda (k)
      (let lp ([n (fn)])
        (if (ssl/error? n)
            (let ([e (ssl/get-error client n)])
              (cond
               ((= e ssl-error-none) (k 0))
               ((or (= e ssl-error-want-write)
                    (= e ssl-error-want-read))
                (let ([buf (ssl-output-buffer client)])
                  (if (positive? (ftype-ref uv-buf (len) buf))
                      ((uv/stream-write stream buf)
                       (lambda (k)
                         (foreign-free (ftype-pointer-address buf)) ;; stream-write will release the base pointer
                         (lp (fn))))
                      (uv/stream-read-raw stream
                                          (lambda (nb buf)
                                            (if nb
                                                (let ([n (ssl/fill-input-buffer client (ftype-pointer-address (ftype-ref uv-buf (base) buf)) nb)])
                                                  (free-buf (ftype-pointer-address (ftype-ref uv-buf (base) buf)))
                                                  (if (= n nb)
                                                      (lp (fn))
                                                      (error 'check-ssl "probably need to loop but will be annoying " n)))
                                                (error 'check-ssl "failed to read bytes" nb)))))))
               (else (raise (ssl/library-error)))))
            (k n)))))

  (define (tls-shutdown tls stream)
    (lambda (k)
      ((check-ssl tls stream (lambda () (ssl/shutdown tls))) k)))

  (define (make-tls-writer client stream
)
    (lambda (buf)
      (lambda (k)
        (let ([buf (if (string? buf) (string->buf buf) buf)])
          ((check-ssl client stream (lambda ()
                                      (ssl/write client
                                                 (ftype-pointer-address (ftype-ref uv-buf (base) buf))
                                                 (ftype-ref uv-buf (len) buf))))
           k)))))

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
                             (info "tls-reader: ~a" (utf8->string bv))
                             (on-read stream bv))))))))

  (define (tls-connect ctx stream)
    (let ([client (ssl/make-stream ctx #t)])
      (lambda (k)
        ((check-ssl client stream (lambda ()
                                    (ssl/connect client)))
         (lambda (val)
           (k (list client
                    (make-tls-reader client stream)
                    (make-tls-writer client stream))))))))

  (define (tls-accept ctx stream)
    (let ([client (ssl/make-stream ctx #f)])
      (lambda (k)
        ((check-ssl client stream (lambda ()
                                    (ssl/accept client)))
         (lambda (val)
           (k (list client
                    (make-tls-reader client stream)
                    (make-tls-writer client stream))))))))

  (define (uv/write-http-response writer req)
    (info "about to write")
    (writer "HTTP/1.1 200 OK\r\nVia: ChezScheme\r\nContent-Length: 0\r\n\r\n"))

  (define (keep-alive? req)
    (let ([version (string=? "HTTP/1.1" (caddar req))]
          [conn (header-value (cadr req) "Connection")])
      (if version
          (not (and conn (string=? "close" conn)))
          #f)))

  (define (serve-http reader writer on-done)
    (let/async ([req (<- (uv/read-http-request reader))]
                [_ (info "serve-http after read")]
                [status (<- (uv/write-http-response writer req))])
               (info "write http request, sent ~a bytes" status)
               (if (keep-alive? req)
                   (begin
                     (serve-http reader writer on-done))
                   (begin
                     (on-done status)))))

  (define (uv/serve-https ctx stream on-done)
    ((tls-accept ctx stream)
     (lambda (tls)
       (serve-http (cadr tls) (caddr tls)
                   (lambda (ok)
                     (ssl/free-stream (car tls))
                     (on-done ok))))))

  (define (uv/serve-http stream on-done)
    (serve-http (uv/make-reader stream (lambda (b) (uv/stream-read stream b)))
                (lambda (s) (uv/stream-write stream s))
                on-done))

  (define close-tls-client
    (lambda (client)
      (lambda (k)
        (ssl/free-stream client)
        (k #t))))

  (define (uv/listen-and-serve-http loop addr)
    (lambda (k)
      (uv/tcp-listen loop addr
                     (lambda (status server client)
                       (if (not status)
                           (error 'listen-and-serve-http "failed to accept" status)
                           (uv/serve-http client
                                          (lambda (status)
                                            (uv/close-stream client))))))))

  (define (uv/make-http-request loop url)
    (lambda (k)
      (let/async ([addr (<- (uv/getaddrinfo loop (uv/url-host url)))]
                  [sa (addr->sockaddr addr (uv/url-port url))]
                  [sock (<- (uv/tcp-connect loop sa))]
                  [status (<- (uv/stream-write sock (format #f "GET / HTTP/1.1\r\nHost: ~a\r\nConnection: close\r\n\r\n" (uv/url-host url))))]
                  [resp (<- (uv/read-http-response (uv/make-reader sock (lambda (b) (uv/stream-read sock b)))))])
                 (k resp))))

  (define (uv/call loop f)
    (letrec ([a (make-handler UV_ASYNC)]
             [code (foreign-callable
                    (lambda (h)
                      (unlock-object code)
                      (handle-close a nothing)
                      (f))
                    (void*)
                    void)])
      (lock-object code)
      (check (uv-async-init loop a (foreign-callable-entry-point code)))
      (check (uv-async-send a))))

  (define (uv/call-after loop timeout repeat cb)
    (letrec ([a (make-handler UV_TIMER)]
             [code (foreign-callable
                    (lambda (h)
                      (unless (cb)
                        (unlock-object code)
                        (handle-close a nothing)))
                    (void*)
                    void)])
      (lock-object code)
      (check (uv-timer-init loop a))
      (check (uv-timer-start a (foreign-callable-entry-point code) timeout repeat))))

  (define (uv/make-https-request loop ctx url)
    (lambda (k)
      (let/async ([addr (<- (uv/getaddrinfo loop (uv/url-host url)))]
                  [stream (<- (uv/tcp-connect loop (addr->sockaddr addr (uv/url-port url))))]
                  [_ (info "https-request tcp connected")]
                  [client (<- (tls-connect ctx stream))]
                  [_ (info "after tls-connect")]
                  [status (<- ((caddr client) (format #f "GET ~a HTTP/1.1\r\nHost: ~a\r\nConnection: close\r\nContent-Length: 0\r\n\r\n" (uv/url-path url)
                                                   (uv/url-host url))))]
                  [_ (info "issuing http request ~a" status)]
                  [resp (<- (uv/read-http-response (cadr client)))]
                  [_ (info "resp = ~a" resp)]
                  [done (<- (tls-shutdown (car client) stream))])
                 (info "done https request")
                 (k resp)))))

