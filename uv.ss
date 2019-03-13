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
          (hpack)
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

      ((_ (((name ...) (<- value)) next ...) body ...)
       (value (lambda (name ...)
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
             (if (and bv (on-line (trim-newline! bv num-read)))
                 (begin
                   (uv/read-lines reader on-line))
                 (begin
                   (on-line #f))))))

 (define (read-headers reader done)
   (let ([lines '()])
     (uv/read-lines reader
                 (lambda (line)
                   ;; (info "read-headers line callback: ~a" (if line (utf8->string line) #f))
                   (if (or (not line) (= 0 (bytevector-length line)))
                       (begin
                         (if line
                             (begin
                               (done (reverse lines))))
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

  (define (ssl-drain client stream k)
    (if (positive? (ssl/num-bytes client))
        (let ([buf (ssl-output-buffer client)])
          ((uv/stream-write stream buf)
           (lambda (n)
             (foreign-free (ftype-pointer-address buf)) ;; stream-write will release the base pointer
             (k))))
        (k)))

  (define (ssl-fill client stream k)
    (uv/stream-read-raw stream
                        (lambda (nb buf)
                          (if nb
                              (let ([n (ssl/fill-input-buffer client (ftype-pointer-address (ftype-ref uv-buf (base) buf)) nb)])
                                (free-buf (ftype-pointer-address (ftype-ref uv-buf (base) buf)))
                                (if (= n nb)
                                    (k)
                                    (error 'check-ssl "probably need to loop but will be annoying " n))) ;; TODO: fix this I think at 16k
                              (error 'check-ssl "failed to read bytes" nb)))))

  (define (flush-ssl client stream k)
    (ssl-drain client stream (lambda () (ssl-fill client stream k))))

  (define (tls-shutdown tls stream)
    (lambda (k)
      ((check-ssl tls stream (lambda () (ssl/shutdown tls))) k)))

  (define (make-tls-writer client stream)
    (lambda (buf)
      (lambda (k)
        (let ([buf (if (string? buf) (string->buf buf) buf)])
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
        ((check-ssl client stream
                    (lambda ()
                      (ssl/connect client)))
         (lambda (n)
           (k (list client
                    (make-tls-reader client stream)
                    (make-tls-writer client stream))))))))

  (define (tls-accept ctx stream)
    (let ([client (ssl/make-stream ctx #f)])
      (lambda (k)
        ((check-ssl client stream
                    (lambda ()
                      (ssl/accept client)))
         (lambda (n)
           ((check-ssl client stream
                       (lambda ()
                         (ssl/do-handshake client)))
            (lambda (n)
              (k (list client
                       (make-tls-reader client stream)
                       (make-tls-writer client stream))))))))))

  (define (uv/write-http-response writer req)
    (writer "HTTP/1.1 200 OK\r\nVia: ChezScheme\r\nContent-Length: 0\r\n\r\n"))

  (define (keep-alive? req)
    (let ([version (string=? "HTTP/1.1" (caddar req))]
          [conn (header-value (cadr req) "Connection")])
      (if version
          (not (and conn (string=? "close" conn)))
          #f)))

  (define (serve-http reader writer on-done)
    (let/async ([req (<- (uv/read-http-request reader))]
                [status (<- (uv/write-http-response writer req))])
               (if (keep-alive? req)
                   (begin
                     (serve-http reader writer on-done))
                   (begin
                     (on-done status)))))

  (define h2-frame-premamble-size (+ 3 1 1 4))

  (define (bytevector->uv-buf bv)
    (let ([buf (make-ftype-pointer uv-buf (foreign-alloc (ftype-sizeof uv-buf)))]
          [b (get-buf)])
      (memcpy2 b bv (bytevector-length bv))
      (ftype-set! uv-buf (base) buf (make-ftype-pointer unsigned-8 b))
      (ftype-set! uv-buf (len) buf (bytevector-length bv))
      buf))

  (define memcpy2
    (foreign-procedure "memcpy"
                       (void* u8* int)
                       void*))

  (define h2-preface "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n")

  (define h2-frame-type-data 0)
  (define h2-frame-type-headers 1)
  (define h2-frame-type-priority 2)
  (define h2-frame-type-rst-stream 3)
  (define h2-frame-type-settings 4)
  (define h2-frame-type-push-promise 5)
  (define h2-frame-type-ping 6)
  (define h2-frame-type-goaway 7)
  (define h2-frame-type-window-update 8)
  (define h2-frame-type-continuation 9)

  (define h2-settings-table-size 1)
  (define h2-settings-enable-push 2)
  (define h2-settings-max-concurrent-streams 3)
  (define h2-settings-initial-window-size 4)
  (define h2-settings-max-frame-size 5)
  (define h2-settings-max-header-list-size 6)

  (define h2-error-no-error 0)
  (define h2-error-protocol-error 1)
  (define h2-error-internal-error 2)
  (define h2-error-flow-control-error 3)
  (define h2-error-settings-timeout 4)
  (define h2-error-stream-closed 5)
  (define h2-error-frame-size-error 6)
  (define h2-error-refused-stream 7)
  (define h2-error-cancel 8)
  (define h2-error-compression-error 9)
  (define h2-error-connect-error 10)
  (define h2-error-enhance-your-calm 11)
  (define h2-error-inadequate-security 12)
  (define h2-error-http/1.1-required 13)


  (define (make-frame type flags id payload)
    (let* ([payload-len (if payload (bytevector-length payload) 0)]
           [buf (make-bytevector (+ payload-len h2-frame-premamble-size))])
      (bytevector-u8-set! buf 0 (logand (fxsra payload-len 16) 255))
      (bytevector-u8-set! buf 1 (logand (fxsra payload-len 8) 255))
      (bytevector-u8-set! buf 2 (logand payload-len 255))
      (bytevector-u8-set! buf 3 type )
      (bytevector-u8-set! buf 4 flags)
      (bytevector-u32-set! buf 5 id 'big)
      (when (positive? payload-len)
        (bytevector-copy! payload 0 buf h2-frame-premamble-size payload-len))
      buf))

  (define write-frame
    (lambda (writer type flags id payload)
      (lambda (k)
        ((writer (bytevector->uv-buf (make-frame type flags id payload))) k))))

  (define-record-type h2-frame
    (fields type flags id payload))

  (define read-frame
    (lambda (reader)
      (lambda (k)
        (info "reading")
        (reader (make-bytevector h2-frame-premamble-size) 0 h2-frame-premamble-size
                (lambda (bv num-read)
                  (info "num-read: ~a" num-read)
                  (info "preamble size: ~a" h2-frame-premamble-size)
                  (unless (= num-read h2-frame-premamble-size)
                    (error 'read-frame "not enough data"))
                  (let ([len (bytevector-u24-ref bv 0 'big)]
                        [type (bytevector-u8-ref bv 3)]
                        [flags (bytevector-u8-ref bv 4)]
                        [id (bytevector-u32-ref bv 5 'big)])
                    (info "frame len: ~a" len)
                    (if (zero? len)
                        (k (make-h2-frame type flags id (make-bytevector 0)))
                        (reader (make-bytevector len) 0 len
                                (lambda (bv num-read)
                                  (unless (= num-read len)
                                    (error 'read-frame "error reading frame payload"))
                                  (k (make-h2-frame type flags id bv)))))))))))

  (define (async-reader reader)
    (lambda (len)
      (lambda (k)
        (reader (make-bytevector len) 0 len
                (lambda (bv num-read)
                  (k bv num-read))))))

  (define (h2-check-preface reader)
    (lambda (k)
      (reader (make-bytevector (string-length h2-preface)) 0 (string-length h2-preface)
              (lambda (bv num-read)
                (if (and (= num-read (string-length h2-preface)) (string=? (utf8->string bv)))
                    (k #t)
                    (error 'check-preface "invalid preface string" bv))))))

  (define (h2-default-settings)
    '((h2-settings-table-size 4096)
      (h2-settings-enable-push #t)
      (h2-settings-max-concurrent-streams +inf.0)
      (h2-settings-initial-window-size 65535)
      (h2-settings-max-frame-size 16384)
      (h2-settings-max-header-list-size +inf.0)))

  (define-condition-type &h2-error &condition make-h2-error h2-error?
    (code h2-error-code)
    (message h2-error-message))

  (define h2-settings-id->symbol
    (lambda (x)
      (cond
       ((= x h2-settings-table-size) 'h2-settings-table-size)
       ((= x h2-settings-enable-push) 'h2-settings-enable-push)
       ((= x h2-settings-max-concurrent-streams) 'h2-settings-max-concurrent-streams)
       ((= x h2-settings-initial-window-size) 'h2-settings-initial-window-size)
       ((= x h2-settings-max-frame-size) 'h2-settings-max-frame-size)
       ((= x h2-settings-max-header-list-size) 'h2-settings-max-header-list-size)
       (else (error 'h2-settings-id->symbol "invalid id" x)))))

  (define (read-settings-frame frame)
    (let ([p (h2-frame-payload frame)])
      (unless (zero? (modulo (bytevector-length p) 6))
        (raise (make-h2-error h2-error-frame-size-error "settings not divisble by 6")))
      (let lp ([i 0]
               [s '()])
        (if (>= i (bytevector-length p))
            s
            (let ([id (h2-settings-id->symbol (bytevector-u16-ref p i 'big))]
                  [value (bytevector-u32-ref p (+ i 2) 'big)])
              (lp (+ i 6) (cons (list id value) s)))))))

  (define (merge-alist old new)
    (fold-left
     (lambda (acc item)
       (let ([other (assq (car item) new)])
         (if other
             (cons (list (car item) (cadr other)) acc)
             (cons item acc))))
     '() old))

  (define h2-settings-ack 1)

  (define (write-h2-ack-settings writer)
    (write-frame writer h2-frame-type-settings h2-settings-ack 0 #f))

  (define h2-ping-ack 1)
  (define h2-header-flag-end-stream 1)
  (define h2-header-flag-end-headers 4)
  (define h2-header-flag-padded 8)
  (define h2-header-flag-priority 32)

  (define-record-type h2-header-frame
    (fields pad-length exclusive? stream-dependency weight headers))

  (define (h2-header-flag? frame . values)
    (let ([v (apply fxlogor values)])
      (= v (fxlogand v (h2-frame-flags frame)))))

  (define (append-bytevectors . bvs)
    (let* ([total-size (fold-left (lambda (acc n)
                                    (+ acc (bytevector-length n))) 0 bvs)]
           [out (make-bytevector total-size)])
      (let lp ([i 0]
               [b bvs])
        (if (pair? b)
            (begin
              (bytevector-copy! (car b) 0 out i (bytevector-length (car b)))
              (lp (+ i (bytevector-length (car b))) (cdr b)))
            out))))

  (define (read-h2-header-frame stream frame . payloads)
    (let ([p (h2-frame-payload frame)]
          [pad-length #f]
          [e #f ]
          [stream-dep #f]
          [weight #f]
          [i  0])
      (when (h2-header-flag? frame h2-header-flag-padded)
        (set! pad-length (bytevector-u8-ref p 0))
        (set! i 1))
      (when (h2-header-flag? frame h2-header-flag-priority)
        (set! e (logbit? 31 (bytevector-u32-ref p i 'big)))
        (set! stream-dep (logand (bytevector-u32-ref p i 'big) #x7fffffff))
        (set! weight (bytevector-u8-ref p (+ i 4)))
        (set! i (+ i 4 1)))
      (info "HEADER FRAME")
      (info "pad-length: ~a" pad-length)
      (info "flags: ~a" (h2-frame-flags frame))
      (info "payload is: ~a" p)
      (info "e: ~a" e)
      (info "stream-dep: ~a" stream-dep)
      (info "weight: ~a" weight)
      (let* ([total-payload (apply append-bytevectors p payloads)]
             [h (hpack/decode total-payload i (h2-stream-in-header-table stream))])
        (info "decoded headers: ~a" h)
        h)))

  (define (process-h2-request stream)
    #f
    )

  (define (h2-stream-push-frame! stream frame)
    (h2-stream-in-frames-set! stream (cons frame (h2-stream-in-frames stream))))

  (define (h2-stream-state=? stream state)
    (eq? (h2-stream-state stream) state))

  (define (h2-stream-assemble-headers! stream)
    (h2-stream-headers-set! stream
                            (let lp ([fragments '()]
                                     [frames (h2-stream-in-frames stream)])
                              (if (null? frames)
                                  (error 'idk "shouldn't get to the end really")
                                  (cond
                                   ((= h2-frame-type-continuation (h2-frame-type (car frames)))
                                    (lp (cons (h2-frame-payload (car frames)) fragments) (cdr frames)))
                                   ((= h2-frame-type-headers (h2-frame-type (car frames)))
                                    (apply read-h2-header-frame stream (car frames) (reverse fragments)))
                                   (else (lp fragments (cdr frames))))))))

  (define-record-type h2-session
    (fields (mutable settings)
            (mutable last-used-id)
            (mutable streams)
            reader writer
            (mutable goaway)
            (mutable window-size)))

  (define (new-h2-session r w)
    (make-h2-session (h2-default-settings) 0 '() r w #f (cadr (assoc 'h2-settings-initial-window-size (h2-default-settings)))))

  (define-record-type h2-stream
    (fields id
            (mutable state)
            (mutable priority)
            (mutable in-frames)
            (mutable out-frames)
            in-header-table
            out-header-table
            (mutable window-size)
            (mutable headers)
            (mutable data-frames)))

  (define (h2-session-stream-add! session stream-id)
    (info "making new session: ~a" stream-id)
    (if (>= (h2-session-last-used-id session) stream-id)
        (error 'idk stream-id)
        (begin
          (let ([stream (make-h2-stream stream-id
                                        'idle
                                        0
                                        '()
                                        '()
                                        (hpack/make-dynamic-table 4096)
                                        (hpack/make-dynamic-table 4096)
                                        (h2-session-setting-value session 'h2-settings-initial-window-size)
                                        #f
                                        #f)])
           (h2-session-streams-set! session (cons (list stream-id stream) (h2-session-streams session)))
           stream))))

  (define (h2-session-setting-value session setting)
    (let ([e (assoc setting (h2-session-settings session))])
      (if (pair? e)
          (cadr e)
          (error 'h2-session-settings "invalid setting" setting))))

  (define (h2-session-find-stream session id)
    (let ([e (assoc id (h2-session-streams session))])
      (if e
          (cadr e)
          #f)))

  (define-syntax else-map
    (syntax-rules ()
      ((_ a b)
       (let ([aa a])
         (if aa
             aa
             b)))))

  (define (h2-stream-valid-for-continuation? stream)
    (let ([frames (h2-stream-in-frames stream)])
      (if (null? frames)
          #f
          (let* ([last-frame (car frames)]
                 [type (h2-frame-type (car frames))])
            (cond
             ((and (or (eq? h2-frame-type-headers type)
                       (eq? h2-frame-type-priority type)
                       (eq? h2-frame-type-continuation type))
                   (not (fxlogbit? 2 (h2-frame-flags last-frame))))
              #t)
             (else #f))))))

  (define (serve-http2 reader writer on-done)
    (define session (new-h2-session reader writer))
    (info "serve-http2")
    (let/async ([_ (<- (h2-check-preface reader))] ;; check preface for http2
                [_ (<- (write-frame writer h2-frame-type-settings 0 0 #f))])
      (info "http2 connection established")
      (let lp ()
        (let/async ([frame (<- (read-frame reader))]
                    [type (h2-frame-type frame)]
                    [flags (h2-frame-flags frame)])
          (info "flags: ~8,'0b" flags)
          (info "type: ~a" type)
          (cond
           ((= h2-frame-type-goaway type) (begin
                                            (info "go away")
                                            (on-done #t)))
           ((= h2-frame-type-ping type)
            (info "ping")
            (cond
             ((not (= 0 (h2-frame-id frame))) (raise (make-h2-error h2-error-protocol-error "invalid stream id for ping")))
             ((not (= 8 (bytevector-length (h2-frame-payload frame))))
              (raise (make-h2-error h2-error-frame-size-error "invalid size for ping frame")))
             (else
              (begin
                ((writer (bytevector->uv-buf (make-frame h2-frame-type-ping h2-ping-ack 0 (h2-frame-payload frame))))
                 (lambda (n)
                   (info "wrote ping ack") #f))))))
           ((= h2-frame-type-settings type)
            (info "settings")
            (begin
              (if (h2-header-flag? frame h2-settings-ack)
                  (begin
                    #f)
                  (begin
                    (h2-session-settings-set! session (merge-alist (h2-session-settings session) (read-settings-frame frame)))
                    ((write-h2-ack-settings writer) (lambda (n)
                                                      #f))))))
           ((= h2-frame-type-continuation type)
            (begin
              (info "continuation")
              (let ([stream (h2-session-find-stream session (h2-frame-id frame))])
                (unless (h2-stream-valid-for-continuation? stream)
                  (raise (make-h2-error h2-error-protocol-error "last frame was not a valid frame for a continuation")))
                (h2-stream-push-frame! stream frame)
                (when (h2-header-flag? frame 4)
                  (h2-stream-assemble-headers! frame))
                (when (h2-header-flag? frame 1)
                  (h2-stream-state-set! stream 'half-closed-remote)
                  (process-h2-request stream)))))
           ((= h2-frame-type-data type)
            (begin
              (info "data")
              ;; TODO: padding verification
              (let ([stream (h2-session-find-stream session (h2-frame-id frame))])
                (cond
                 ((= 0 (h2-frame-id frame))
                  (raise (make-h2-error h2-error-protocol-error "stream id cannot be 0 for data frames")))
                 ((not stream) (make-h2-error h2-error-protocol-error "no stream found for provided stream id")))
              (if (or (eq? 'open (h2-stream-state stream)) (eq? 'half-closed-local (h2-stream-state stream)))
                  (h2-stream-push-frame! stream frame)
                  (raise (make-h2-error h2-error-protocol-error "wrong stream state")))
              (when (fxlogbit? 1 flags)
                ;; end-stream
                (h2-stream-state-set! stream 'half-closed-remote)
                (process-h2-request stream)))))
           ((= h2-frame-type-headers type)
            (begin
              (info "headers")
              (let ([stream (else-map (h2-session-find-stream session (h2-frame-id frame)) (h2-session-stream-add! session (h2-frame-id frame)))])
                (unless (h2-stream-state=? stream 'idle)
                  (raise (make-h2-error h2-error-protocol-error "invalid stream state for headers frame")))
                (h2-stream-state-set! stream 'open)
               (cond
                ((not (h2-header-flag? frame h2-header-flag-end-headers))
                 (begin
                   ;; should get continuations
                   (h2-stream-push-frame! stream frame)))
                ((h2-header-flag? frame h2-header-flag-end-headers h2-header-flag-end-stream)
                 (begin
                   (h2-stream-state-set! stream 'half-closed-remote)
                   (let ([headers (read-h2-header-frame stream frame )])
                     (h2-stream-headers-set! stream headers)
                     (process-h2-request stream))))
                ((h2-header-flag? frame h2-header-flag-end-headers)
                 (let ([headers (read-h2-header-frame stream frame )])
                   (h2-stream-headers-set! stream headers)))
                (else
                 (error 'oopsie "shouldn't get here"))))))
           ((= h2-frame-type-window-update type)
            (begin
              (info "window update")
              (let ([size (bytevector-u32-ref (h2-frame-payload frame) 0 'big)])
                ;; (info "flags: ~a, stream-id: ~a" (h2-frame-flags frame) (h2-frame-id frame))
                ;; (info "window-size: ~a" size)
                #f
                )))
           (else (info "didn't handle it: ~a" type)))
          (lp)))))

  ;; (define serve-http2
  ;;   (lambda (reader writer on-done)
  ;;     (let/async ([settings (h2-default-settings)]
  ;;                 [_ (<- (h2-check-preface reader))] ;; check preface for http2
  ;;                 [_ (<- (write-frame writer h2-frame-type-settings 0 0 #f))]
  ;;                 [header-table (hpack/make-dynamic-table 4096)])
  ;;       (let lp ([streams '()])
  ;;         (let/async ([frame (<- (read-frame reader))]
  ;;                     [type (h2-frame-type frame)]
  ;;                     [flags (h2-frame-flags frame)])
  ;;           (info "flags: ~8,'0b" flags)
  ;;           (info "type: ~a" type)
  ;;           (cond
  ;;            ((= h2-frame-type-goaway type) (begin
  ;;                                             (info "go away")
  ;;                                             (on-done #t)))
  ;;            ((= h2-frame-type-ping type)
  ;;             (cond
  ;;              ((not (= 0 (h2-frame-id frame))) (raise (make-h2-error h2-error-protocol-error "invalid stream id for ping")))
  ;;              ((not (= 8 (bytevector-length (h2-frame-payload frame))))
  ;;               (raise (make-h2-error h2-error-frame-size-error "invalid size for ping frame")))
  ;;              (else
  ;;               (begin
  ;;                 ((writer (bytevector->uv-buf (make-frame h2-frame-type-ping h2-ping-ack 0 (h2-frame-payload frame)) ))
  ;;                  (lambda (n)
  ;;                    (info "wrote ping ack") #f))
  ;;                 (lp streams)))))
  ;;            ((= h2-frame-type-settings type) (begin
  ;;                                               (if (h2-header-flag? frame h2-settings-ack)
  ;;                                                   (begin
  ;;                                                     (lp streams))
  ;;                                                   (begin
  ;;                                                     (set! settings (merge-alist settings (read-settings-frame frame)))
  ;;                                                     ((write-h2-ack-settings writer) (lambda (n)
  ;;                                                                                       #f
  ;;                                                                                       ))
  ;;                                                     (lp streams)))))
  ;;            ((= h2-frame-type-headers type) (begin
  ;;                                              ;; TODO: assert for prev. used stream ids
  ;;                                              (cond
  ;;                                               ((not (h2-header-flag? frame h2-header-flag-end-headers))
  ;;                                                (begin
  ;;                                                  ;; do something with continuation frames
  ;;                                                  ;; add to streams -> waiting for complete headers
  ;;                                                  (lp (cons (list (h2-frame-id frame) frame) streams))))
  ;;                                               ((h2-header-flag? frame h2-header-flag-end-headers h2-header-flag-end-stream)
  ;;                                                (begin
  ;;                                                  ;; end of headers and end of stream
  ;;                                                  (let ([headers (read-h2-header-frame frame -table)])
  ;;                                                    ((writer (bytevector->uv-buf (process-h2-request headers #f)))
  ;;                                                     (lambda (x)
  ;;                                                       (lp streams))))))
  ;;                                               ((h2-header-flag? frame h2-header-flag-end-headers)
  ;;                                                (let ([headers (read-h2-header-frame frame header-table)])
  ;;                                                  (lp (cons (list (h2-frame-id frame) frame) streams))))
  ;;                                               (else
  ;;                                                (begin
  ;;                                                  (lp streams))
  ;;                                                ))))
  ;;            ((= h2-frame-type-window-update type) (begin
  ;;                                                    (let ([size (bytevector-u32-ref (h2-frame-payload frame) 0 'big)])
  ;;                                                      ;; (info "flags: ~a, stream-id: ~a" (h2-frame-flags frame) (h2-frame-id frame))
  ;;                                                      ;; (info "window-size: ~a" size)
  ;;                                                      (lp streams))))
  ;;             (else (begin
  ;;                     (info "idk: ~a" (h2-frame-type frame))
  ;;                     (lp streams)))))
  ;;         )
  ;;       )))


  (define (uv/serve-https ctx stream on-done)
    ((tls-accept ctx stream)
     (lambda (tls)
       (case (ssl/get-selected-alpn (car tls))
         (h2 (serve-http2 (cadr tls) (caddr tls)
                          (lambda (ok)
                            (ssl/free-stream (car tls)) (on-done ok))))
         (http/1.1 (serve-http (cadr tls) (caddr tls)
                               (lambda (ok)
                                 (ssl/free-stream (car tls))
                                 (on-done ok))))))))

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
                  [client (<- (tls-connect ctx stream))]
                  [status (<- ((caddr client) (format #f "GET ~a HTTP/1.1\r\nHost: ~a\r\nConnection: close\r\nContent-Length: 0\r\n\r\n" (uv/url-path url)
                                                   (uv/url-host url))))]
                  [resp (<- (uv/read-http-response (cadr client)))]
                  [done (<- (tls-shutdown (car client) stream))])
                 (k resp)))))
