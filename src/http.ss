(library (http)
  (export)
  (import (chezscheme)
          (utils))

  (define-record-type http-request
    (fields headers body method url))

  (define-record-type http-response
    (fields headers body status version code))

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

 (define (read-headers reader done)
   (define lines '())
   (uv/read-lines reader
                  (lambda (line)
                    (if (or (not line) (= 0 (bytevector-length line)))
                        (begin
                          (if line
                              (done (reverse lines)))
                          #f)
                        (begin
                          (set! lines (cons line lines))
                          #t)))))

 (define (uv/make-http-request loop url)
   (lambda (k)
     (let/async ([addr (<- (uv/getaddrinfo loop (uv/url-host url)))]
                 [sa (addr->sockaddr addr (uv/url-port url))]
                 [sock (<- (uv/tcp-connect loop sa))]
                 [status (<- (uv/stream-write sock (format #f "GET / HTTP/1.1\r\nHost: ~a\r\nConnection: close\r\n\r\n" (uv/url-host url))))]
                 [_ (info "write status: ~a" status)]
                 [resp (<- (uv/read-http-response (uv/make-reader sock (lambda (b) (uv/stream-read sock b)))))]
                 [_ (handle-close sock nothing)])
                (k resp))))

 (define (uv/make-https-request loop ctx url)
   (lambda (k)
     (let/async ([addr (<- (uv/getaddrinfo loop (uv/url-host url)))]
                 [_ (info "https-request addr is: ~a" addr) ]
                 [stream (<- (uv/tcp-connect loop (addr->sockaddr addr (uv/url-port url))))]
                 [_ (info "stream is: ~a" stream)]
                 [client (<- (tls-connect ctx stream))]
                 [status (<- ((caddr client) (format #f "GET ~a HTTP/1.1\r\nHost: ~a\r\nConnection: close\r\nContent-Length: 0\r\n\r\n" (uv/url-path url)
                                                     (uv/url-host url))))]
                 [resp (<- (uv/read-http-response (cadr client)))]
                 [done (<- (tls-shutdown (car client) stream))])
                (info "https-request: ~a" resp)
                (k resp))))

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

 (define (uv/serve-https ctx stream)
   (lambda (k)
     (let/async ([tls (<- (tls-accept ctx stream))])
                (case (ssl/get-selected-alpn (car tls))
                  (h2 (serve-http2 (cadr tls) (caddr tls)
                                   (lambda (ok)
                                     (ssl/free-stream (car tls)) (k ok))))
                  (http/1.1 (serve-http (cadr tls) (caddr tls)
                                        (lambda (ok)
                                          (ssl/free-stream (car tls))
                                          (k ok))))))))

 (define (parse-status status-line)
   (let ([status (string-split (utf8->string status-line) #\space)])
     (list (car status) (string->number (cadr status)) (caddr status))))
  )
