(library (http)
  (export make-http-request
          http-response-status
          http-response-headers
          http-response-body
          make-http-response
          http-do
          serve-http
          serve-https)
  (import (chezscheme)
          (utils)
          (tls)
          (openssl)
          (log)
          (url)
          (uv))

  (define-record-type (http-request %make-http-request http-request?)
    (fields headers body status url tls-ctx))

  (define (http-request-version req)
    (caddr (http-request-status req)))

  (define make-http-request
    (case-lambda
     ([]
      (error 'make-http-request "invalid arguments"))
     ([url tls-ctx]
      (%make-http-request '() #f "GET" (if (url? url) url (string->url url))
                          tls-ctx))
     ([url]
      (make-http-request url '()))))

  (define-record-type http-status
    (fields code method))

  (define-record-type http-response
    (fields headers body status))

  (define (serve-http ctx stream handler)
    (lambda (on-done)
      (%serve-http (uv/make-reader stream (lambda (b) ((uv/stream-read->bytevector ctx stream) b)))
                   (lambda (s) (uv/stream-write ctx stream s))
                   handler
                   on-done)))

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
                                    (k (make-http-response headers #f status)))
                                    (begin
                                      (uv/read-fully reader content-length
                                                     (lambda (body)
                                                       (k (make-http-response headers body status)))))))
                                (k (make-http-response headers #t #f)))))
                          (error 'eof "read to end of line")))

  (define (header-value headers key)
    (let ([v (assoc key headers)])
      (if v
          (cadr v)
          #f)))

  (define (header->number headers key)
    (let ([v (header-value headers key)])
      (if v (string->number v) v)))

  (define (http-response-string-by-code code)
    (case code
      ((200) "OK")
      ((500) "Internal Server Error")
      (else "IDK")))

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
                                      (k (%make-http-request headers #f status #f #f))
                                      (uv/read-fully reader content-length
                                                     (lambda (body)
                                                       (k (%make-http-request headers body status #f #f))))))
                                (k (%make-http-request headers #t status #f #f))))
                          (error 'eof "read to end of line"))))))

  (define default-transcoder (make-transcoder (utf-8-codec)))

  (define (http-response-serialize response)
    (let-values ([(op g) (open-bytevector-output-port default-transcoder)])
      (format op "HTTP/1.1 ~d ~a\r\n" (car (http-response-status response))
              (http-response-string-by-code (car (http-response-status response))))
      (for-each
       (lambda (x)
         (format op "~a: ~a\r\n" (car x) (cadr x)))
       (http-response-headers response))
      (format op "Date: ~a\r\n" (date-and-time (current-date)))
      (when (http-response-body response)
        (format op "Content-Length: ~d\r\n" (bytevector-length (http-response-body response))))
      (format op "\r\n")
      (if (http-response-body response)
          (put-bytevector op (http-response-body response)))
      (format op "\r\n")
      (g)))

  (define (uv/write-http-response writer response)
    (writer (http-response-serialize response)))

  (define (keep-alive? req)
    (let ([version (string=? "HTTP/1.1" (http-request-version req))]
          [conn (header-value (http-request-headers req) "Connection")])
      (if version
          (not (and conn (string=? "close" conn)))
          #f)))

  (define (error-response-handler writer error on-done)
    ((uv/write-http-response writer (make-http-response '() "error" '(500))) on-done))

  (define (%serve-http reader writer handler on-done)
    (let/async ([req (<- (uv/read-http-request reader))] )
               (guard (x [(error? x) (error-response-handler writer x on-done)])
                 (let/async ([resp (<- (handler req))]
                             [status (<- (uv/write-http-response writer resp))])
                            (if (keep-alive? req)
                                (%serve-http reader writer handler on-done)
                                (on-done status))))))

  ;; TODO: fix this to return a lamba (k)
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

 ;; TODO: fold these methods
 (define (http-do-tls ctx req)
   (lambda (k)
     (let/async ([url (http-request-url req)]
                 [addr (<- (uv/getaddrinfo ctx (url-host url)))]
                 [sa (addr->sockaddr addr (url-port url))]
                 [sock (<- (uv/tcp-connect ctx sa))]
                 [stream (<- (tls-connect ctx (http-request-tls-ctx req) sock))]
                 [n (<- ((tls-stream-writer stream) (format #f "GET / HTTP/1.1\r\nHost: ~a\r\nConnection: close\r\n\r\n" (url-host url))))]
                 [resp (<- (uv/read-http-response (tls-stream-reader stream)))]
                 [n (<- (close-tls-stream ctx stream sock))]
                 [_ (<- (uv/close-stream ctx sock))])
                (k resp))))

 ;; this one as well
 (define (http-do ctx req)
   (if (eq? 'https (url-protocol (http-request-url req)))
       (http-do-tls ctx req)
       (lambda (k)
         (let/async ([url (http-request-url req)]
                     [addr (<- (uv/getaddrinfo ctx (url-host url)))]
                     [sa (addr->sockaddr addr (url-port url))]
                     [sock (<- (uv/tcp-connect ctx sa))]
                     [status (<- (uv/stream-write ctx sock (format #f "GET / HTTP/1.1\r\nHost: ~a\r\nConnection: close\r\n\r\n" (url-host url))))]
                     [resp (<- (uv/read-http-response (uv/make-reader sock (lambda (b)
                                                                             ((uv/stream-read->bytevector ctx sock) b)))))]
                     [_ (<- (uv/close-stream ctx sock))]
                     )
                    (k resp)))))

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

 (define (serve-https ctx tls-ctx stream handler)
   (lambda (k)
     (guard (e [else (k #f)])
      (let/async ([tls (<- (tls-accept ctx tls-ctx stream))])
                 (case (ssl/get-selected-alpn (tls-stream-ssl tls))
                   ;; (h2 (serve-http2 (cadr tls) (caddr tls)
                   ;;                  (lambda (ok)
                   ;;                    (ssl/free-stream (car tls)) (k ok))))
                   (else (%serve-http (tls-stream-reader tls) (tls-stream-writer tls) handler
                                      (lambda (ok)
                                        ((close-tls-stream ctx tls stream) k)))))))))

 (define (parse-status status-line)
   (let ([status (string-split (utf8->string status-line) #\space)])
     (list (car status) (string->number (cadr status)) (caddr status)))))
