;; -*- geiser-scheme-implementation: chez -*-
(import (chezscheme)
        (log)
        (uv))

;; (define (run)
;;   (format #t "Running\n")
;;   (call/cc
;;    (lambda (done)
;;      (time
;;       (uv/call-with-loop
;;        (lambda (loop)
;;          (ssl/call-with-context "fixtures/nginx/cert.pem"  "fixtures/nginx/key.pem" #f
;;                                    (lambda (ctx)
;;                                      (let/async ([(status server client) (<- (uv/tcp-listen loop "127.0.0.1:8181"))]
;;                                                  [d (<- (uv/serve-https ctx client))])
;;                                                 (format #t "d: ~a\n" d)
;;                                                 (format #t "closing client: ~a\n" client)
;;                                                 (format #t "server: ~a\n" server)
;;                                                 (uv/close-stream client))))))))))
(define (listen)
  (info "Running...")
  (call/cc
   (lambda (k)
     (uv/call-with-context
      (lambda (ctx)
        (let/async ([(status server client) (<- (uv/tcp-listen ctx "127.0.0.1:8181"))]
                    [(_ msg) (<- (uv/stream-read->bytevector ctx client))]
                    [msg (utf8->string msg)])
                   (if (string=? "PING" msg)
                     (let/async ([n (<- (uv/stream-write ctx client "PONG"))])
                                (uv/close-stream client))
                     (uv/close-stream client))))))))

(listen)