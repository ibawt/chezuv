#! /usr/bin/env scheme-script

(import (chezscheme)
        (uv))

(with-uvloop
 (lambda (loop)
   (getaddrinfo loop "google.ca"
                (lambda (status addr)
                  (when (= 0 status)
                    (let ([s (ftype-ref addrinfo (ai_addr) addr)])
                      (format #t "found ip: ~a~n" (sockaddr->ip-address s))
                      (sockaddr-set-port s 80)
                      (display (tcp-connect loop s
                                            (lambda (conn status)
                                              (stream-write conn (format #f "GET / \r\n\r\n")
                                                            (lambda (s status)
                                                              (stream-read s
                                                                           (lambda (s buf)
                                                                             (display (utf8->string buf)))))))))))))
   (uv-run loop 0)))
