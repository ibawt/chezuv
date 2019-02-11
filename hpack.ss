(library (hpack)
  (export hpack/decode
          hpack/encode)
  (import (chezscheme)
          (log))

  (define static-table
    (list->vector
     '((#f #f) ;; table starts at 1
       :authority
       (:method "GET")
       (:method "POST")
       (:path "/")
       (:path "/index.html")
       (:scheme "http")
       (:scheme "https")
       (:status 200)
       (:status 204)
       (:status 206)
       (:status 304)
       (:status 400)
       (:status 404)
       (:status 500)
       accept-charset
       (accept-encoding "gzip, deflate")
       accept-language
       accept-ranges
       accept
       access-control-allow-origin
       age
       allow
       authorization
       cache-control
       content-disposition
       content-encoding
       content-language
       content-length
       content-location
       content-range
       content-type
       cookie
       date
       etag
       expect
       expires
       from
       host
       if-match
       if-modified-since
       if-none-match
       if-range
       if-unmodified-since
       last-modified
       link
       location
       max-forwards
       proxy-authenticate
       proxy-authorization
       range
       referer
       refresh
       retry-after
       server
       set-cookie
       strict-transport-security
       transfer-encoding
       user-agent
       vary
       via
       www-authenticate)))

  (define (hpack/encode headers)
    #f)

  (define (bytevector-reader bv start)
    (let ([position start])
      (lambda (peek?)
       (if (>= position (bytevector-length bv))
           #f
           (let ([c (bytevector-u8-ref bv position)])
             (unless peek?
              (set! position (+ 1 position)))
             c)))))

  (define (decode-prefixed-integer prefix reader initial)
    ;; TODO: add the limits in
    ;; and make this set!'ey
    (if (< initial (ash 1 prefix))
        initial
        (let ([m 0]
              [i initial]
              [b (reader #f)])
          (let lp ()
            (set! i (+ i (* (ash 1 m) (logand b 127))))
            (set! m (+ m 7))
            (set! b (reader #f))
            (if (= 128 (logand b 128))
                (lp)
                i)))))

  (define (decode-string-literal reader)
    (let ([c (reader #f)])
      (let ([huffman-encoded? (logbit? 7 c)]
            [octets (decode-prefixed-integer 7 reader (logand 7 #x7f))])
        (info "huffman-encoded?: ~a\n,octets: ~a" huffman-encoded? octets)
        (let lp ([i 0])
          (when (< i octets)
            (reader #f)
            (lp (+ 1 i)))))))

  (define (bitmask bits)
    (- (ash 1 bits) 1))

  (define index-header-field #b10000000)
  (define literal-header-field #b01000000)

  (define (hpack/decode bytes start)
    (let ([r (bytevector-reader bytes start)])
      (let lp ([headers '()])
        (info "headers: ~a" headers)
        (let ([c (r #f)])
          (format #t "c: ~8,'0b\n" c)
          (if c
              (cond
               [(logbit? 7 c) (let* ([n (decode-prefixed-integer 7 r (logand c #x7f))]
                                     [h (vector-ref static-table n)])
                                (info "indexed header field: ~a" h)
                                (lp (cons h headers)))]
               [(and (not (logbit? 7 c))
                     (logbit? 6 c))
                (begin
                  (info "literal header field")
                  (let ([index (logand c #b00111111)])
                    (if (= 0 index)
                        (begin
                          (info "idk"))
                        (begin
                          (let* ([i (decode-prefixed-integer 6 r (logand c (bitmask 6)))]
                                 [h (vector-ref static-table i)])
                            (info "decoded int: ~a" i)
                            (info "h:~a" h)
                            (lp (cons (list h (decode-string-literal r)) headers)))))))
                ]
               )
              headers)))))

  )
