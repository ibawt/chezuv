(library (hpack)
  (export hpack/decode
          hpack/make-dynamic-table
          hpack/encode)
  (import (chezscheme)
          (utils)
          (log))

  (define-record-type huffman-entry
    (fields char value bits))


  (define huffman-table
    (list->vector
     (map (lambda (e) (apply make-huffman-entry e))
          '((0 #x1ff8 13)
            (1 #x7fffd8 23)
            (2 #xfffffe2 28)
            (3 #xfffffe3 28)
            (4 #xfffffe4 28)
            (5 #xfffffe5 28)
            (6 #xfffffe6 28)
            (7 #xfffffe7 28)
            (8 #xfffffe8 28)
            (9 #xffffea 24)
            (10 #x3ffffffc 30)
            (11 #xfffffe9 28)
            (12 #xfffffea 28)
            (13 #x3ffffffd 30)
            (14 #xfffffeb 28)
            (15 #xfffffec 28)
            (16 #xfffffed 28)
            (17 #xfffffee 28)
            (18 #xfffffef 28)
            (19 #xffffff0 28)
            (20 #xffffff1 28)
            (21 #xffffff2 28)
            (22 #x3ffffffe 30)
            (23 #xffffff3 28)
            (24 #xffffff4 28)
            (25 #xffffff5 28)
            (26 #xffffff6 28)
            (27 #xffffff7 28)
            (28 #xffffff8 28)
            (29 #xffffff9 28)
            (30 #xffffffa 28)
            (31 #xffffffb 28)
            (32 #x14 6)
            (33 #x3f8 10)
            (34 #x3f9 10)
            (35 #xffa 12)
            (36 #x1ff9 13)
            (37 #x15 6)
            (38 #xf8 8)
            (39 #x7fa 11)
            (40 #x3fa 10)
            (41 #x3fb 10)
            (42 #xf9 8)
            (43 #x7fb 11)
            (44 #xfa 8)
            (45 #x16 6)
            (46 #x17 6)
            (47 #x18 6)
            (48 #x0 5)
            (49 #x1 5)
            (50 #x2 5)
            (51 #x19 6)
            (52 #x1a 6)
            (53 #x1b 6)
            (54 #x1c 6)
            (55 #x1d 6)
            (56 #x1e 6)
            (57 #x1f 6)
            (58 #x5c 7)
            (59 #xfb 8)
            (60 #x7ffc 15)
            (61 #x20 6)
            (62 #xffb 12)
            (63 #x3fc 10)
            (64 #x1ffa 13)
            (65 #x21 6)
            (66 #x5d 7)
            (67 #x5e 7)
            (68 #x5f 7)
            (69 #x60 7)
            (70 #x61 7)
            (71 #x62 7)
            (72 #x63 7)
            (73 #x64 7)
            (74 #x65 7)
            (75 #x66 7)
            (76 #x67 7)
            (77 #x68 7)
            (78 #x69 7)
            (79 #x6a 7)
            (80 #x6b 7)
            (81 #x6c 7)
            (82 #x6d 7)
            (83 #x6e 7)
            (84 #x6f 7)
            (85 #x70 7)
            (86 #x71 7)
            (87 #x72 7)
            (88 #xfc 8)
            (89 #x73 7)
            (90 #xfd 8)
            (91 #x1ffb 13)
            (92 #x7fff0 19)
            (93 #x1ffc 13)
            (94 #x3ffc 14)
            (95 #x22 6)
            (96 #x7ffd 15)
            (97 #x3 5)
            (98 #x23 6)
            (99 #x4 5)
            (100 #x24 6)
            (101 #x5 5)
            (102 #x25 6)
            (103 #x26 6)
            (104 #x27 6)
            (105 #x6 5)
            (106 #x74 7)
            (107 #x75 7)
            (108 #x28 6)
            (109 #x29 6)
            (110 #x2a 6)
            (111 #x7 5)
            (112 #x2b 6)
            (113 #x76 7)
            (114 #x2c 6)
            (115 #x8 5)
            (116 #x9 5)
            (117 #x2d 6)
            (118 #x77 7)
            (119 #x78 7)
            (120 #x79 7)
            (121 #x7a 7)
            (122 #x7b 7)
            (123 #x7ffe 15)
            (124 #x7fc 11)
            (125 #x3ffd 14)
            (126 #x1ffd 13)
            (127 #xffffffc 28)
            (128 #xfffe6 20)
            (129 #x3fffd2 22)
            (130 #xfffe7 20)
            (131 #xfffe8 20)
            (132 #x3fffd3 22)
            (133 #x3fffd4 22)
            (134 #x3fffd5 22)
            (135 #x7fffd9 23)
            (136 #x3fffd6 22)
            (137 #x7fffda 23)
            (138 #x7fffdb 23)
            (139 #x7fffdc 23)
            (140 #x7fffdd 23)
            (141 #x7fffde 23)
            (142 #xffffeb 24)
            (143 #x7fffdf 23)
            (144 #xffffec 24)
            (145 #xffffed 24)
            (146 #x3fffd7 22)
            (147 #x7fffe0 23)
            (148 #xffffee 24)
            (149 #x7fffe1 23)
            (150 #x7fffe2 23)
            (151 #x7fffe3 23)
            (152 #x7fffe4 23)
            (153 #x1fffdc 21)
            (154 #x3fffd8 22)
            (155 #x7fffe5 23)
            (156 #x3fffd9 22)
            (157 #x7fffe6 23)
            (158 #x7fffe7 23)
            (159 #xffffef 24)
            (160 #x3fffda 22)
            (161 #x1fffdd 21)
            (162 #xfffe9 20)
            (163 #x3fffdb 22)
            (164 #x3fffdc 22)
            (165 #x7fffe8 23)
            (166 #x7fffe9 23)
            (167 #x1fffde 21)
            (168 #x7fffea 23)
            (169 #x3fffdd 22)
            (170 #x3fffde 22)
            (171 #xfffff0 24)
            (172 #x1fffdf 21)
            (173 #x3fffdf 22)
            (174 #x7fffeb 23)
            (175 #x7fffec 23)
            (176 #x1fffe0 21)
            (177 #x1fffe1 21)
            (178 #x3fffe0 22)
            (179 #x1fffe2 21)
            (180 #x7fffed 23)
            (181 #x3fffe1 22)
            (182 #x7fffee 23)
            (183 #x7fffef 23)
            (184 #xfffea 20)
            (185 #x3fffe2 22)
            (186 #x3fffe3 22)
            (187 #x3fffe4 22)
            (188 #x7ffff0 23)
            (189 #x3fffe5 22)
            (190 #x3fffe6 22)
            (191 #x7ffff1 23)
            (192 #x3ffffe0 26)
            (193 #x3ffffe1 26)
            (194 #xfffeb 20)
            (195 #x7fff1 19)
            (196 #x3fffe7 22)
            (197 #x7ffff2 23)
            (198 #x3fffe8 22)
            (199 #x1ffffec 25)
            (200 #x3ffffe2 26)
            (201 #x3ffffe3 26)
            (202 #x3ffffe4 26)
            (203 #x7ffffde 27)
            (204 #x7ffffdf 27)
            (205 #x3ffffe5 26)
            (206 #xfffff1 24)
            (207 #x1ffffed 25)
            (208 #x7fff2 19)
            (209 #x1fffe3 21)
            (210 #x3ffffe6 26)
            (211 #x7ffffe0 27)
            (212 #x7ffffe1 27)
            (213 #x3ffffe7 26)
            (214 #x7ffffe2 27)
            (215 #xfffff2 24)
            (216 #x1fffe4 21)
            (217 #x1fffe5 21)
            (218 #x3ffffe8 26)
            (219 #x3ffffe9 26)
            (220 #xffffffd 28)
            (221 #x7ffffe3 27)
            (222 #x7ffffe4 27)
            (223 #x7ffffe5 27)
            (224 #xfffec 20)
            (225 #xfffff3 24)
            (226 #xfffed 20)
            (227 #x1fffe6 21)
            (228 #x3fffe9 22)
            (229 #x1fffe7 21)
            (230 #x1fffe8 21)
            (231 #x7ffff3 23)
            (232 #x3fffea 22)
            (233 #x3fffeb 22)
            (234 #x1ffffee 25)
            (235 #x1ffffef 25)
            (236 #xfffff4 24)
            (237 #xfffff5 24)
            (238 #x3ffffea 26)
            (239 #x7ffff4 23)
            (240 #x3ffffeb 26)
            (241 #x7ffffe6 27)
            (242 #x3ffffec 26)
            (243 #x3ffffed 26)
            (244 #x7ffffe7 27)
            (245 #x7ffffe8 27)
            (246 #x7ffffe9 27)
            (247 #x7ffffea 27)
            (248 #x7ffffeb 27)
            (249 #xffffffe 28)
            (250 #x7ffffec 27)
            (251 #x7ffffed 27)
            (252 #x7ffffee 27)
            (253 #x7ffffef 27)
            (254 #x7fffff0 27)
            (255 #x3ffffee 26)
            (256 #x3fffffff 30)))))

  (define end-of-string 256)

  (define huffman-table-sorted
    (sort (lambda (a b)
                   (< (huffman-entry-bits a) (huffman-entry-bits b)))
          (vector->list huffman-table)))

  (define (bit-reader reader octets)
    (define bytes-read 0)
    (define num-bits 0)
    (define shift-register 0)
    (lambda (num-bits-requested match-value)
      (when (and (< num-bits num-bits-requested) (< bytes-read octets))
        (let lp ([bits-read 0])
          (unless (>= bits-read num-bits-requested)
            (let ([c (reader #f)])
              (unless c
                (error 'read-more "can't read more, reader returned #f"))
              (set! shift-register (fxlogor (fxsll shift-register 8) c))
              (set! num-bits (+ num-bits 8))
              (set! bytes-read (+ bytes-read 1))
              (lp (+ bits-read 8))))))
      (if (and (> num-bits-requested num-bits) (>= bytes-read octets))
          (if (= (bitmask num-bits) shift-register)
              'end-of-string
              shift-register)
          (if (= match-value (fxlogand (bitmask num-bits-requested) (fxsra shift-register (- num-bits num-bits-requested))))
              (begin
                (set! num-bits (- num-bits num-bits-requested))
                (set! shift-register (fxlogand shift-register (bitmask num-bits)))
                #t)
              #f))))

  (define (decode-huffman-string reader octets)
    (let ([bit-reader (bit-reader reader octets)])
      (let char-loop ([s '()])
        (let table-loop ([table huffman-table-sorted])
          (if (pair? table)
              (let ([matched? (bit-reader (huffman-entry-bits (car table)) (huffman-entry-value (car table)))])
                (cond
                 ((eq? matched? 'end-of-string) (list->string (reverse s)))
                 (matched? (char-loop (cons (integer->char (huffman-entry-char (car table))) s)))
                 (else (table-loop (cdr table)))))
              (error 'decode-huffman-string "no matching huffman bit pattern"))))))

  (define (bit-writer)
    (define shift-register 0)
    (define num-bits 0)
    (let-values ([(out buf) (open-bytevector-output-port)])
      (case-lambda
       ([] (when (positive? num-bits)
             (put-u8 out (fxlogor (fxsll shift-register (- 8 num-bits)) (bitmask (- 8 num-bits))))
             (set! num-bits 0)
             (set! shift-register 0)
             (buf)))
       ([value bits-to-write]
        ;; (info "value: ~a, bits-to-write: ~a" value bits-to-write)
        (let lp ((bits-written 0))
          (if (= bits-written bits-to-write)
              bits-to-write
              (let ([bits-to-fill (min (- bits-to-write bits-written) (- 8 num-bits))])
                ;; (info "bits-to-fill: ~a" bits-to-fill)
                (if (zero? bits-to-fill)
                    (begin
                      (put-u8 out shift-register)
                      (set! shift-register 0)
                      (set! num-bits 0))
                    (begin
                      (set! shift-register
                            (fxlogor (fxsll shift-register bits-to-fill)
                                     (fxlogand (bitmask bits-to-fill) (fxsra value (- bits-to-write bits-written bits-to-fill)))))
                      (set! num-bits (+ num-bits bits-to-fill))))
                (lp (+ bits-written bits-to-fill)))))))))

  (define (list-equal? a b)
    (cond
     ((and (null? a) (null? b)) #t)
     ((and (pair? a) (pair? b) (eq? (car a) (car b)))
      (list-equal? (cdr a) (cdr b)))
     (else #f)))

  (define (find-static-entry-for-header h)
    (let lp ([i 0])
      (if (>= i (vector-length static-table))
          #f
          (let ([entry (vector-ref static-table i)])
            (if (pair? entry)
                (if (list-equal? entry h)
                    i
                    (lp (+ 1 i)))
                (if (eq? entry (car h))
                    i
                    (lp (+ 1 i))))))))

  (define-syntax with-bit-writer
    (syntax-rules ()
      ((_ e body ...)
       (let ([e (bit-writer)])
         body ...
         (e)))))

  (define (encode-huffman-string s)
    (with-bit-writer w
      (string-for-each
       (lambda (c)
         (let ([entry (vector-ref huffman-table (char->integer c))])
           (w (huffman-entry-value entry) (huffman-entry-bits entry)))) s)))

  (define (encode-string-literal writer s)
    (if (number? s)
        (encode-string-literal writer (format #f "~a" s))
        (if (> (string-length s) 4)
            (begin
              (let ([bytes (encode-huffman-string s)])
                (writer 1 1)
                (encode-prefixed-integer writer 7 (bytevector-length bytes))
                (let lp ([i 0])
                  (unless (>= i (bytevector-length bytes))
                    (writer (bytevector-u8-ref bytes i) 8)
                    (lp (+ 1 i))))))
            (begin
              (writer 0 1) ;; no huffman :(
              (encode-prefixed-integer writer 7 (string-length s))
              (string-for-each
               (lambda (s)
                 (writer (char->integer s) 8)) s)))))

  (define (should-index? entry)
    (case entry
      ((authorization cookie set-cookie) #f)
      (else #t)))

  (define (hpack/encode headers)
    (let ([writer (bit-writer)])
      (let lp ([h headers])
        (if (pair? h)
            (let ([static-entry (find-static-entry-for-header (car h))])
              (if static-entry
                  (let ([entry (vector-ref static-table static-entry)])
                    (if (pair? entry)
                        (begin
                          (writer 1 1) ; write to bit 7
                          (encode-prefixed-integer writer 7 static-entry))
                        (if (should-index? entry)
                            (begin
                              (writer #b01 2)
                              (encode-prefixed-integer writer 6 static-entry)
                              (encode-string-literal writer (cadar h)))
                            (begin
                              (writer 1 4)
                              (info "static-entry is: ~a" static-entry)
                              (encode-prefixed-integer writer 4 static-entry)
                              (encode-string-literal writer (cadar h)))))
                    (info "encode a static entry"))
                  (begin
                    (info "encode a dyn entry")))
              (lp (cdr h)))
            (writer)))))

  (define (bytevector-reader bv start)
    (let ([position start])
      (lambda (peek?)
        (if (>= position (bytevector-length bv))
            #f
            (let ([c (bytevector-u8-ref bv position)])
              (unless peek?
                (set! position (+ 1 position)))
              c)))))

  (define (encode-prefixed-integer writer prefix i)
    (if (< i (bitmask prefix))
        (writer i prefix)
        (begin
          (writer (bitmask prefix) prefix)
          (let lp ([i (- i  (bitmask prefix))])
           (if (>= i 128)
               (begin
                 (writer (+ 128 (mod i 128)) 8)
                 (lp (fxdiv i 128)))
               (writer i 8))))))

  (define (decode-prefixed-integer prefix reader initial)
    ;; TODO: add the limits in
    ;; and make this less set!'ey
    (if (< initial (bitmask prefix))
        initial
        (let ([m 0]
              [i initial]
              [b #f])
          (let lp ()
            (set! b (reader #f))
            (set! i (+ i (* (fxsll 1 m) (fxlogand b 127))))
            (set! m (+ m 7))
            (if (= 128 (fxlogand b 128))
                (lp)
                (begin
                  i))))))

  (define (fold-range f n initial)
    (let lp ([i 0]
             [acc initial])
      (if (>= i n)
          acc
          (lp (+ i 1) (f acc)))))

  (define (decode-string-literal reader)
    (let ([c (reader #f)])
      (let* ([huffman-encoded? (fxlogbit? 7 c)]
             [octets (decode-prefixed-integer 7 reader (fxlogand c #x7f))])
        (if huffman-encoded?
            (decode-huffman-string reader octets)
            (list->string (reverse
                           (fold-range (lambda (s)
                                         (cons (integer->char (reader #f)) s))
                                       octets '())))))))

  (define (bitmask bits)
    (- (fxsll 1 bits) 1))

  (define index-header-field #b10000000)
  (define literal-header-field #b01000000)

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

  (define static-table-length (vector-length static-table))

  (define-record-type dynamic-table
    (fields (mutable buffer) head tail size))

  (define (hpack/make-dynamic-table length)
    (make-dynamic-table '() 0 0 0))

  (define (dynamic-table-insert! table value)
    (dynamic-table-buffer-set! table (cons value (dynamic-table-buffer table))))

  (define (mtrace fmt v)
    (info fmt v)
    v)

  (define (header-ref table index)
    (if (<= index static-table-length)
        (vector-ref static-table index)
        (let lp ([n 0]
                 [vals (dynamic-table-buffer table)])
          (if (pair? vals)
              (if (= n index)
                  (car vals)
                  (lp (+ n 1) (cdr vals)))
              (error 'header-ref "index not found" index)))))

  (define (hpack/decode bytes start header-table)
    ;; (let* ([b (with-bit-writer w
    ;;                          (w 0 3)
    ;;                          (encode-prefixed-integer w 5 1337))]
    ;;        [r (bytevector-reader b 0)])

    ;;   (info "decoded is: ~a" (decode-prefixed-integer 5 r (r #f))))
    (let ([r (bytevector-reader bytes start)])
      (let lp ([headers '()])
        (info "decode: ~a" headers)
        (let ([c (r #f)])
          (if c
              (cond
               [(fxlogbit? 7 c) (let* ([n (decode-prefixed-integer 7 r (logand c #x7f))]
                                       [h (header-ref header-table n)])
                                  (when (= 0 n)
                                    (error 'hpack/decode "invalid index for header" n))
                                  (lp (cons h headers)))]
               [(and (not (fxlogbit? 7 c))
                     (fxlogbit? 6 c))
                (let ([index (logand c #b00111111)])
                  (if (= 0 index)
                      (let* ([h (decode-string-literal r)]
                             [v (decode-string-literal r)])
                        (dynamic-table-insert! header-table (list h v))
                        (lp (cons (list h v) headers)))
                      (let* ([i (decode-prefixed-integer 6 r (logand c (bitmask 6)))]
                             [h (header-ref header-table i)]
                             [v (decode-string-literal r)])
                        (dynamic-table-insert! header-table v)
                        (lp (cons (list (if (pair? h) (car h) h) v) headers)))))]

               [(= 0 (fxsra c 4))
                (let ([index (logand c (bitmask 4))])
                  (if (= 0 index)
                      (let* ([h (decode-string-literal r)]
                             [v (decode-string-literal r)])
                        (lp (cons (list h v) headers)))
                      (let* ([i (decode-prefixed-integer 4 r (logand c (bitmask 4)))]
                             [h (header-ref header-table i)]
                             [v (decode-string-literal r)])
                        (lp (cons (list (if (pair? h) (car h) h) v) headers)))))]
               [(= 1 (fxsra c 4))
                (let ([index (logand c (bitmask 4))])
                  (info "in here? index: ~a" index)
                  (if (= 0 index)
                      (let* ([h (decode-string-literal r)]
                             [v (decode-string-literal r)])
                        (lp (cons (list h v) headers)))
                      (let* ([i (decode-prefixed-integer 4 r (logand c (bitmask 4)))]
                             [h (header-ref header-table i)]
                             [v (decode-string-literal r)])
                        (lp (cons (list (if (pair? h) (car h) h) v) headers)))))
                ]
               [(= #b001 (fxsra c 5))
                (begin
                  (let ([n (decode-prefixed-integer 5 r (logand c (bitmask 5)))])
                    (lp headers)))]
                (else (begin
                        (lp headers))))
               headers))))))


