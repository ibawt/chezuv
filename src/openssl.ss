(library (openssl)
  (export
   ssl/make-context
   ssl/free-context
   ssl/drain-output-buffer
   ssl/fill-input-buffer
   ssl/write
   ssl/read
   ssl/make-stream
   ssl/free-stream
   ssl/free-stream
   ssl/accept
   ssl/connect
   ssl/shutdown
   ssl/error?
   ssl/get-error
   ssl/set-ca-path!
   ssl/library-error
   ssl/call-with-context
   ssl/num-bytes
   ssl/clear-errors
   ssl/client?
   ssl/get-selected-alpn
   ssl/do-handshake

   ssl-error-none
   ssl-error-ssl
   ssl-error-want-read
   ssl-error-want-write)

  (import (chezscheme)
          (log)
          (utils))

  (define init
    (case (machine-type)
      ((ta6le a6le)
       (begin
         (load-shared-object "libssl.so")))))

  (define-condition-type &ssl-error &condition make-ssl-error ssl-error?
    (code ssl-error-code)
    (message ssl-error-message))

  (define ssl-filetype-pem 1)

  (define ssl-do-handshake
    (foreign-procedure "SSL_do_handshake"
                       (void*)
                       int))

  (define (ssl/do-handshake s)
    (ssl-do-handshake (ssl-stream-ssl s)))

  (define ssl/client?
    (lambda (s)
      (ssl-stream-client? s)))

  (define (ssl/error? e)
    (negative? e))

  (define (ssl/get-error s n)
    (ssl-get-error (ssl-stream-ssl s) n))

  (define bio-ctrl-pending
    (foreign-procedure "BIO_ctrl_pending"
                       (void*)
                       size_t))

  (define (ssl/num-bytes s)
    (bio-ctrl-pending (ssl-stream-writer s)))

  (define ssl-error-none 0)
  (define ssl-error-ssl 1)
  (define ssl-error-want-read 2)
  (define ssl-error-want-write 3)

  (define ssl-op-all #x80000854)
  ;; SSL_OP_NO_SSLv2|SSL_OP_NO_SSLv3|SSL_OP_CIPHER_SERVER_PREFERENCE
  (define ssl-op-modern-server #x2400000)

  (define ssl-modern-cipher-list
    "ECDHE-ECDSA-CHACHA20-POLY1305:ECDHE-RSA-CHACHA20-POLY1305:ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-GCM-SHA384:DHE-RSA-AES128-GCM-SHA256:DHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-AES128-SHA256:ECDHE-RSA-AES128-SHA256:ECDHE-ECDSA-AES128-SHA:ECDHE-RSA-AES256-SHA384:ECDHE-RSA-AES128-SHA:ECDHE-ECDSA-AES256-SHA384:ECDHE-ECDSA-AES256-SHA:ECDHE-RSA-AES256-SHA:DHE-RSA-AES128-SHA256:DHE-RSA-AES128-SHA:DHE-RSA-AES256-SHA256:DHE-RSA-AES256-SHA:ECDHE-ECDSA-DES-CBC3-SHA:ECDHE-RSA-DES-CBC3-SHA:EDH-RSA-DES-CBC3-SHA:AES128-GCM-SHA256:AES256-GCM-SHA384:AES128-SHA256:AES256-SHA256:AES128-SHA:AES256-SHA:DES-CBC3-SHA:!DSS")

  (define ssl/verify-none 0)
  (define ssl/verify-peer 1)
  (define ssl/verify-fail-if-no-peer-cert 2)
  (define ssl/verify-client-once 4)

  (define-record-type ssl-stream (fields ssl reader writer client?))

  (define err-clear-error
    (foreign-procedure "ERR_clear_error"
                       ()
                       void))

  (define (ssl/clear-errors)
    (let lp ([n (err-get-error)]
             [x 0])
      (unless (= 0 n)
        (lp (err-get-error)
            (+ 1 x)))))

  (define ssl/make-stream
    (lambda (ctx client?)
      (let ([ssl (ssl-new ctx)]
            [reader (bio-new (bio-s-mem))]
            [writer (bio-new (bio-s-mem))])
        (ssl-set-bio ssl reader writer)
        (if client?
            (ssl-set-connect-state ssl)
            (ssl-set-accept-state ssl))
        (make-ssl-stream ssl reader writer client?))))

  (define ssl/free-stream
    (lambda (s)
      (ssl-free (ssl-stream-ssl s))))

  (define (clamp x)
    (if (negative? x) 0 x))

  (define (ssl/should-drain s)
    (bio-should-write (ssl-stream-writer s)))

  (define (ssl/should-fill s)
    (bio-should-read (ssl-stream-reader s)))

  (define (ssl/drain-output-buffer ssl-stream buf len)
    (let loop ([bytes-written 0])
      (let ([n (bio-read (ssl-stream-writer ssl-stream) (+ buf bytes-written) (clamp (- len bytes-written)))])
        (if (positive? n)
            (loop (+ bytes-written n))
            (if (not (bio-should-retry (ssl-stream-writer ssl-stream)))
                -1
                bytes-written)))))

  (define (ssl/fill-input-buffer s buf len)
    (let loop ([bytes-read 0])
      (let ([n (bio-write (ssl-stream-reader s) (+ buf bytes-read) (clamp (- len bytes-read)))])
        (if (positive? n)
            (loop (+ n bytes-read))
            bytes-read))))

  (define (ssl/read s buf len)
    (let ([n (ssl-read (ssl-stream-ssl s) buf len)])
      n))

  (define (ssl/write s buf len)
      (let ([n (ssl-write (ssl-stream-ssl s) buf len)])
        n))

  (define ssl/connect
    (lambda (s)
      (ssl-connect (ssl-stream-ssl s))))

  (define (ssl/shutdown s)
    (ssl-shutdown (ssl-stream-ssl s)))

  (define ssl/accept
    (lambda (s)
      (ssl-accept (ssl-stream-ssl s))))

  (define ssl-shutdown
    (foreign-procedure "SSL_shutdown"
                       (void*)
                       int))

  (define ssl-get-error
    (foreign-procedure "SSL_get_error"
                       (void* int)
                       int))

  (define err-get-error
    (foreign-procedure "ERR_get_error"
                       ()
                       int))

  (define err-get-string-n
    (foreign-procedure "ERR_error_string_n"
                       (int u8* int)
                       void))

  (define ssl/library-error
    (lambda ()
      (let ([b (make-bytevector 2048)]
            [e (err-get-error)])
        (err-get-string-n e b (bytevector-length b))
        (make-ssl-error e (from-c-string b)))))

  (define ssl-new
    (foreign-procedure "SSL_new"
                       (void*)
                       void*))

  (define bio-new
    (foreign-procedure "BIO_new"
                       (void*)
                       void*))

  (define bio-s-mem
    (foreign-procedure "BIO_s_mem"
                       ()
                       void*))

  (define ssl-set-connect-state
    (foreign-procedure "SSL_set_connect_state"
                       (void*)
                       void))

  (define ssl-set-accept-state
    (foreign-procedure "SSL_set_accept_state"
                       (void*)
                       void))

  (define ssl-set-bio
    (foreign-procedure "SSL_set_bio"
                       (void* void* void*)
                       void))

  (define bio-write
    (foreign-procedure "BIO_write"
                       (void* void* int)
                       int))

  (define bio-read
    (foreign-procedure "BIO_read"
                       (void* void* int)
                       int))

  (define bio-test-flags
    (foreign-procedure "BIO_test_flags"
                       (void* int)
                       boolean))

  (define bio-flags-should-retry #x08)
  (define bio-flags-should-read 1)
  (define bio-flags-should-write 2)

  (define bio-should-retry
    (lambda (writer)
      (bio-test-flags writer bio-flags-should-retry)))

  (define bio-should-read
    (lambda (w)
      (bio-test-flags w bio-flags-should-read)))

  (define bio-should-write
    (lambda (w)
      (bio-test-flags w bio-flags-should-write)))

  (define ssl-in-init
    (foreign-procedure "SSL_in_init"
                       (void*)
                       boolean))

  (define ssl-is-server
    (foreign-procedure "SSL_is_server"
                       (void*)
                       boolean))

  (define ssl-in-connect-init
    (lambda (ssl)
      (and (ssl-in-init ssl) (not (ssl-is-server ssl)))))

  (define ssl-connect
    (foreign-procedure "SSL_connect"
                       (void*)
                       int))

  (define ssl-accept
    (foreign-procedure "SSL_accept"
                       (void*)
                       int))

  (define ssl-is-init-finished
    (foreign-procedure "SSL_is_init_finished"
                       (void*)
                       int))

  (define ssl-read
    (foreign-procedure "SSL_read"
                       (void* void* int)
                       int))

  (define ssl-write
    (foreign-procedure "SSL_write"
                       (void* void* int)
                       int))

  (define ssl-free
    (foreign-procedure "SSL_free"
                       (void*)
                       void))

  (define ssl-ctx-new
    (foreign-procedure "SSL_CTX_new"
                       (void*)
                       void*))

  (define tls-method
    (foreign-procedure "TLS_method"
                       ()
                       void*))

  (define tls-server-method
    (foreign-procedure "TLS_server_method"
                       ()
                       void*))

  (define ssl-ctx-use-certificate-file
    (foreign-procedure "SSL_CTX_use_certificate_file"
                       (void* string int)
                       int))

  (define ssl-ctx-use-private-key-file
    (foreign-procedure "SSL_CTX_use_PrivateKey_file"
                       (void* string int)
                       int))

  (define ssl-ctx-check-private-key
    (foreign-procedure "SSL_CTX_check_private_key"
                       (void*)
                       int))

  (define ssl-ctx-set-cipher-list
    (foreign-procedure "SSL_CTX_set_cipher_list"
                       (void* string)
                       int))

  (define ssl-ctx-set-options
    (foreign-procedure "SSL_CTX_set_options"
                       (void* int)
                       void))

  (define ssl-ctx-free
    (foreign-procedure "SSL_CTX_free"
                       (void*)
                       void))

  (define ssl-ctx-set-verify
    (foreign-procedure "SSL_CTX_set_verify"
                       (void* int void*)
                       void))

  (define ssl-ctx-set-verify-depth
    (foreign-procedure "SSL_CTX_set_verify_depth"
                       (void* int)
                       void))

  (define ssl/free-context
    (lambda (ctx)
      (ssl-ctx-free ctx)))

  (define ssl-ctx-load-verify-locations
    (foreign-procedure "SSL_CTX_load_verify_locations"
                       (void* string string)
                       int))

  (define ca-path "/etc/ssl/certs")

  (define ssl/set-ca-path!
    (lambda (path)
      (set! ca-path path)))

  (define x509-store-add-cert
    (foreign-procedure "X509_STORE_add_cert"
                       (void* void*)
                       int))

  (define pem-read-bio-x509
    (foreign-procedure "PEM_read_bio_X509"
                       (void* void* void* void*)
                       void*))

  (define bio-free
    (foreign-procedure "BIO_free"
                       (void*)
                       int))

  (define bio-write2
    (foreign-procedure "BIO_write"
                       (void* u8* int)
                       int))

  (define (pem->x509 pem)
    (let* ([b (bio-new (bio-s-mem))]
           [_ (bio-write2 b (string->utf8 pem) (string-length pem))]
           [x509 (pem-read-bio-x509 b 0 0 0)])
      (bio-free b)
      (if (= 0 x509)
          (raise (ssl/library-error))
          x509)))

  (define ssl-ctx-use-certificate
    (foreign-procedure "SSL_CTX_use_certificate"
                       (void* void*)
                       int))

  (define ssl/call-with-context
    (lambda (cert key client? f)
      (let ([ctx (ssl/make-context cert key client?)])
        (f ctx))))

  (define-syntax simple-check
    (syntax-rules ()
      ((_ e)
       (let ([err e])
         (unless (= 1 err)
           (raise (ssl/library-error)))))))

  (define ssl/make-context
    (let ([g (make-guardian)])
     (lambda (cert key client?)
       (let reclaim ([x (g)])
         (when x
           (ssl/free-context x)
           (reclaim (g))))
       (let ([ctx (ssl-ctx-new (if client? (tls-method) (tls-server-method)))])
         (if (= 0 ctx)
             (raise (ssl/library-error)))
         (when cert
           (simple-check (ssl-ctx-use-certificate-file ctx cert ssl-filetype-pem)))
         (when key
           (simple-check (ssl-ctx-use-private-key-file ctx key ssl-filetype-pem))
           (simple-check (ssl-ctx-check-private-key ctx)))
         (simple-check (ssl-ctx-load-verify-locations ctx cert ca-path))

         (ssl/set-alpn-protos ctx alpn-protocols)
         (ssl/set-alpn-callback ctx)
         (if client?
             (begin
               (ssl-ctx-set-verify ctx ssl/verify-peer 0)
               (ssl-ctx-set-options ctx ssl-op-all))
             (begin
               (simple-check (ssl-ctx-set-cipher-list ctx ssl-modern-cipher-list))
               (ssl-ctx-set-options ctx ssl-op-modern-server)))
         (g ctx)
         ctx))))

  (define ssl-ctx-set-alpn-protos
    (foreign-procedure "SSL_CTX_set_alpn_protos"
                       (void* u8* unsigned-int)
                       int))

  (define alpn-protocols
    (u8-list->bytevector (fold-left
                          (lambda (acc x)
                            (cons (string-length x) (append (map char->integer (string->list x)) acc))) '() 
                            '("http/1.1" "h2"))))

  (define ssl-select-next-proto
    (foreign-procedure "SSL_select_next_proto"
                       (void* void* u8* int u8* int)
                       int))

  (define (ssl/set-alpn-protos ctx protos)
    (lock-object alpn-protocols)
    (let ((n (ssl-ctx-set-alpn-protos ctx alpn-protocols (bytevector-length alpn-protocols))))
      n))

  (define npn-unsupported 0)
  (define npn-negotiated 1)
  (define npn-no-overlap 2)

  ;; out == **char, len == *char
  (define (alpn->string out len)
    (let ([n (foreign-ref 'unsigned-8 len 0)])
      (let lp ([i 0]
               [s '()])
        (if (>= i n)
            (list->string (reverse s))
            (lp (+ 1 i) (cons (integer->char (foreign-ref 'unsigned-8 (foreign-ref 'void* out 0) i)) s))))))

  (define (ssl/set-alpn-callback ctx)
    (let ([code (foreign-callable
                 (lambda (ssl out outlen in inlen opaque)
                   (let ((n (ssl-select-next-proto out outlen alpn-protocols (bytevector-length alpn-protocols)
                                                   in inlen)))
                     (if (= npn-negotiated n)
                         0
                         3)))
                 (void* void* void* u8* int void*)
                 int)])

      (lock-object code)
      (ssl-ctx-set-alpn-select-cb ctx (foreign-callable-entry-point code) 0)))

  (define (ssl/get-selected-alpn s)
    (let ([len (foreign-alloc (ftype-sizeof int))]
          [out (foreign-alloc (ftype-sizeof iptr))])
      (ssl-get0-alpn-selected (ssl-stream-ssl s) out len)
      (let ((n (foreign-ref 'int len 0)))
        (let ([alpn
               (let lp ([s '()]
                        [i  0])
                 (if (>= i n)
                     (list->string (reverse s))
                     (lp (cons (integer->char (foreign-ref 'unsigned-8 (foreign-ref 'void* out 0) i)) s)
                         (+ 1 i))))])
          (foreign-free len)
          (foreign-free out)
          (string->symbol alpn)))))

  (define ssl-get0-alpn-selected
    (foreign-procedure "SSL_get0_alpn_selected"
                       (void* void* void*)
                       void))

  (define ssl-ctx-set-alpn-select-cb
    (foreign-procedure "SSL_CTX_set_alpn_select_cb"
                       (void* void* void*)
                       void)))