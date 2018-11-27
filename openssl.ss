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

   ssl-error-none
   ssl-error-ssl
   ssl-error-want-read
   ssl-error-want-write)

  (import (chezscheme))

  (define init
    (case (machine-type)
      ((ta6le)
       (begin
         (load-shared-object "libcrypto.so")
         (load-shared-object "libssl.so")))))

  (define (info . args)
    (apply format #t args)
    (newline))

  (define ssl-filetype-pem 1)

  (define (ssl/error? e)
    (negative? e))

  (define (ssl/get-error s n)
    (ssl-get-error (ssl-stream-ssl s) n))

  (define ssl-error-none 0)
  (define ssl-error-ssl 1)
  (define ssl-error-want-read 2)
  (define ssl-error-want-write 3)

  (define ssl-op-all #x80000854)
  ;; SSL_OP_NO_SSLv2|SSL_OP_NO_SSLv3|SSL_OP_CIPHER_SERVER_PREFERENCE
  (define ssl-op-modern-server #x2400000)

  (define ssl-modern-cipher-list
    "ECDHE-ECDSA-CHACHA20-POLY1305:ECDHE-RSA-CHACHA20-POLY1305:ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-GCM-SHA384:DHE-RSA-AES128-GCM-SHA256:DHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-AES128-SHA256:ECDHE-RSA-AES128-SHA256:ECDHE-ECDSA-AES128-SHA:ECDHE-RSA-AES256-SHA384:ECDHE-RSA-AES128-SHA:ECDHE-ECDSA-AES256-SHA384:ECDHE-ECDSA-AES256-SHA:ECDHE-RSA-AES256-SHA:DHE-RSA-AES128-SHA256:DHE-RSA-AES128-SHA:DHE-RSA-AES256-SHA256:DHE-RSA-AES256-SHA:ECDHE-ECDSA-DES-CBC3-SHA:ECDHE-RSA-DES-CBC3-SHA:EDH-RSA-DES-CBC3-SHA:AES128-GCM-SHA256:AES256-GCM-SHA384:AES128-SHA256:AES256-SHA256:AES128-SHA:AES256-SHA:DES-CBC3-SHA:!DSS")

  (define-record ssl-stream (ssl reader writer))

  (define ssl/make-stream
    (lambda (ctx client?)
      (let ([ssl (ssl-new ctx)]
            [reader (bio-new (bio-s-mem))]
            [writer (bio-new (bio-s-mem))])
        (if client?
            (ssl-set-connect-state ssl)
            (ssl-set-accept-state ssl))
        (ssl-set-bio ssl reader writer)
        (make-ssl-stream ssl reader writer))))

  (define ssl/free-stream
    (lambda (s)
      #f))

  (define (clamp x)
    (if (negative? x) 0 x))

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
    (let loop ([bytes-read 0])
      (let ([n (ssl-read (ssl-stream-ssl s) (+ buf bytes-read) (clamp (- len bytes-read)))])
        (if (positive? n)
            (loop (+ bytes-read n))
            (if (and (negative? n) (= bytes-read 0))
                n
                bytes-read)))))

  (define (ssl/write s buf len)
    (let loop ([bytes-written 0])
      (let ([n (ssl-write (ssl-stream-ssl s) (+ buf bytes-written) (clamp (- len bytes-written)))])
        (if (positive? n)
            (loop (+ bytes-written n))
            (if (and (negative? n) (= bytes-written 0))
                n
                bytes-written)))))

  (define ssl/connect
    (lambda (s)
      (if (not (ssl-in-connect-init (ssl-stream-ssl s)))
          0
          (ssl-connect (ssl-stream-ssl s)))))

  (define (ssl/shutdown s)
    (ssl-shutdown (ssl-stream-ssl s)))

  (define ssl/accept
    (lambda (s)
      (if (= 0 (ssl-is-init-finished (ssl-stream-ssl s)))
          1
          (let ([n (ssl-accept (ssl-stream-ssl s))])
            (if (<= n 0)
                n
                n)))))

  (define ssl-shutdown
    (foreign-procedure "SSL_shutdown"
                       (void*)
                       int))

  (define ssl-get-error
    (foreign-procedure "SSL_get_error"
                       (void* int)
                       int))

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

  (define bio-should-retry
    (lambda (writer)
      (bio-test-flags writer bio-flags-should-retry)))

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

  (define ssl/free-context
    (lambda (ctx)
      (ssl-ctx-free ctx)))

  (define ssl/make-context
    (lambda (cert key client?)
      (let ([ctx (ssl-ctx-new (if client? (tls-method) (tls-server-method)))])
        (if (= 0 ctx)
            (error 'ssl-ctx-new "making ssl context"))
        (when (and cert key)
            (let ([err (ssl-ctx-use-certificate-file ctx cert ssl-filetype-pem)])
              (if (not (= 1 err))
                  (error 'ssl-ctx-use-certificate-file err)))
            (let ([err (ssl-ctx-use-private-key-file ctx key ssl-filetype-pem)])
              (if (not (= 1 err))
                  (error 'ssl-ctx-use-private-key-file err)))
            (let ([err (ssl-ctx-check-private-key ctx)])
              (if (not (= 1 err))
                  (error 'ssl-ctx-check-private-key err))))
        (if client?
            (ssl-ctx-set-options ctx ssl-op-all)
            (begin
              (ssl-ctx-set-cipher-list ctx ssl-modern-cipher-list)
              (ssl-ctx-set-options ctx ssl-op-modern-server)))
        ctx))))

