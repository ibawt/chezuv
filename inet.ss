(library (inet)
  (export
   addrinfo
   sockaddr_in
   sockaddr
   in_addr
   in_addr_t
   AF_INET
   SOCK_STREAM
   SOCK_DGRAM
   int->ip-address
   sockaddr->ip-address
   sockaddr-set-port)
  (import (chezscheme))

  (define init
    (case (machine-type)
      ((ta6le) (load-shared-object "libc.so.6"))))

  ;; FIXME: this only works on OSX, maybe BSD idk

  (define AF_INET 2)
  (define SOCK_STREAM 1)
  (define SOCK_DGRAM 2)

  (meta-cond
   [(eq? 'ta6le (machine-type))
      (define-ftype sa_family_t unsigned-16)
      (define-ftype in_port_t unsigned-16)

      (define-ftype socklen_t unsigned-int)
      (define-ftype sockaddr_un
        (struct (sun_family sa_family_t)
                (sun_data (array 108 char))))

      (define-ftype in_addr_t unsigned-32)
      (define-ftype in_addr
        (struct (s_addr in_addr_t)))

      (define-ftype sockaddr_in
        (struct
         (sin_family sa_family_t)
         (sin_port in_port_t)
         (sin_addr in_addr)))

      (define-ftype sockaddr
        (struct
         (sa_family sa_family_t)
         (sa_data (array 14 unsigned-8))))

      (define-ftype addrinfo
        (struct
         (ai_flags int)
         (ai_family int)
         (ai_socktype int)
         (ai_protocol int)
         (ai_addrlen socklen_t)
         (ai_addr (* sockaddr))
         (ai_canonname (* char))
         (ai_next (* addrinfo))))]
   [else
    (define-ftype sa_family_t unsigned-8)
    (define-ftype in_port_t unsigned-16)

    (define-ftype socklen_t unsigned-int)
    (define-ftype sockaddr_un
      (struct (sun_family sa_family_t)
              (sun_data (array 108 char))))

    (define-ftype in_addr_t unsigned-32)
    (define-ftype in_addr
      (struct (s_addr in_addr_t)))

    (define-ftype sockaddr_in
      (struct
       (sin_len unsigned-8)
       (sin_family sa_family_t)
       (sin_port in_port_t)
       (sin_addr in_addr)
       (sin_zero (array 8 unsigned-8))))

    (define-ftype sockaddr
      (struct
       (len unsigned-8)
       (sa_family sa_family_t)
       (sa_data (array 14 unsigned-8))))

    (define-ftype addrinfo
      (struct
       (ai_flags int)
       (ai_family int)
       (ai_socktype int)
       (ai_protocol int)
       (ai_addrlen socklen_t)
       (ai_canonname (* char))
       (ai_addr (* sockaddr))
       (ai_next (* addrinfo))))])

  (define INADDR_ANY 0)

  (define int->ip-address
    (lambda (in-addr)
      (list (fxand in-addr 255)
            (fxand (fxsra in-addr 8) 255)
            (fxand (fxsra in-addr 16) 255)
            (fxsra in-addr 24))))

  (define sockaddr->ip-address
    (lambda (s)
      (if (= AF_INET (ftype-ref sockaddr (sa_family) s))
          (int->ip-address
           (ftype-ref sockaddr_in (sin_addr s_addr)
                      (make-ftype-pointer sockaddr_in
                                          (ftype-pointer-address s)))))))
  (define htons
    (foreign-procedure "htons"
                       (unsigned-short)
                       unsigned-short))

  (define (sockaddr-set-port s p)
    (ftype-set! sockaddr_in (sin_port)
                (make-ftype-pointer sockaddr_in (ftype-pointer-address s))
                (htons p)))


  (define display-addrinfo
    (lambda (a)
      (define display
        (lambda (a)
          (format #t "ai_flags: ~A~n" (ftype-ref addrinfo (ai_flags) a))
          (format #t "ai_family: ~A~n" (ftype-ref addrinfo (ai_family) a))
          (format #t "ai_sockettype: ~A~n" (ftype-ref addrinfo (ai_socktype) a))
          (format #t "ai_socklen: ~A~n" (ftype-ref addrinfo (ai_addrlen) a))
          (format #t "ai_addr: ~A~n" (sockaddr->ip-address (ftype-ref addrinfo (ai_addr) a)))
          (format #t "ai_canonname: ~A~n"  (ftype-ref addrinfo (ai_canonname) a))
          (format #t "ai_next: ~A~n" (ftype-ref addrinfo (ai_next) a))))
      (for-each-addr a display)))

  (define (for-each-addr a f)
    (let ([next (ftype-ref addrinfo (ai_next) a)])
      (f a)
      (if (ftype-pointer-null? next)
          #f
          (for-each-addr next f)))))
