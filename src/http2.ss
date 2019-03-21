(library (http2)
  (export h2)
  (import (chezscheme)
          (hpack)
          (log))

  (define h2-preface "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n")

  (define h2-frame-premamble-size (+ 3 1 1 4))

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
        (reader (make-bytevector h2-frame-premamble-size) 0 h2-frame-premamble-size
                (lambda (bv num-read)
                  (unless (= num-read h2-frame-premamble-size)
                    (error 'read-frame "not enough data"))
                  (let ([len (bytevector-u24-ref bv 0 'big)]
                        [type (bytevector-u8-ref bv 3)]
                        [flags (bytevector-u8-ref bv 4)]
                        [id (bytevector-u32-ref bv 5 'big)])
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
                (if (and (= num-read (string-length h2-preface)) (string=? (utf8->string bv) h2-preface))
                    (k #t)
                    (error 'h2-check-preface "invalid preface" bv))))))

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
     (lambda (

acc item)
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

  (define-syntax if-let
    (syntax-rules ()
      ((_ ([a b] binds ... ) body ...)
       (let ([a b])
         (if a
             (if-let (binds ...)
                     body ...))))))

  (define (h2-session-setting-value session setting)
    (let ([e (assoc setting (h2-session-settings session))])
      (if e
          (cadr e)
          (error 'h2-session-settings "invalid setting" setting))))

  (define-syntax assoc-value
    (syntax-rules ()
      ((_ key alist)
       (let ([e (assoc key alist)])
         (if e
             (cadr e)
             #f)))))

  (define (h2-session-find-stream session id)
    (assoc-value id (h2-session-streams session)))

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

  (define (h2-handler-goaway session frame)
    (info "going away!")
    #f)

  (define (h2-handler-ping session frame)
    (info "ping handler")
    (cond
     ((not (= 0 (h2-frame-id frame))) (raise (make-h2-error h2-error-protocol-error "invalid stream id for ping")))
     ((not (= 8 (bytevector-length (h2-frame-payload frame))))
      (raise (make-h2-error h2-error-frame-size-error "invalid size for ping frame")))
     (else
      (begin
        (info "ping handler")
        (((h2-session-writer session) (bytevector->uv-buf (make-frame h2-frame-type-ping h2-ping-ack 0 (h2-frame-payload frame))))
         (lambda (n)
           (info "wrote ping ack") #f)))))
    #t)

  (define (h2-handler-settings session frame)
    (begin
      (if (h2-header-flag? frame h2-settings-ack)
          (begin
            (info "remote acked settings")
            #t)
          (begin
            (h2-session-settings-set! session (merge-alist (h2-session-settings session) (read-settings-frame frame)))
            ((write-h2-ack-settings (h2-session-writer session)) (lambda (n)
                                                                   #t))))
      #t))

  (define (h2-handler-continuation session frame)
    (info "continuation")
    (let ([stream (h2-session-find-stream session (h2-frame-id frame))])
      (unless (h2-stream-valid-for-continuation? stream)
        (raise (make-h2-error h2-error-protocol-error "last frame was not a valid frame for a continuation")))
      (h2-stream-push-frame! stream frame)
      (when (h2-header-flag? frame 4)
        (h2-stream-assemble-headers! frame))
      (when (h2-header-flag? frame 1)
        (h2-stream-state-set! stream 'half-closed-remote)
        (process-h2-request stream))))

  (define (h2-handler-data session frame)
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
        (when (fxlogbit? 1 (h2-frame-flags frame))
          ;; end-stream
          (h2-stream-state-set! stream 'half-closed-remote)
          (process-h2-request stream))))
    )

  (define (h2-handler-window-update session frame)
    (begin
      (info "window update")
      (let ([size (bytevector-u32-ref (h2-frame-payload frame) 0 'big)])
        ;; (info "flags: ~a, stream-id: ~a" (h2-frame-flags frame) (h2-frame-id frame))
        ;; (info "window-size: ~a" size)
        #t
        ))
    )

  (define (h2-handler-headers session frame)
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

  (define (h2-handler-priority session frame)
    #t)


  (define (h2-handler-rst-stream session frame)
    #t)

  (define (h2-handler-push-promise session frame)
    #t)

  (define h2-frame-type-handlers
    (list->vector
     (list h2-handler-data
           h2-handler-headers
           h2-handler-priority
           h2-handler-rst-stream
           h2-handler-settings
           h2-handler-push-promise
           h2-handler-ping
           h2-handler-goaway
           h2-handler-window-update
           h2-handler-continuation)))

  (define (h2-event-loop session)
    (lambda (k)
      (guard (e [(h2-error? e)
                 (begin
                   (info "caught a http2 error aborting connection: [~a] ~a" (h2-error-code e) (h2-error-message e))
                   (k #f))])
             (let lp ()
               (let/async ([frame (<- (read-frame (h2-session-reader session)))]
                           [type (h2-frame-type frame)]
                           [flags (h2-frame-flags frame)])
                  (when (> type h2-frame-type-continuation)
                    (raise (make-h2-error h2-error-protocol-error "invalid frame type")))
                  (let ([h (vector-ref h2-frame-type-handlers type)])
                    (unless h (error 'not-implemented "no handler for type" type))
                    (let ([r (h session frame)])
                      (lp))))))))

  (define (serve-http2 reader writer on-done)
    (define session (new-h2-session reader writer))
    (guard (e [(error? e) (info "error in serving-http2: ~a" e) (on-done #f)])
           (let/async ([_ (<- (h2-check-preface reader))] ;; check preface for http2
                       [_ (<- (write-frame writer h2-frame-type-settings 0 0 #f))])
                      (info "h2 session established")
                      ((h2-event-loop session) on-done))))

  )
