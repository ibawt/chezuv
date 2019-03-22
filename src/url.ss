(library (url)
  (export
   url
   string->url
   url-host
   url-path
   url-port
   &url-error
   url-error?
   url-error-message
   url-protocol)
  (import (chezscheme)
          (irregex))

  (define-condition-type &url-error &condition make-url-error
    url-error?
    (message url-error-message))

  (define-record-type url
    (fields host port protocol path fragment query))

  (define url-regex (irregex "(?<protocol>.+)://(?<host>[A-Za-z0-9._]+):?(?<port>\\d*)/?(?<path>.*)"))
  (define default-ports
    '((http 80)
      (https 443)))

  (define (string->url s)
    (let ([m (irregex-search url-regex s)])
      (if m
          (let ([proto (string->symbol (irregex-match-substring m 'protocol))])
            (make-url
             (irregex-match-substring m 'host)
             (or (string->number (irregex-match-substring m 'port)) (cadr (assoc proto default-ports)))
             proto
             (string-append "/" (irregex-match-substring m 'path))
             #f #f))
          (raise (make-url-error "failed to match url"))))))
