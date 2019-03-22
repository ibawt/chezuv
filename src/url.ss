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

  (define (uv/string->url s)
    (let ([m (irregex-search url-regex s)])
      (if m
          (let ([proto (string->symbol (irregex-match-substring m 'protocol))])
            `((host ,(irregex-match-substring m 'host))
              (protocol ,proto)
              (port ,(or (string->number (irregex-match-substring m 'port)) (cadr (assoc proto default-ports))))
              (path ,(string-append "/" (irregex-match-substring m 'path)))))
          (raise (make-url-error "failed to match url")))))

  (define (uv/url-host url)
    (cadr (assoc 'host url)))

  (define (uv/url-port url)
    (cadr (assoc 'port url)))

  (define (uv/url-protocol url)
    (cadr (assoc 'protocol url)))

  (define (uv/url-path url)
    (cadr (assoc 'path url))))
