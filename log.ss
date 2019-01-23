(library (log)
  (export
   log-level
   info)
  (import (chezscheme))

  (define log-level #t)

  (define (info . args)
    (when log-level
      (display "[INFO] ")
      (apply format #t args)
      (newline))))
