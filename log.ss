(library (log)
  (export
   info)
  (import (chezscheme))

  (define (info . args)
    (display "[INFO] ")
    (apply format #t args)
    (newline)))
