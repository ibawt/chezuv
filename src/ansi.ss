(library (ansi)
  (export
   red
   green
   color)
  (import (chezscheme))

  (define escape (integer->char 27))

  (define red 31)
  (define green 32)

  (define (color c s)
    (format #f "~a[~am~a~a[0m" escape c s escape)))
