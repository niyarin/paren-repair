(import (scheme base)
        (scheme process-context)
        (scheme write)
        (scheme read))

(define (%main)
  (write "paren-repair")
  (write (command-line))
  (newline))

(%main)
