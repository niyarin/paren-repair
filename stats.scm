(define-library (paren-repair stats)
  (import (scheme base))
  (export  minimum-list-length-stats)
  (begin
    ;;未定義動作はあまり許容しない
    (define minimum-list-length-stats
      '((define . 3)
        (if . 3)
        (set! . 3)

        (cons . 3)
        ))
    ))
