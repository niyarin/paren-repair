(include "./scheme-reader/scheme-reader/core.scm")

(import (scheme base)
        (scheme process-context)
        (scheme file)
        (scheme write)
        (scheme read)
        (prefix (scheme-reader core) rdr/))

(define (%read-tokens port)
  (let ((cell-top (cons '() '())))
    (let loop ((cell cell-top)
               (paren-depth 0))
      (let* ((token (rdr/read-token port))
             (paren-depth-diff
               (cond
                 ((not (rdr/lexical? token)) 0)
                 ((eq? (rdr/lexical-type token) 'OPEN-PAREN) 1)
                 ((eq? (rdr/lexical-type token) 'CLOSE-PAREN) -1)
                 (else 0))))
        (if (eof-object? token)
          (values (cdr cell-top) paren-depth)
          (begin
            (set-cdr! cell (cons token '()))
            (loop (cdr cell)
                  (+ paren-depth paren-depth-diff))))))))

(define (%repair-paren filename)
  (call-with-input-file
    filename
    (lambda (port)
      (let-values (((tokens depth) (%read-tokens port)))
        #;(write
          (map (lambda (t) (if (rdr/lexical? t) (rdr/lexical-type t) t))
               tokens ))
      (if (zero? depth)
        (begin (write "Ok.")(newline))
        (begin (write "Fail.")(newline)))))))

(define (%main args)
  #;(begin
    (write "paren-repair")
    (write (command-line))
    (newline)
    (write "############")
    (newline))
  (if (null? (cdr args))
    (begin (write "ERROR: usage paren-repair <input-file>.")(newline))
    (%repair-paren (cadr args))))

(%main (command-line))
