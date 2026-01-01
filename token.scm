(define-library (paren-repair token)
  (import (scheme base)
          (prefix (scheme-reader core) rdr/))
  (export
    open-paren? close-paren? paren?
    whitespace?
    space?
    comment?
    visual-token?
    newline?
    extract-token-positions make-close-paren-token
    make-multi-space
    extract-symbol)
  (begin
    ;;predicates

    (define (open-paren? token)
      (and (rdr/lexical? token)
           (eq? (rdr/lexical-type token) 'OPEN-PAREN)))

    (define (close-paren? token)
      (and (rdr/lexical? token)
           (eq? (rdr/lexical-type token) 'CLOSE-PAREN)))

    (define (paren? token)
      (or (open-paren? token)
          (close-paren? token)))

    (define (space? token)
      (and (rdr/lexical? token)
           (or (eq? (rdr/lexical-type token) 'SPACE)
               (eq? (rdr/lexical-type token) 'MULTI-SPACE))))

    (define (newline? token)
      (and (rdr/lexical? token)
           (eq? (rdr/lexical-type token) 'NEWLINE)))

    (define (whitespace? token)
      (or (space? token)
          (newline? token)))

    (define (comment? token)
      (and (rdr/lexical? token)
           (eq? (rdr/lexical-type token) 'COMMENT)))

    (define (%quote? token)
      (and (rdr/lexical? token)
           (eq? (rdr/lexical-type token) 'QUOTE)))

    (define (%quasi-quote? token)
      (and (rdr/lexical? token)
           (eq? (rdr/lexical-type token) 'QUASI-QUOTE)))

    (define (%unquote? token)
      (and (rdr/lexical? token)
           (eq? (rdr/lexical-type token) 'UNQUOTE)))

    (define (%string? token)
      (and (rdr/lexical? token)
           (eq? (rdr/lexical-type token) 'STRING)))

    (define (%atom? token)
      (and (rdr/lexical? token)
           (eq? (rdr/lexical-type token) 'ATOM)))

    (define (%keyword? token)
      (and (rdr/lexical? token)
           (eq? (rdr/lexical-type token) 'KEYWORD)))

    (define (%dot? token)
      (and (rdr/lexical? token)
           (eq? (rdr/lexical-type token) 'DOT)))

    (define (%directive? token)
      (and (rdr/lexical? token)
           (eq? (rdr/lexical-type token) 'DIRECTIVE)))

    (define (%shebang? token)
      (and (rdr/lexical? token)
           (eq? (rdr/lexical-type token) 'SHEBANG)))

    (define (visual-token? token)
      (or (space? token)
          (newline? token)
          (comment? token)
          (%directive? token)))

    ;;utils

    (define (make-close-paren-token)
      (rdr/make-lexical 'CLOSE-PAREN #\)))

    (define (make-multi-space n)
      (rdr/make-lexical 'MULTI-SPACE (make-string n #\space)))

    (define (extract-symbol token)
      (cond
        ((rdr/lexical? token)
         (let ((data (rdr/lexical-data token)))
           (and (symbol? data) data)))
        ((symbol? token) token)
        (else #f)))

    (define (token-length token)
      (cond
        ((open-paren? token) 1)
        ((close-paren? token) 1)
        ((and (rdr/lexical? token)
              (eq? (rdr/lexical-type token) 'MULTI-SPACE))
         (string-length (rdr/lexical-data token)))
        ((space? token) 1)
        ((newline? token) 0)
        ((%string? token) (+ 2 (string-length (rdr/lexical-data token))))
        ((comment? token) (+ 1 (string-length (rdr/lexical-data token))))
        ((rdr/lexical? token)
         (let ((data (rdr/lexical-data token)))
           (cond
             ((symbol? data) (string-length (symbol->string data)))
             ((string? data) (string-length data))
             ((number? data) (string-length (number->string data)))
             (else 1))))
        ((symbol? token) (string-length (symbol->string token)))
        ((number? token) (string-length (number->string token)))
        (else 1)))

    (define (extract-token-positions tokens)
      (let ((positions (make-vector (length tokens))))
        (let loop ((i 0)
                   (toks tokens)
                   (line 0)
                   (col 0))
          (if (null? toks)
            positions
            (let ((token (car toks)))
              (vector-set! positions i (cons col line))
              (cond
                ((newline? token)
                 (loop (+ i 1) (cdr toks) (+ line 1) 0))
                (else
                 (let ((token-len (token-length token)))
                   (loop (+ i 1) (cdr toks) line (+ col token-len))))))))))))
