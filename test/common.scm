;; Common utilities for tests

(define (make-open) (rdr/make-lexical 'OPEN-PAREN #\())
(define (make-close) (rdr/make-lexical 'CLOSE-PAREN #\)))
(define (make-sym name) (rdr/make-lexical 'ATOM name))
(define (make-num n) (rdr/make-lexical 'ATOM n))
(define (make-space) (rdr/make-lexical 'SPACE #\space))

(define (check-type tokens)
  (syntax-result-type (check-syntax tokens)))
