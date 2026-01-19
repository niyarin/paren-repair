(include "../scheme-reader/scheme-reader/core.scm")
(include "../token.scm")
(include "../syntax-checker/rules.scm")
(include "../syntax-checker/core.scm")

(import (scheme base)
        (scheme write)
        (prefix (scheme-reader core) rdr/)
        (paren-repair token)
        (paren-repair syntax-checker)
        (srfi 78))

(include "common.scm")

(check-set-mode! 'report-failed)

(check (check-type (list (make-open) (make-close))) => 'complete)
(check (check-type (list (make-open) (make-open) (make-close) (make-close))) => 'complete)
(check (check-type (list (make-open) (make-sym 'x))) => 'valid)
(check (check-type (list (make-close))) => 'invalid)
(check (check-type (list (make-open) (make-close) (make-close))) => 'invalid)

(check (check-type (list (make-open) (make-sym 'let) (make-space)
                         (make-open) (make-open) (make-sym 'x) (make-space) (make-num 1) (make-close) (make-close)
                         (make-space) (make-sym 'x) (make-close)))
       => 'complete)

(check (check-type (list (make-open) (make-sym 'let) (make-space)
                         (make-open) (make-open) (make-sym 'x) (make-space) (make-num 1) (make-close)
                         (make-space) (make-open) (make-sym 'y) (make-space) (make-num 2) (make-close) (make-close)
                         (make-space) (make-sym 'x) (make-close)))
       => 'complete)

(check (check-type (list (make-open) (make-sym 'let) (make-space)
                         (make-open) (make-close)
                         (make-space) (make-sym 'x) (make-close)))
       => 'complete)

(check (check-type (list (make-open) (make-sym 'let) (make-space)
                         (make-open) (make-open) (make-sym 'x) (make-space) (make-num 1) (make-close) (make-close)))
       => 'valid)

(check (check-type (list (make-open) (make-sym 'let) (make-space)
                         (make-open) (make-open) (make-sym 'x) (make-space)))
       => 'valid)

;; Named let
(check (check-type (list (make-open) (make-sym 'let) (make-space) (make-sym 'loop) (make-space)
                         (make-open) (make-open) (make-sym 'i) (make-space) (make-num 0) (make-close) (make-close)
                         (make-space) (make-sym 'i) (make-close)))
       => 'complete)

(check (check-type (list (make-open) (make-sym 'let) (make-space) (make-sym 'loop) (make-space)
                         (make-open) (make-open) (make-sym 'i) (make-space) (make-num 0) (make-close)
                         (make-space) (make-open) (make-sym 'j) (make-space) (make-num 1) (make-close) (make-close)
                         (make-space) (make-sym 'i) (make-close)))
       => 'complete)

(check (check-type (list (make-open) (make-sym 'let) (make-space) (make-sym 'loop) (make-space)))
       => 'valid)

(check (check-type (list (make-open) (make-sym 'let) (make-space) (make-sym 'loop) (make-space)
                         (make-open) (make-close)))
       => 'valid)

;; cond
(check (check-type (list (make-open) (make-sym 'cond)
                         (make-space) (make-open) (make-sym 'x) (make-space) (make-num 1) (make-close)
                         (make-close)))
       => 'complete)

(check (check-type (list (make-open) (make-sym 'cond)
                         (make-space) (make-open) (make-sym 'x) (make-space) (make-num 1) (make-close)
                         (make-space) (make-open) (make-sym 'y) (make-space) (make-num 2) (make-close)
                         (make-close)))
       => 'complete)

(check (check-type (list (make-open) (make-sym 'cond)
                         (make-space) (make-open) (make-open) (make-sym 'eq?) (make-space) (make-sym 'x) (make-space) (make-num 1) (make-close)
                         (make-space) (make-num 1) (make-close)
                         (make-close)))
       => 'complete)

(check (check-type (list (make-open) (make-sym 'cond)
                         (make-space) (make-open) (make-sym 'x) (make-space) (make-num 1) (make-close)))
       => 'valid)

(check (check-type (list (make-open) (make-sym 'cond)
                         (make-space) (make-open) (make-sym 'x)))
       => 'valid)

(check (check-type (list (make-open) (make-sym 'cond)
                         (make-space) (make-open) (make-sym 'else) (make-space) (make-num 1) (make-close)
                         (make-close)))
       => 'complete)

(check (check-type (list (make-sym 'else))) => 'invalid)

(check (check-type (list (make-open) (make-sym 'else) (make-close))) => 'invalid)

;; cond with =>
(check (check-type (list (make-open) (make-sym 'cond)
                         (make-space) (make-open) (make-sym 'x) (make-space) (make-sym '=>) (make-space) (make-sym 'proc) (make-close)
                         (make-close)))
       => 'complete)

(check (check-type (list (make-open) (make-sym 'cond)
                         (make-space) (make-open) (make-open) (make-sym 'assq) (make-space) (make-sym 'x) (make-space) (make-sym 'lst) (make-close)
                         (make-space) (make-sym '=>) (make-space) (make-sym 'proc) (make-close)
                         (make-close)))
       => 'complete)

(check (check-type (list (make-open) (make-sym 'cond)
                         (make-space) (make-open) (make-sym 'x) (make-space) (make-sym '=>) (make-space)
                         (make-open) (make-sym 'lambda) (make-space) (make-open) (make-sym 'y) (make-close)
                         (make-space) (make-sym 'y) (make-close) (make-close)
                         (make-close)))
       => 'complete)

(check (check-type (list (make-open) (make-sym 'cond)
                         (make-space) (make-open) (make-sym 'x) (make-space) (make-sym '=>) (make-space) (make-sym 'proc) (make-close)
                         (make-space) (make-open) (make-sym 'else) (make-space) (make-num 0) (make-close)
                         (make-close)))
       => 'complete)

(check (check-type (list (make-open) (make-sym 'cond)
                         (make-space) (make-open) (make-sym 'x) (make-space) (make-sym '=>) (make-space) (make-sym 'proc1) (make-space) (make-sym 'proc2) (make-close)
                         (make-close)))
       => 'invalid)

(check (check-type (list (make-open) (make-sym 'cond)
                         (make-space) (make-open) (make-sym 'x) (make-space) (make-sym '=>) (make-space) (make-sym 'proc1)
                         (make-space) (make-sym 'proc2) (make-close)
                         (make-close)))
       => 'invalid)

(check (check-type (list (make-open) (make-sym 'define) (make-space) (make-open) (make-sym 'foo) (make-close)
                         (make-space) (make-open) (make-sym 'cond)
                         (make-space) (make-open) (make-sym 'x) (make-space) (make-sym '=>) (make-space) (make-sym 'proc1) (make-space) (make-sym 'proc2) (make-close)
                         (make-close) (make-close)))
       => 'invalid)

;; define
(check (check-type (list (make-open) (make-sym 'define) (make-space) (make-sym 'x) (make-space) (make-num 42) (make-close)))
       => 'complete)

(check (check-type (list (make-open) (make-sym 'define) (make-space) (make-sym 'x) (make-space)
                         (make-open) (make-sym '+) (make-space) (make-num 1) (make-space) (make-num 2) (make-close) (make-close)))
       => 'complete)

(check (check-type (list (make-open) (make-sym 'define) (make-space) (make-open) (make-sym 'f) (make-close)
                         (make-space) (make-num 42) (make-close)))
       => 'complete)

(check (check-type (list (make-open) (make-sym 'define) (make-space) (make-open) (make-sym 'f) (make-space) (make-sym 'x) (make-close)
                         (make-space) (make-sym 'x) (make-close)))
       => 'complete)

(check (check-type (list (make-open) (make-sym 'define) (make-space) (make-open) (make-sym 'f) (make-space) (make-sym 'x) (make-space) (make-sym 'y) (make-close)
                         (make-space) (make-open) (make-sym '+) (make-space) (make-sym 'x) (make-space) (make-sym 'y) (make-close) (make-close)))
       => 'complete)

(check (check-type (list (make-open) (make-sym 'define) (make-space) (make-open) (make-sym 'f) (make-close)
                         (make-space) (make-num 1) (make-space) (make-num 2) (make-close)))
       => 'complete)

(check (check-type (list (make-open) (make-sym 'define) (make-space) (make-open) (make-sym 'f) (make-space)
                         (make-open) (make-sym 'x) (make-close) (make-close)
                         (make-space) (make-sym 'x) (make-close)))
       => 'invalid)

(check (check-type (list (make-open) (make-sym 'define) (make-space) (make-sym 'x)))
       => 'valid)

(check (check-type (list (make-open) (make-sym 'define) (make-space) (make-open) (make-sym 'f) (make-close)))
       => 'valid)

;; case
(check (check-type (list (make-open) (make-sym 'case) (make-space) (make-sym 'x)
                         (make-space) (make-open) (make-open) (make-num 1) (make-close) (make-space) (make-sym 'a) (make-close)
                         (make-close)))
       => 'complete)

(check (check-type (list (make-open) (make-sym 'case) (make-space) (make-sym 'x)
                         (make-space) (make-open) (make-open) (make-num 1) (make-space) (make-num 2) (make-close) (make-space) (make-sym 'a) (make-close)
                         (make-close)))
       => 'complete)

(check (check-type (list (make-open) (make-sym 'case) (make-space) (make-sym 'x)
                         (make-space) (make-open) (make-open) (make-num 1) (make-close) (make-space) (make-sym 'a) (make-close)
                         (make-space) (make-open) (make-open) (make-num 2) (make-close) (make-space) (make-sym 'b) (make-close)
                         (make-close)))
       => 'complete)

(check (check-type (list (make-open) (make-sym 'case) (make-space) (make-sym 'x)
                         (make-space) (make-open) (make-open) (make-num 1) (make-close) (make-space) (make-sym 'a) (make-close)
                         (make-space) (make-open) (make-sym 'else) (make-space) (make-sym 'b) (make-close)
                         (make-close)))
       => 'complete)

(check (check-type (list (make-open) (make-sym 'case) (make-space) (make-open) (make-sym 'get-key) (make-close)
                         (make-space) (make-open) (make-open) (make-num 1) (make-close) (make-space) (make-sym 'a) (make-close)
                         (make-close)))
       => 'complete)

(check (check-type (list (make-open) (make-sym 'case) (make-space) (make-sym 'x)
                         (make-space) (make-open) (make-open) (make-num 1) (make-close) (make-space) (make-sym 'a) (make-space) (make-sym 'b) (make-close)
                         (make-close)))
       => 'complete)

(check (check-type (list (make-open) (make-sym 'case) (make-space) (make-sym 'x)
                         (make-space) (make-open) (make-open) (make-num 1) (make-close) (make-space) (make-sym 'a) (make-close)))
       => 'valid)

(check (check-type (list (make-open) (make-sym 'case) (make-space) (make-sym 'x)
                         (make-space) (make-open) (make-open) (make-num 1)))
       => 'valid)

;; if
;; Pattern 1: if without alternate
(check (check-type (list (make-open) (make-sym 'if) (make-space) (make-sym 'x) (make-space) (make-num 1) (make-close)))
       => 'complete)

(check (check-type (list (make-open) (make-sym 'if) (make-space)
                         (make-open) (make-sym 'eq?) (make-space) (make-sym 'x) (make-space) (make-num 0) (make-close)
                         (make-space) (make-num 1) (make-close)))
       => 'complete)

;; Pattern 2: if with alternate
(check (check-type (list (make-open) (make-sym 'if) (make-space) (make-sym 'x) (make-space) (make-num 1) (make-space) (make-num 2) (make-close)))
       => 'complete)

(check (check-type (list (make-open) (make-sym 'if) (make-space)
                         (make-open) (make-sym '<) (make-space) (make-sym 'x) (make-space) (make-num 10) (make-close)
                         (make-space) (make-open) (make-sym '+) (make-space) (make-sym 'x) (make-space) (make-num 1) (make-close)
                         (make-space) (make-open) (make-sym '-) (make-space) (make-sym 'x) (make-space) (make-num 1) (make-close)
                         (make-close)))
       => 'complete)

;; Pattern 3: if with too many expressions (invalid)
(check (check-type (list (make-open) (make-sym 'if) (make-space) (make-sym 'x) (make-space) (make-num 1) (make-space) (make-num 2) (make-space) (make-num 3) (make-close)))
       => 'invalid)

(check (check-type (list (make-open) (make-sym 'if) (make-space) (make-sym 'x) (make-space) (make-num 1) (make-space) (make-num 2) (make-space) (make-num 3) (make-space) (make-num 4) (make-close)))
       => 'invalid)

;; Regular Lists
(check (check-type (list (make-open) (make-sym 'foo) (make-space) (make-num 1) (make-close)))
       => 'complete)

(check (check-type (list (make-open) (make-sym 'foo) (make-space)
                         (make-open) (make-sym 'bar) (make-space) (make-num 1) (make-close) (make-close)))
       => 'complete)

(check (check-type (list (make-open) (make-sym 'define) (make-sym 'x) (make-num 42) (make-close)))
       => 'complete)

(let* ((state-map (debug-state-map))
       (normal-idx (cdr (assq 'normal state-map)))
       (let-body-idx (cdr (assq 'let-body state-map)))
       (define-body-idx (cdr (assq 'define-body state-map)))
       (cond-expr-idx (cdr (assq 'cond-expr state-map)))
       (in-let-bindings-idx (cdr (assq 'in-let-bindings state-map)))
       (define-formals-idx (cdr (assq 'define-formals state-map))))

  (check (accepts-arbitrary-expr? normal-idx) => #t)
  (check (accepts-arbitrary-expr? let-body-idx) => #t)
  (check (accepts-arbitrary-expr? define-body-idx) => #t)
  (check (accepts-arbitrary-expr? cond-expr-idx) => #t)

  (check (accepts-arbitrary-expr? in-let-bindings-idx) => #f)
  (check (accepts-arbitrary-expr? define-formals-idx) => #f))

(check-report)
