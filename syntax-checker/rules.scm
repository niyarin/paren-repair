;; Transition rules for syntax checker
(define-library (paren-repair syntax-checker rules)
  (import (scheme base))
  (export transition-rules)
  (begin
    ;; Basic parenthesis rules
    (define basic-paren-rules
      '(
        ((normal open-paren) -> (after-open (push normal)))
        ((after-open open-paren) -> (after-open (push normal)))
        ((after-open close-paren) -> (normal (pop)))
        ((normal close-paren) -> (normal (pop)))
        ))

    ;; define syntax rules
    (define define-rules
      '(
        ;; define keyword
        ((after-open (symbol define)) -> (define-after-keyword ()))
        ;; Variable definition: (define var value)
        ((define-after-keyword symbol) -> (define-value ()))
        ((define-value open-paren) -> (define-value-expr (push normal)))
        ((define-value symbol) -> (define-after-value ()))
        ((define-value any) -> (define-after-value ()))
        ;; When value expression is a list
        ((define-value-expr open-paren) -> (define-value-expr (push normal)))
        ((define-value-expr close-paren (stack-top normal)) -> (define-value-expr (pop)))
        ((define-value-expr close-paren) -> (normal (pop)))  ; End of entire define
        ((define-value-expr any) -> (define-value-expr ()))
        ;; After value
        ((define-after-value close-paren) -> (normal (pop)))
        ;; Function definition: (define (name arg ...) body ...)
        ((define-after-keyword open-paren) -> (define-formals (push define-args)))
        ;; Argument list (no nesting allowed)
        ((define-formals symbol) -> (define-formals ()))
        ((define-formals close-paren) -> (define-body (pop)))
        ;; Body part
        ((define-body close-paren) -> (normal (pop)))
        ((define-body open-paren) -> (after-open (push normal)))
        ((define-body any) -> (define-body ()))
        ))

    ;; let syntax rules (regular let and named-let)
    (define let-rules
      '(
        ;; let keyword
        ((after-open (symbol let)) -> (let-after-keyword ()))
        ;; Regular let: (let ((var val) ...) body ...)
        ((let-after-keyword open-paren) -> (in-let-bindings (push let-bindings)))
        ;; Named-let: (let name ((var val) ...) body ...)
        ((let-after-keyword symbol) -> (let-bindings-start ()))
        ((let-bindings-start open-paren) -> (in-let-bindings (push let-bindings)))
        ;; Inside binding list
        ((in-let-bindings open-paren) -> (binding-var (push let-binding)))
        ((in-let-bindings close-paren) -> (let-body (pop)))
        ((binding-var symbol) -> (binding-val ()))
        ;; Value part (atom or list)
        ((binding-val open-paren) -> (binding-val-expr ()))
        ((binding-val any-atom) -> (binding-close ()))
        ((binding-val any) -> (binding-val ()))  ; Skip special chars like quote
        ;; When value is a list
        ((binding-val-expr open-paren) -> (binding-val-expr (push normal)))
        ((binding-val-expr close-paren (stack-top normal)) -> (binding-val-expr (pop)))
        ((binding-val-expr close-paren (stack-top let-binding)) -> (binding-close ()))
        ((binding-val-expr any) -> (binding-val-expr ()))
        ((binding-close close-paren) -> (in-let-bindings (pop)))
        ;; let body (check close-paren first)
        ((let-body close-paren) -> (normal (pop)))
        ((let-body open-paren) -> (after-open (push normal)))
        ((let-body any) -> (let-body ()))
        ))

    ;; let* syntax rules
    (define let*-rules
      '(
        ;; let* keyword
        ((after-open (symbol let*)) -> (let-after-keyword ()))
        ))

    ;; if syntax rules
    (define if-rules
      '(
        ;; if keyword
        ((after-open (symbol if)) -> (if-test ()))
        ;; Test expression
        ((if-test open-paren) -> (if-test-expr (push if-test-ctx)))
        ((if-test symbol) -> (if-consequent ()))
        ((if-test any) -> (if-consequent ()))
        ;; Test expression (when it's a list)
        ((if-test-expr open-paren) -> (if-test-expr (push normal)))
        ((if-test-expr close-paren (stack-top normal)) -> (if-test-expr (pop)))
        ((if-test-expr close-paren (stack-top if-test-ctx)) -> (if-consequent (pop)))
        ((if-test-expr any) -> (if-test-expr ()))
        ;; Consequent expression
        ((if-consequent open-paren) -> (if-consequent-expr (push if-consequent-ctx)))
        ((if-consequent symbol) -> (if-after-consequent ()))
        ((if-consequent any) -> (if-after-consequent ()))
        ;; Consequent expression (when it's a list)
        ((if-consequent-expr open-paren) -> (if-consequent-expr (push normal)))
        ((if-consequent-expr close-paren (stack-top normal)) -> (if-consequent-expr (pop)))
        ((if-consequent-expr close-paren (stack-top if-consequent-ctx)) -> (if-after-consequent (pop)))
        ((if-consequent-expr any) -> (if-consequent-expr ()))
        ;; After consequent - alternate or close
        ((if-after-consequent close-paren) -> (normal (pop)))
        ((if-after-consequent open-paren) -> (if-alternate-expr (push if-alternate-ctx)))
        ((if-after-consequent symbol) -> (if-after-alternate ()))
        ((if-after-consequent any) -> (if-after-alternate ()))
        ;; Alternate expression (when it's a list)
        ((if-alternate-expr open-paren) -> (if-alternate-expr (push normal)))
        ((if-alternate-expr close-paren (stack-top normal)) -> (if-alternate-expr (pop)))
        ((if-alternate-expr close-paren (stack-top if-alternate-ctx)) -> (if-after-alternate (pop)))
        ((if-alternate-expr any) -> (if-alternate-expr ()))
        ;; After alternate - must close
        ((if-after-alternate close-paren) -> (normal (pop)))
        ))

    ;; case syntax rules
    (define case-rules
      '(
        ;; case keyword
        ((after-open (symbol case)) -> (case-after-keyword ()))
        ;; Key expression (any expression)
        ((case-after-keyword open-paren) -> (case-key-expr (push case-key)))
        ((case-after-keyword symbol) -> (case-clause-start ()))  ; When key is symbol
        ((case-after-keyword any) -> (case-clause-start ()))  ; When key is other
        ;; When key expression is a list
        ((case-key-expr open-paren) -> (case-key-expr (push normal)))
        ((case-key-expr close-paren (stack-top normal)) -> (case-key-expr (pop)))
        ((case-key-expr close-paren (stack-top case-key)) -> (case-clause-start (pop)))
        ((case-key-expr any) -> (case-key-expr ()))
        ;; Start of clause
        ((case-clause-start open-paren) -> (case-datum-list-start (push case-clause)))
        ;; Start of datum list
        ((case-datum-list-start open-paren) -> (case-in-datum-list (push case-datum-list)))
        ((case-datum-list-start symbol) -> (case-after-datum-list ()))  ; For else case
        ;; Inside datum list
        ((case-in-datum-list close-paren) -> (case-after-datum-list (pop)))
        ((case-in-datum-list symbol) -> (case-in-datum-list ()))
        ((case-in-datum-list any) -> (case-in-datum-list ()))
        ;; After datum list - expr part
        ((case-after-datum-list close-paren (stack-top case-clause)) -> (case-after-clause (pop)))  ; Empty expr
        ((case-after-datum-list open-paren) -> (case-expr (push normal)))
        ((case-after-datum-list any) -> (case-expr ()))
        ;; Expr part
        ((case-expr open-paren) -> (case-expr (push normal)))
        ((case-expr close-paren (stack-top normal)) -> (case-expr (pop)))
        ((case-expr close-paren (stack-top case-clause)) -> (case-after-clause (pop)))
        ((case-expr any) -> (case-expr ()))
        ;; After clause
        ((case-after-clause open-paren) -> (case-datum-list-start (push case-clause)))
        ((case-after-clause close-paren) -> (normal (pop)))
        ))

    ;; cond syntax rules
    (define cond-rules
      '(
        ;; cond keyword
        ((after-open (symbol cond)) -> (cond-clause-start ()))
        ;; Start of clause
        ((cond-clause-start open-paren) -> (cond-test (push cond-clause)))
        ((cond-after-clause open-paren) -> (cond-test (push cond-clause)))
        ;; Test part (condition expression or else)
        ((cond-test symbol) -> (cond-after-test ()))
        ((cond-test open-paren) -> (cond-test-list (push normal)))  ; When test is list
        ((cond-test close-paren) -> (cond-after-clause (pop)))  ; Empty clause
        ;; Inside test list
        ((cond-test-list open-paren) -> (cond-test-list (push normal)))
        ((cond-test-list close-paren (stack-top normal)) -> (cond-after-test (pop)))  ; End of test list
        ((cond-test-list any) -> (cond-test-list ()))
        ;; After test - => or regular expr
        ((cond-after-test (symbol =>)) -> (cond-arrow-proc ()))  ; => syntax
        ((cond-after-test close-paren (stack-top cond-clause)) -> (cond-after-clause (pop)))  ; Test-only clause
        ((cond-after-test open-paren) -> (cond-expr (push normal)))  ; List in expr
        ((cond-after-test any) -> (cond-expr ()))  ; Continue regular expr
        ;; Proc part after => (only one expression)
        ((cond-arrow-proc symbol) -> (cond-arrow-close ()))
        ((cond-arrow-proc open-paren) -> (cond-arrow-expr (push arrow-proc)))  ; Use arrow-proc context
        ;; arrow-expr - when proc expression is a list, process its contents
        ((cond-arrow-expr open-paren) -> (cond-arrow-expr (push normal)))
        ((cond-arrow-expr close-paren (stack-top normal)) -> (cond-arrow-expr (pop)))  ; End of nested list
        ((cond-arrow-expr close-paren (stack-top arrow-proc)) -> (cond-arrow-close (pop)))  ; End of entire proc expr, wait for clause close
        ((cond-arrow-expr any) -> (cond-arrow-expr ()))
        ;; arrow-close - after proc, only close clause (both symbol and list)
        ((cond-arrow-close close-paren) -> (cond-after-clause (pop)))
        ;; Expr part (body)
        ((cond-expr open-paren) -> (cond-expr (push normal)))  ; List in expr
        ((cond-expr close-paren (stack-top normal)) -> (cond-expr (pop)))  ; End of nested list
        ((cond-expr close-paren (stack-top cond-clause)) -> (cond-after-clause (pop)))  ; End of clause
        ((cond-expr symbol) -> (cond-expr ()))
        ((cond-expr any) -> (cond-expr ()))
        ;; End of entire cond
        ((cond-after-clause close-paren) -> (normal (pop)))
        ))

    ;; Default rules (accept other tokens)
    ;; else is only allowed inside cond, so reject it here
    (define default-rules
      '(
        ((after-open not-else-any) -> (normal ()))
        ((normal not-else-any) -> (normal ()))
        ))

    ;; Combine all rules
    (define transition-rules
      (append basic-paren-rules
              define-rules
              let-rules
              let*-rules
              if-rules
              case-rules
              cond-rules
              default-rules))
))
