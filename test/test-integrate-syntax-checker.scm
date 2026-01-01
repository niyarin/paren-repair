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

;; Test token sequence: "(begin (let ((a b c)) bad-let-body) (let ((d e)) good-let-body))"
(define test-tokens-mixed-let
  (list (make-open) (make-sym 'begin) (make-space)
        ;; First let with bad binding (3 elements)
        (make-open) (make-sym 'let) (make-space)
        (make-open) (make-open) (make-sym 'a) (make-space) (make-sym 'b) (make-space) (make-sym 'c) (make-close) (make-close)
        (make-space) (make-sym 'bad-let-body) (make-close)
        (make-space)
        ;; Second let with good binding (2 elements)
        (make-open) (make-sym 'let) (make-space)
        (make-open) (make-open) (make-sym 'd) (make-space) (make-sym 'e) (make-close) (make-close)
        (make-space) (make-sym 'good-let-body) (make-close)
        (make-close)))

;; Paren info record: stores whether the paren accepts arbitrary expressions and the state
(define-record-type <paren-info>
  (make-paren-info accepts-arbitrary-expr state)
  paren-info?
  (accepts-arbitrary-expr paren-info-accepts-arbitrary-expr)
  (state paren-info-state))

;; Process token sequence with paren tracking
;; Returns: (bad-expr-count . total-expr-count)
(define (process-tokens-with-paren-tracking tokens initial-state)
  (let loop ((tokens tokens)
             (state initial-state)
             (paren-stack '())
             (in-fail #f)
             (bad-expr-count 0)
             (total-expr-count 0))
    (if (null? tokens)
      ;; All tokens processed - return (bad-expr-count . total-expr-count)
      (cons bad-expr-count total-expr-count)
      (let ((token (car tokens))
            (rest (cdr tokens)))
        (if in-fail
          ;; In fail mode - just track parens, don't call transition-step
          (cond
            ((open-paren? token)
             ;; Push dummy paren info (we can't determine accepts-arb in fail mode)
             (let ((paren-info (make-paren-info #f state)))
               (loop rest state (cons paren-info paren-stack) in-fail bad-expr-count total-expr-count)))
            ((close-paren? token)
             ;; Pop paren info
             (if (null? paren-stack)
               (loop rest state paren-stack in-fail bad-expr-count total-expr-count)
               (let* ((paren-info (car paren-stack))
                      (accepts-arb (paren-info-accepts-arbitrary-expr paren-info))
                      (saved-state (paren-info-state paren-info)))
                 (if accepts-arb
                   ;; This paren accepts arbitrary expr - recover by simulating arbitrary expr
                   (let ((sim-result (simulate-arbitrary-expr saved-state)))
                     (if (eq? (car sim-result) 'success)
                       ;; Recovery successful - exit fail mode
                       (loop rest (cdr sim-result) (cdr paren-stack) #f bad-expr-count total-expr-count)
                       ;; Recovery failed - continue in fail mode
                       (loop rest state (cdr paren-stack) in-fail bad-expr-count total-expr-count)))
                   ;; This paren doesn't accept arbitrary expr - just pop and continue
                   (loop rest state (cdr paren-stack) in-fail bad-expr-count total-expr-count)))))
            (else
             ;; Other token - continue
             (loop rest state paren-stack in-fail bad-expr-count total-expr-count)))
          ;; Not in fail mode - normal processing
          (let* ((is-open (open-paren? token))
                 (is-close (close-paren? token))
                 (step-result (transition-step token state)))
            (if (eq? (car step-result) 'fail)
              ;; Transition failed - enter fail mode but continue processing
              (let ((new-bad-count (+ bad-expr-count 1)))
                (cond
                  (is-open
                   (let ((paren-info (make-paren-info #f state)))
                     (loop rest state (cons paren-info paren-stack) #t new-bad-count total-expr-count)))
                  (is-close
                   (if (null? paren-stack)
                     (loop rest state paren-stack #t new-bad-count total-expr-count)
                     (loop rest state (cdr paren-stack) #t new-bad-count total-expr-count)))
                  (else
                   (loop rest state paren-stack #t new-bad-count total-expr-count))))
              ;; Transition succeeded
              (let ((new-state (cdr step-result)))
                (cond
                  ;; Open paren - push paren info
                  (is-open
                   (let* ((state-idx (pda-expect new-state))
                          (accepts-arb (accepts-arbitrary-expr? state-idx))
                          (paren-info (make-paren-info accepts-arb new-state))
                          (new-total (if accepts-arb (+ total-expr-count 1) total-expr-count)))
                     (loop rest new-state (cons paren-info paren-stack) in-fail bad-expr-count new-total)))
                  ;; Close paren - pop paren info
                  (is-close
                   (if (null? paren-stack)
                     ;; Shouldn't happen if transition succeeded, but handle it
                     (loop rest new-state paren-stack in-fail bad-expr-count total-expr-count)
                     (loop rest new-state (cdr paren-stack) in-fail bad-expr-count total-expr-count)))
                  ;; Other token - continue
                  (else
                   (loop rest new-state paren-stack in-fail bad-expr-count total-expr-count)))))))))))

;; Test the function
(let* ((initial-state-idx (cdr (assq 'normal (debug-state-map))))
       (initial-state (make-pda-state '() initial-state-idx))
       (result (process-tokens-with-paren-tracking test-tokens-mixed-let initial-state)))
  (check (car result) => 1)  ; bad-expr-count should be 1
  (check (cdr result) => 3)) ; total-expr-count should be 3

(check-report)
