(define-library (paren-repair core)
  (import (scheme base)
          (prefix (scheme-reader core) rdr/)
          (only (chicken sort) sort)
          (only (srfi 1) every)
          (prefix (paren-repair token) t/)
          (prefix (paren-repair graph) graph/)
          (paren-repair records)
          (prefix (paren-repair syntax-checker) syntax-checker/))
  (export beam-search
          repair-action-type
          repair-action-position
          repair-action-value
          state-actions
          state-score)
  (begin

    (define (select-top-k states k)
      (let ((sorted (sort states (lambda (a b) (> (state-score a) (state-score b))))))
        (if (<= (length sorted) k)
          sorted
          (let loop ((i 0) (result '()) (remaining sorted))
            (if (>= i k)
              (reverse result)
              (loop (+ i 1) (cons (car remaining) result) (cdr remaining)))))))

    (define (close-remaining-parens state)
      (if (<= (length (state-stack state)) 1)
        (list state)
        (let* ((close-paren (t/make-close-paren-token))
               (new-stack (cdr (state-stack state)))
               (new-expr-root (if (null? (cdr new-stack)) #f (state-expr-root state)))
               (new-action (make-repair-action 'INSERT (state-position state) close-paren))
               (new-score (+ (state-score state))))
          (close-remaining-parens
            (make-repair-state
              (state-position state)
              new-stack
              (cons new-action (state-actions state))
              new-score
              (state-current-indent state)
              (state-current-line state)
              (state-remaining-tokens state)
              new-expr-root
              #f
              #f)))))

    (define (beam-search tokens beam-width)
      (let* ((token-positions
               (vector-map
                 (lambda (x) (make-indent-info (car x) (cdr x)))
                 (t/extract-token-positions tokens)))
             (dummy-paren-ctx (make-paren-context #f (make-indent-info 0 0) #f 0 #f #f))
             (initial-stack (list dummy-paren-ctx))
             (initial-pda-state (syntax-checker/make-pda-state '() syntax-checker/init-state-idx))
             (initial-state (make-repair-state 0 initial-stack '() 0.0 0 0 tokens #f #f initial-pda-state))
             (num-tokens (length tokens)))
        (let loop ((beam (list initial-state)))
          (if (every (lambda (state) (>= (state-position state) num-tokens)) beam)
            (let* ((sorted-beam (select-top-k beam (length beam)))
                   (final-beam
                    (apply append
                           (map (lambda (state)
                                  (close-remaining-parens state))
                                sorted-beam))))
              (car (select-top-k final-beam 1)))
            (let* ((next-beam
                     (apply append
                            (map (lambda (state)
                                   (if (>= (state-position state) num-tokens)
                                     (list state)
                                     (let ((token (car (state-remaining-tokens state))))
                                       (graph/expand-state state token token-positions))))
                                 beam)))
                   (filtered-beam (select-top-k next-beam beam-width)))
              (loop filtered-beam))))))))
