(define-library (paren-repair graph)
  (import (scheme base)
          (prefix (paren-repair token) t/)
          (paren-repair records)
          (prefix (paren-repair syntax-checker) syntax-checker/))
  (export expand-state)
  (begin
    ;; ========================================
    ;; scoring APIs
    ;; ========================================
    (define (calculate-balance-score state is-final?)
      (let ((stack-depth (length (state-stack state))))
        (cond
          ;; 最終状態で括弧が完全に閉じられている
          ((and is-final? (zero? stack-depth)) 10.0)
          ;; 最終状態でまだ開き括弧が残っている（ペナルティ）
          (is-final? (* -4.0 stack-depth))
          ;; 途中状態：深さが浅いほうが良い
          (else (* 0.1 (- 10 stack-depth))))))

    (define (score-action action-type matched? is-open-paren?)
      (case action-type
        ((KEEP) (if matched? 10.0 0.0))  ; マッチング成功なら高スコア
        ((DELETE) (if is-open-paren? -100.0 -1))
        ((INSERT) -3.0)                   ; 挿入は大きなペナルティ（インデントスコアで補正）
        (else 0.0)))

    (define (calculate-syntax-fail-penalty old-in-fail new-in-fail)
      (if (and (not old-in-fail) new-in-fail)
        -15.0  ; 構文エラー状態に入ると10点減点
        0.0))

    (define (new-calculate-indent-score state action insert-col insert-line)
      (if (null? (cdr (state-stack state)))
        0.0
        (let* ((open-indent (paren-indent (car (state-stack state))))
               (open-col (indent-column open-indent))
               (open-line (indent-line open-indent)))
          (cond
            ((= open-line insert-line) 5.0)
            ((= (+ open-col 1) insert-col) 5.0)
            ((< insert-col (+ open-col 1)) -90.0)
            (else 0.0)))))

    (define (calculate-prev-indent-score state action insert-col insert-line)
      (let* ((prev-indent (paren-prev-object-pos (car (state-stack state))))
             (prev-col (and prev-indent (indent-column prev-indent)))
             (prev-line (and prev-indent (indent-line prev-indent)))
             (open-indent (paren-indent (car (state-stack state))))
             (open-line (indent-line open-indent)))
        (cond
          ((not prev-indent) 0.0)
          ((= prev-line insert-line) 6.0)
          ((= insert-col prev-col) 6.0)
          ((and (%is-second-element? state)) -1.0)
          ((and (%is-third-element? state)
                (= open-line prev-line)
                (> insert-col prev-col))
           -20.0)
          ((and (%is-third-element? state)
                (= open-line prev-line)
                (= insert-col prev-col))
           0.0)
          ((and (%is-third-element? state)
                (= open-line prev-line)
                (< insert-col prev-col))
           -1.0)
          ((> insert-col prev-col) -50.0)
          ((< insert-col prev-col) -50.0)
          (else -2.0))))

    ;; ========================================
    ;; state transition APIs
    ;; ========================================

    (define (copy-state state)
      (make-repair-state
        (state-position state)
        (list-copy (state-stack state))
        (list-copy (state-actions state))
        (state-score state)
        (state-current-indent state)
        (state-current-line state)
        (state-remaining-tokens state)
        (state-expr-root state)
        (state-in-fail-syntax state)
        (state-pda-state state)))

    (define (update-stack-top-prev-pos stack new-prev-pos)
      (if (null? stack)
        stack
        (let* ((top (car stack))
               (new-top (make-paren-context
                          (paren-token top)
                          (paren-indent top)
                          new-prev-pos
                          (paren-element-count top)
                          (paren-accepts-arbitrary-expr top)
                          (paren-pda-state top))))  ; 既存の値を保持
          (cons new-top (cdr stack)))))

    (define (increment-element-count stack)
      (if (null? stack)
        stack
        (let* ((top (car stack))
               (new-top (make-paren-context
                          (paren-token top)
                          (paren-indent top)
                          (paren-prev-object-pos top)
                          (+ (paren-element-count top) 1)
                          (paren-accepts-arbitrary-expr top)
                          (paren-pda-state top))))
          (cons new-top (cdr stack)))))

    (define (%get-prev-token actions)
      (if (null? actions)
        #f
        (repair-action-value (car actions))))

    (define (%is-second-element? state)
      (and (not (null? (cdr (state-stack state))))
           (let ((cnt (paren-element-count (car (state-stack state)))))
             (= cnt 1))))

    (define (%is-third-element? state)
      (and (not (null? (cdr (state-stack state))))
           (let ((cnt (paren-element-count (car (state-stack state)))))
             (= cnt 2))))

    (define (%move-keeping-open-paren token token-indent-info state next-remaining)
       (let* ((pos (state-position state))
              (current-in-fail (state-in-fail-syntax state))
              (current-pda-state (state-pda-state state))

              (transition-result
                (if (not current-in-fail)
                  (if current-pda-state
                    (syntax-checker/transition-step token current-pda-state)
                    (cons 'fail #f))
                  (cons 'skip current-pda-state)))

              (transition-success? (eq? (car transition-result) 'success))
              (new-pda-state
                (cond
                  ((eq? (car transition-result) 'skip) current-pda-state)
                  (transition-success? (cdr transition-result))
                  (else current-pda-state)))

              (accepts-arbitrary?
                (and transition-success?
                     new-pda-state
                     (syntax-checker/accepts-arbitrary-expr? (syntax-checker/pda-expect new-pda-state))))

              (new-in-fail
                (cond
                  (current-in-fail #t)
                  ((not transition-success?) #t)
                  (else #f)))

              (paren-ctx (make-paren-context token token-indent-info #f 0 accepts-arbitrary? new-pda-state))
              (updated-parent-stack
                (update-stack-top-prev-pos (state-stack state) token-indent-info))
              (new-stack (cons paren-ctx updated-parent-stack))
              (current-expr-root (state-expr-root state))
              (new-expr-root
                      (if (<= (length (state-stack state)) 1)
                        (make-expr-root-context paren-ctx next-remaining)
                        current-expr-root))
              (new-action (make-repair-action 'KEEP pos token))
              (token-col (indent-column token-indent-info))
              (token-line (indent-line token-indent-info))
              (new-score (+ (state-score state)
                            (score-action 'KEEP #f #f)
                            (new-calculate-indent-score state new-action token-col token-line)
                            (calculate-prev-indent-score state new-action token-col token-line)
                            (calculate-balance-score
                              (make-repair-state 0 new-stack '() 0 0 0 '() #f #f #f) #f)
                            (calculate-syntax-fail-penalty current-in-fail new-in-fail))))
             (make-repair-state
                 (+ pos 1)
                 new-stack
                 (cons new-action (state-actions state))
                 new-score
                 token-col
                 token-line
                 next-remaining
                 new-expr-root
                 new-in-fail
                 new-pda-state)))

    (define (%move-removing-open-paren token token-indent-info state next-remaining)
       (let* ((pos (state-position state))
              (current-expr-root (state-expr-root state))
              (token-col (indent-column token-indent-info))
              (token-line (indent-line token-indent-info))
              (new-action (make-repair-action 'DELETE pos token))
              (new-score (+ (state-score state)
                            (score-action 'DELETE #f #t))))
          (make-repair-state
                         (+ pos 1)
                         (state-stack state)
                         (cons new-action (state-actions state))
                         new-score
                         token-col
                         token-line
                         next-remaining
                         current-expr-root
                         (state-in-fail-syntax state)
                         (state-pda-state state))))

    (define (%move-keeping-close-paren token token-indent-info state next-remaining)
     (let* ((popped-ctx (car (state-stack state)))
            (popped-stack (cdr (state-stack state)))
            (pos (state-position state))
            (token-col (indent-column token-indent-info))
            (token-line (indent-line token-indent-info))
            (current-in-fail (state-in-fail-syntax state))
            (current-pda-state (state-pda-state state))

            (transition-result
              (if (not current-in-fail)
                (if current-pda-state
                  (syntax-checker/transition-step token current-pda-state)
                  (cons 'fail #f))
                (cons 'skip current-pda-state)))

            (transition-success? (eq? (car transition-result) 'success))
            (new-pda-state
              (cond
                ((eq? (car transition-result) 'skip) current-pda-state)
                (transition-success? (cdr transition-result))
                (else current-pda-state)))

            (popped-accepts-arbitrary? (paren-accepts-arbitrary-expr popped-ctx))
            (popped-pda-state (paren-pda-state popped-ctx))

            (simulated-result
              (if (and current-in-fail popped-accepts-arbitrary? popped-pda-state)
                (syntax-checker/simulate-arbitrary-expr popped-pda-state)
                (cons 'skip #f)))

            (simulated-success? (eq? (car simulated-result) 'success))

            (final-pda-state
              (cond
                (simulated-success? (cdr simulated-result))
                ((eq? (car simulated-result) 'skip) new-pda-state)
                (else new-pda-state)))

            (new-in-fail
              (cond
                ((and current-in-fail popped-accepts-arbitrary? simulated-success?) #f)
                (current-in-fail #t)
                ((not transition-success?) #t)
                (else #f)))

            (nest-start-indent (paren-indent popped-ctx))
            (new-stack (let ((stack1 (update-stack-top-prev-pos popped-stack nest-start-indent)))
                         (increment-element-count stack1)))
            (new-expr-root (if (null? (cdr new-stack)) #f (state-expr-root state)))
            (new-action (make-repair-action 'KEEP pos token))
            (new-score (+ (state-score state)
                          (score-action 'KEEP #t #f)
                          (calculate-balance-score
                            (make-repair-state 0 new-stack '() 0 0 0 '() #f #f #f) #f)
                          (calculate-syntax-fail-penalty current-in-fail new-in-fail))))
         (make-repair-state
           (+ pos 1)
           new-stack
           (cons new-action (state-actions state))
           new-score
           token-col
           token-line
           next-remaining
           new-expr-root
           new-in-fail
           final-pda-state)))

    (define (%move-removing-close-paren token token-indent-info state next-remaining)
       (let* ((pos (state-position state))
              (token-col (indent-column token-indent-info))
              (token-line (indent-line token-indent-info))
              (new-action (make-repair-action 'DELETE pos token))
              (new-score (+ (state-score state) (score-action 'DELETE #f #f))))
          (make-repair-state
                         (+ pos 1)
                         (state-stack state)
                         (cons new-action (state-actions state))
                         new-score
                         token-col
                         token-line
                         next-remaining
                         (state-expr-root state)
                         (state-in-fail-syntax state)
                         (state-pda-state state))))

    (define (%move-keeping-atom token token-indent-info state next-remaining)
      (let* ((pos (state-position state))
             (token-col (indent-column token-indent-info))
             (token-line (indent-line token-indent-info))
             (current-in-fail (state-in-fail-syntax state))
             (current-pda-state (state-pda-state state))
             (new-action (make-repair-action 'KEEP pos token))

             (transition-result
               (if (not current-in-fail)
                 (if current-pda-state
                   (syntax-checker/transition-step token current-pda-state)
                   (cons 'fail #f))
                 (cons 'skip current-pda-state)))

             (transition-success? (eq? (car transition-result) 'success))
             (new-pda-state
               (cond
                 ((eq? (car transition-result) 'skip) current-pda-state)
                 (transition-success? (cdr transition-result))
                 (else current-pda-state)))

             (new-in-fail
               (cond
                 (current-in-fail #t)
                 ((not transition-success?) #t)
                 (else #f)))

             (updated-stack (if (or (t/whitespace? token) (t/comment? token))
                              (state-stack state)
                              (let ((stack1 (update-stack-top-prev-pos (state-stack state) token-indent-info)))
                                (increment-element-count stack1))))

            (indent-score
              (if (t/visual-token? token)
                0.0
                (new-calculate-indent-score state new-action token-col token-line)))
            (prev-indent-score
              (if (t/visual-token? token)
                0.0
                (calculate-prev-indent-score state new-action token-col token-line))))
        (make-repair-state
               (+ pos 1)
               updated-stack
               (cons new-action (state-actions state))
               (+ (state-score state)
                  (score-action 'KEEP #f #f)
                  indent-score
                  prev-indent-score
                  (calculate-syntax-fail-penalty current-in-fail new-in-fail))
               token-col
               token-line
               next-remaining
               (state-expr-root state)
               new-in-fail
               new-pda-state)))

    (define (%move-inserting-close-paren %token %token-indent-info state remaining)
      (let* ((pos (state-position state))
             (popped-ctx (car (state-stack state)))
             (popped-stack (cdr (state-stack state)))
             (current-in-fail (state-in-fail-syntax state))
             (current-pda-state (state-pda-state state))
             (close-paren (t/make-close-paren-token))

             (transition-result
               (if (not current-in-fail)
                 (if current-pda-state
                   (syntax-checker/transition-step close-paren current-pda-state)
                   (cons 'fail #f))
                 (cons 'skip current-pda-state)))

             (transition-success? (eq? (car transition-result) 'success))
             (new-pda-state
               (cond
                 ((eq? (car transition-result) 'skip) current-pda-state)
                 (transition-success? (cdr transition-result))
                 (else current-pda-state)))

             (popped-accepts-arbitrary? (paren-accepts-arbitrary-expr popped-ctx))
             (popped-pda-state (paren-pda-state popped-ctx))

             (simulated-result
               (if (and current-in-fail popped-accepts-arbitrary? popped-pda-state)
                 (syntax-checker/simulate-arbitrary-expr popped-pda-state)
                 (cons 'skip #f)))

             (simulated-success? (eq? (car simulated-result) 'success))

             (final-pda-state
               (cond
                 (simulated-success? (cdr simulated-result))
                 ((eq? (car simulated-result) 'skip) new-pda-state)
                 (else new-pda-state)))

             (new-in-fail
               (cond
                 ((and current-in-fail popped-accepts-arbitrary? simulated-success?) #f)
                 (current-in-fail #t)
                 ((not transition-success?) #t)
                 (else #f)))

             (nest-start-indent (paren-indent popped-ctx))
             (new-stack (let ((stack1 (update-stack-top-prev-pos popped-stack nest-start-indent)))
                          (increment-element-count stack1)))
             (new-expr-root (if (null? (cdr new-stack)) #f (state-expr-root state)))
             (new-action (make-repair-action 'INSERT pos close-paren))
             (new-score (+ (state-score state)
                           (score-action 'INSERT #f #f)
                           ;(calculate-balance-score (make-repair-state 0 new-stack '() 0 0 0 '() #f #f #f) #f)
                           (calculate-syntax-fail-penalty current-in-fail new-in-fail))))
           (make-repair-state
                  pos
                  new-stack
                  (cons new-action (state-actions state))
                  new-score
                  (state-current-indent state)
                  (state-current-line state)
                  remaining
                  new-expr-root
                  new-in-fail
                  final-pda-state)))

    (define (%move-white-space token token-indent-info state next-remaining)
      (let* ((pos (state-position state))
             (token-col (indent-column token-indent-info))
             (token-line (indent-line token-indent-info))
             (new-action (make-repair-action 'KEEP pos token)))
       (make-repair-state
               (+ pos 1)
               (state-stack state)
               (cons new-action (state-actions state))
               (state-score state)
               token-col
               token-line
               next-remaining
               (state-expr-root state)
               (state-in-fail-syntax state)
               (state-pda-state state))))

    (define (expand-state state token token-positions)
      (let* ((token-indent-info (vector-ref token-positions (state-position state)))
             (remaining (state-remaining-tokens state))
             (next-remaining (if (null? remaining) '() (cdr remaining))))
        (cond
          ((t/open-paren? token)
             (list
               (%move-keeping-open-paren token token-indent-info state next-remaining)
               (%move-removing-open-paren token token-indent-info state next-remaining)))
          ((t/close-paren? token)
           (let* ((new-states
                    (if (not (null? (cdr (state-stack state))))
                      (list (%move-keeping-close-paren token token-indent-info state next-remaining))
                      '())))
             (cons (%move-removing-close-paren token token-indent-info state next-remaining)
                   new-states)))
          ((or (t/space? token) (t/comment? token))
           (list (%move-white-space token token-indent-info state next-remaining)))
          ((t/newline? token)
           (let ((new-states
                   (if (and (not (null? (state-stack state)))
                            (not (null? (cdr (state-stack state))))
                            (not (t/comment? (%get-prev-token (state-actions state))))
                            (or (= (indent-line token-indent-info)
                                   (indent-line (paren-indent (car (state-stack state)))))
                                (>= (indent-column token-indent-info)
                                    (indent-column (paren-indent (car (state-stack state)))))))
                     (list (%move-inserting-close-paren #f #f state remaining))
                     '())))
             (cons (%move-keeping-atom token token-indent-info state next-remaining)
                   new-states)))
          (else (list (%move-keeping-atom token token-indent-info state next-remaining))))))))
