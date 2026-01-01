(define-library (paren-repair syntax-checker)
  (import (scheme base)
          (scheme cxr)
          (prefix (scheme-reader core) rdr/)
          (prefix (paren-repair token) t/)
          (prefix (paren-repair syntax-checker rules) rules/))
  (export check-syntax
          syntax-result-type
          syntax-result-state
          pda-stack
          pda-expect
          pda-expect-name
          debug-state-vector
          debug-state-map
          enable-debug-trace!
          disable-debug-trace!
          transition-step
          accepts-arbitrary-expr?
          make-pda-state
          simulate-arbitrary-expr
          init-state-idx)
  (begin
    (define-record-type <syntax-result>
      (make-syntax-result type state)
      syntax-result?
      (type syntax-result-type)
      (state syntax-result-state))

    (define-record-type <pda-state>
      (make-pda-state stack expect)
      pda-state?
      (stack pda-stack)
      (expect pda-expect))

    ;;
    ;; rules encoder APIs
    ;;

    (define (build-state-index rules)
      (let loop ((rules rules) (states '()) (index 0))
        (if (null? rules)
          (cons (reverse states) index)  ; (state-list . max-index)
          (let* ((rule (car rules))
                 (from (car rule))
                 (from-state (car from))
                 (to (list-ref rule 2))
                 (to-state (car to)))
            (let* ((states (if (not (memq from-state states))
                             (cons from-state states)
                             states))
                   (states (if (not (memq to-state states))
                             (cons to-state states)
                             states)))
              (loop (cdr rules) states (max index (length states))))))))

    (define (make-state-map state-list)
      (let loop ((states state-list) (index 0) (result '()))
        (if (null? states)
          result
          (loop (cdr states) (+ index 1) (cons (cons (car states) index) result)))))

    (define (index->state state-vec index)
      (vector-ref state-vec index))

    (define (state->index state-map state)
      (cdr (assq state state-map)))

    (define (build-transition-table rules state-map)
      (let* ((max-states (length state-map))
             (table (make-vector max-states '())))
        (let loop ((rules rules))
          (when (not (null? rules))
            (let* ((rule (car rules))
                   (from (car rule))
                   (from-state (car from))
                   (from-token (cadr from))
                   (from-stack (and (> (length from) 2)
                                    (list-ref from 2)))
                   (to (list-ref rule 2))
                   (to-state (car to))
                   (to-op (cadr to))
                   (state-idx (state->index state-map from-state))
                   (to-idx (state->index state-map to-state))
                   (compiled-rule (list from-token from-stack to-idx to-op))
                   (existing (vector-ref table state-idx)))
              (vector-set! table state-idx (cons compiled-rule existing))
              (loop (cdr rules)))))
        (vector-map reverse table)))

    (define state-info (build-state-index rules/transition-rules))
    (define state-list (car state-info))
    (define state-map (make-state-map state-list))
    (define state-vector (list->vector state-list))
    (define transition-table (build-transition-table rules/transition-rules state-map))
    (define init-state-idx (cdr (assq 'normal state-map)))

    ;;
    ;; 遷移関数
    ;;

    ;; 状態インデックスから状態名を取得（外部公開用）
    (define (pda-expect-name state)
      (vector-ref state-vector (pda-expect state)))

    ;; デバッグ用に状態情報を公開
    (define (debug-state-vector) state-vector)
    (define (debug-state-map) state-map)

    (define (accepts-arbitrary-expr? state-idx)
      (let ((rules (vector-ref transition-table state-idx)))
        (let loop ((rules rules))
          (if (null? rules)
            #f
            (let* ((rule (car rules))
                   (from-token (car rule))
                   (to-idx (caddr rule))
                   (to-op (cadddr rule)))
              (if (and (eq? from-token 'open-paren)
                       (or (eq? (vector-ref state-vector to-idx) 'after-open)
                           (and (not (null? to-op))
                                (eq? (car to-op) 'push)
                                (eq? (cadr to-op) 'normal))))
                #t
                (loop (cdr rules))))))))

    ;; デバッグトレース
    (define *debug-trace* #f)
    (define (enable-debug-trace!) (set! *debug-trace* #t))
    (define (disable-debug-trace!) (set! *debug-trace* #f))

    (define (show-token-debug token)
      (cond
        ((t/open-paren? token) "(")
        ((t/close-paren? token) ")")
        ((t/whitespace? token) "SP")
        ((t/comment? token) "COM")
        ((rdr/lexical? token)
         (let ((data (rdr/lexical-data token)))
           (cond
             ((symbol? data) (symbol->string data))
             ((number? data) (number->string data))
             ((char? data) (string data))
             (else "???"))))
        ((symbol? token) (symbol->string token))
        (else "???")))

    ;; トークンが条件にマッチするかチェック
    (define (token-matches? token condition)
      (case condition
        ((open-paren) (t/open-paren? token))
        ((close-paren) (t/close-paren? token))
        ((symbol)
         ;; lexicalオブジェクトまたは生のsymbol
         (or (and (rdr/lexical? token)
                  (eq? (rdr/lexical-type token) 'ATOM)
                  (symbol? (rdr/lexical-data token)))
             (symbol? token)))
        ((quote)
         ;; quoteトークン
         (and (rdr/lexical? token)
              (eq? (rdr/lexical-type token) 'QUOTE)))
        ((any-atom)
         ;; 括弧とquote以外の任意のトークン（atom, number, string, etc.）
         (not (or (t/open-paren? token)
                  (t/close-paren? token)
                  (and (rdr/lexical? token)
                       (eq? (rdr/lexical-type token) 'QUOTE)))))
        ((any) #t)
        ((not-else-any)
         ;; elseというsymbol以外のすべて
         (not (or (and (rdr/lexical? token)
                       (eq? (rdr/lexical-type token) 'ATOM)
                       (eq? (rdr/lexical-data token) 'else))
                  (eq? token 'else))))
        (else
         ;; (symbol name) の形式
         (and (pair? condition)
              (eq? (car condition) 'symbol)
              (or (and (rdr/lexical? token)
                       (eq? (rdr/lexical-type token) 'ATOM)
                       (eq? (rdr/lexical-data token) (cadr condition)))
                  (eq? token (cadr condition)))))))

    ;; 遷移ルールを検索（vectorテーブル使用、状態インデックスベース）
    ;; 戻り値: (to-state-idx . stack-op) または #f
    (define (find-transition state-idx token stack)
      (let ((rules (vector-ref transition-table state-idx)))
        (let loop ((rules rules))
          (if (null? rules)
            #f
            (let* ((rule (car rules))
                   (from-token (car rule))
                   (from-stack (cadr rule))
                   (to-idx (caddr rule))
                   (to-op (cadddr rule)))
              (if (and (token-matches? token from-token)
                       (or (not from-stack)
                           (and (not (null? stack))
                                (eq? (car stack) (cadr from-stack)))))
                (cons to-idx to-op)
                (loop (cdr rules))))))))

    ;; スタック操作を適用
    ;; 戻り値: (success . new-stack) または (fail . stack)
    (define (apply-stack-op stack operation)
      (if (null? operation)
        (cons 'success stack)
        (case (car operation)
          ((push) (cons 'success (cons (cadr operation) stack)))
          ((pop) (if (null? stack)
                   (cons 'fail stack)  ; スタックが空なのにpop -> 失敗
                   (cons 'success (cdr stack))))
          (else (cons 'success stack)))))

    ;; トークン列をチェック
    (define (check-syntax tokens)
      (let* ((initial-state-idx (state->index state-map 'normal))
             (initial-state (make-pda-state '() initial-state-idx)))
        (check-tokens tokens initial-state)))

    ;; Returns (success . new-state) or (fail . state)
    (define (transition-step token state)
      (cond
        ((or (t/whitespace? token) (t/comment? token))
         (cons 'success state))

        (else
         (let ((transition (find-transition (pda-expect state) token (pda-stack state))))
           (if transition
             (let* ((next-expect-idx (car transition))
                    (stack-op (cdr transition))
                    (op-result (apply-stack-op (pda-stack state) stack-op)))
               (if (eq? (car op-result) 'fail)
                 (cons 'fail state)
                 (let* ((new-stack (cdr op-result))
                        (new-state (make-pda-state new-stack next-expect-idx)))
                   (cons 'success new-state))))
             (cons 'fail state))))))

    ;; Simulate accepting one arbitrary expression
    ;; This simulates: open-paren -> after-open (push normal) -> close-paren -> normal (pop)
    ;; Returns: (success . new-state) or (fail . state)
    (define (simulate-arbitrary-expr state)
      (let ((state-idx (pda-expect state)))
        (if (accepts-arbitrary-expr? state-idx)
          ;; Simulate open-paren transition
          (let* ((after-open-idx (state->index state-map 'after-open))
                 (new-stack (cons 'normal (pda-stack state)))
                 (temp-state (make-pda-state new-stack after-open-idx)))
            ;; Simulate close-paren transition (pop)
            (if (null? (pda-stack temp-state))
              (cons 'fail state)
              (let* ((normal-idx (state->index state-map 'normal))
                     (popped-stack (cdr (pda-stack temp-state)))
                     (final-state (make-pda-state popped-stack normal-idx)))
                (cons 'success final-state))))
          ;; State doesn't accept arbitrary expressions
          (cons 'fail state))))

    (define (check-tokens tokens state)
      (if (null? tokens)
        (if (null? (pda-stack state))
          (make-syntax-result 'complete state)
          (make-syntax-result 'valid state))
        (let* ((token (car tokens))
               (rest (cdr tokens))
               (result (transition-step token state)))
          (if (eq? (car result) 'fail)
            (make-syntax-result 'invalid state)
            (check-tokens rest (cdr result))))))))
