(define-library (paren-repair core)
  (import (scheme base)
          (prefix (scheme-reader core) rdr/)
          (only (chicken sort) sort)
          (only (srfi 1) every)
          (prefix (paren-repair token) t/)
          (prefix (paren-repair stats) stats/))
  (export beam-search
          repair-action-type
          repair-action-value
          state-actions
          state-score)
  (begin
    ;; ========================================
    ;; ビームサーチ用のデータ構造
    ;; ========================================

    ;; インデント情報
    (define-record-type <indent-info>
      (make-indent-info column line-number)
      indent-info?
      (column indent-column)         ; 列番号（0始まり）
      (line-number indent-line))     ; 行番号（0始まり）

    ;; 括弧とインデントの対応
    (define-record-type <paren-context>
      (make-paren-context token indent)
      paren-context?
      (token paren-token)            ; 括弧トークン
      (indent paren-indent))         ; その括弧のインデント位置 (<indent-info>)

    ;; 式のルート情報
    (define-record-type <expr-root-context>
      (make-expr-root-context paren-ctx tokens-tail)
      expr-root-context?
      (paren-ctx expr-root-paren-context)    ; ルートの開き括弧
      (tokens-tail expr-root-tokens-tail))   ; トークン列へのポインタ

    ;; 修正アクション
    (define-record-type <repair-action>
      (make-repair-action action-type position value)
      repair-action?
      (action-type repair-action-type)  ; 'KEEP, 'DELETE, 'INSERT
      (position repair-action-position)
      (value repair-action-value))

    ;; 修正状態（インデント情報 + remaining-tokens付き）
    (define-record-type <repair-state>
      (make-repair-state position paren-stack actions score current-indent current-line remaining-tokens expr-root)
      repair-state?
      (position state-position)      ; 現在のトークン位置
      (paren-stack state-stack)      ; 開き括弧のスタック (<paren-context>のリスト)
      (actions state-actions)        ; 修正アクション列
      (score state-score)            ; 累積スコア
      (current-indent state-current-indent)  ; 現在のインデント（列番号）
      (current-line state-current-line)      ; 現在の行番号
      (remaining-tokens state-remaining-tokens)  ; まだ処理していないトークン列
      (expr-root state-expr-root))   ; 現在修正中の式のルート (<expr-root-context> or #f)

    ;; ========================================
    ;; スコアリング関数
    ;; ========================================

    (define (calculate-balance-score state is-final?)
      (let ((stack-depth (length (state-stack state))))
        (cond
          ;; 最終状態で括弧が完全に閉じられている
          ((and is-final? (= stack-depth 0)) 10.0)
          ;; 最終状態でまだ開き括弧が残っている（ペナルティ）
          (is-final? (* -5.0 stack-depth))
          ;; 途中状態：深さが浅いほうが良い
          (else (* 0.5 (- 10 stack-depth))))))

    ;; アクションに対するスコアを計算
    (define (score-action action-type matched? is-paren?)
      (case action-type
        ((KEEP) (if matched? 10.0 0.0))  ; マッチング成功なら高スコア
        ((DELETE) (if is-paren? -20.0 -0.5))  ; 括弧の削除は大ペナルティ
        ((INSERT) -5.0)                   ; 挿入は大きなペナルティ（インデントスコアで補正）
        (else 0.0)))

    ;; インデント一貫性スコアを計算（挿入位置の列と行を受け取る）
    (define (calculate-indent-score state action insert-col insert-line)
      (if (not (eq? (repair-action-type action) 'INSERT))
        0.0  ; INSERTアクション以外はスコアなし
        (if (null? (state-stack state))
          0.0  ; スタックが空ならスコアなし
          (let* ((paren-ctx (car (state-stack state)))
                 (open-indent (paren-indent paren-ctx))
                 (open-col (indent-column open-indent))
                 (open-line (indent-line open-indent)))
            (let ((distance (- insert-col open-col)))
              (cond
                ;; 開き括弧と同じ列に配置 -> 最高スコア
                ((= insert-col open-col) 8.0)
                ;; 同じ行で遠すぎる（外側の括弧の可能性） -> 大ペナルティ
                ((and (= insert-line open-line) (>= distance 10)) -15.0)
                ;; 同じ行（直後に配置） -> 高スコア
                ((= insert-line open-line) 5.0)
                ;; それ以外（インデントがずれている） -> ペナルティ
                (else -10.0)))))))

    ;; e.g. (a b c) => 3
    ;;      (a (b c) (d e) f) => 4
    (define (calculate-expr-length root-tokens current-tokens)
      (let loop ((toks root-tokens) (count 0))
        (cond
          ((null? toks) count)
          ((eq? toks current-tokens) count)
          ((t/open-paren? (car toks))
           (let skip-nested ((t (cdr toks)) (depth 1))
             (cond
               ((null? t) (+ count 1))
               ((eq? t current-tokens) (+ count 1))
               ((t/open-paren? (car t))
                (skip-nested (cdr t) (+ depth 1)))
               ((t/close-paren? (car t))
                (if (= depth 1)
                  (loop (cdr t) (+ count 1))
                  (skip-nested (cdr t) (- depth 1))))
               (else (skip-nested (cdr t) depth)))))
          ((or (t/whitespace? (car toks)) (t/comment? (car toks)))
           (loop (cdr toks) count))
          ((t/visual-token? (car toks))
           (loop (cdr toks) (+ count 1)))
          (else (loop (cdr toks) count)))))

    ;; 式の長さスコアを計算
    (define (calculate-expression-length-score state)
      (if (not (expr-root-context? (state-expr-root state)))
        0.0  ; expr-rootがない場合はスコアなし
        (let* ((expr-root (state-expr-root state))
               (root-tokens (expr-root-tokens-tail expr-root))
               ;; 式の長さを計算（開き括弧の次のトークンから現在位置まで）
               (expr-length (calculate-expr-length root-tokens (state-remaining-tokens state)))
               ;; 式の先頭がシンボルかチェック
               (head-token (if (null? root-tokens) #f (car root-tokens)))
               (head-symbol (t/extract-symbol head-token)))
          (if (not head-symbol)
            0.0  ; シンボルでなければスコアなし
            (let ((min-length-pair (assq head-symbol stats/minimum-list-length-stats)))
              (if min-length-pair
                (if (>= expr-length (cdr min-length-pair))
                  10.0   ; 最小長さを満たしている: 良い評価
                  -5.0) ; 最小長さを満たしていない: ペナルティ
                1.0))))))  ; 連想リストにない: 中くらいの評価

    ;; ========================================
    ;; 状態遷移関数
    ;; ========================================

    ;; 状態をコピー
    (define (copy-state state)
      (make-repair-state
        (state-position state)
        (list-copy (state-stack state))
        (list-copy (state-actions state))
        (state-score state)
        (state-current-indent state)
        (state-current-line state)
        (state-remaining-tokens state)  ; リストポインタはそのまま共有
        (state-expr-root state)))

    ;; 状態から次の候補状態を生成（インデント情報 + expr-root付き）
    (define (expand-state state token token-positions)
      (let* ((pos (state-position state))
             (token-indent-info (vector-ref token-positions pos))
             (token-col (indent-column token-indent-info))
             (token-line (indent-line token-indent-info))
             (current-expr-root (state-expr-root state))
             (remaining (state-remaining-tokens state))
             (next-remaining (if (null? remaining) '() (cdr remaining)))
             (candidates '()))
        (cond
          ;; トークンが開き括弧の場合
          ((t/open-paren? token)
           (let* ((paren-ctx (make-paren-context token token-indent-info))
                  (new-stack (cons paren-ctx (state-stack state)))
                  ;; スタックが空なら新しいexpr-rootを設定
                  (new-expr-root
                    (if (null? (state-stack state))
                      (make-expr-root-context paren-ctx next-remaining)
                      current-expr-root)))

             ;; 遷移1: 開き括弧を保持
             (let* ((new-action (make-repair-action 'KEEP pos token))
                    (new-score (+ (state-score state)
                                 (score-action 'KEEP #f #f)
                                 (calculate-balance-score
                                   (make-repair-state 0 new-stack '() 0 0 0 '() #f) #f))))
               (set! candidates
                 (cons (make-repair-state
                         (+ pos 1)
                         new-stack
                         (append (state-actions state) (list new-action))
                         new-score
                         token-col
                         token-line
                         next-remaining
                         new-expr-root)
                       candidates)))

             ;; 遷移2: 開き括弧を削除
             (let* ((new-action (make-repair-action 'DELETE pos token))
                    (new-score (+ (state-score state) (score-action 'DELETE #f #t))))
               (set! candidates
                 (cons (make-repair-state
                         (+ pos 1)
                         (state-stack state)
                         (append (state-actions state) (list new-action))
                         new-score
                         token-col
                         token-line
                         next-remaining
                         current-expr-root)  ; DELETE時はexpr-root変更なし
                       candidates)))))

          ;; トークンが閉じ括弧の場合
          ((t/close-paren? token)
           (if (not (null? (state-stack state)))
             ;; 遷移1: マッチング成功
             (let* ((new-stack (cdr (state-stack state)))
                    ;; スタックが空になったらexpr-rootをリセット
                    (new-expr-root (if (null? new-stack) #f current-expr-root))
                    (new-action (make-repair-action 'KEEP pos token))
                    (new-score (+ (state-score state)
                                 (score-action 'KEEP #t #f)
                                 (calculate-balance-score
                                   (make-repair-state 0 new-stack '() 0 0 0 '() #f) #f))))
               (set! candidates
                 (cons (make-repair-state
                         (+ pos 1)
                         new-stack
                         (append (state-actions state) (list new-action))
                         new-score
                         token-col
                         token-line
                         next-remaining
                         new-expr-root)
                       candidates))))

           ;; 遷移2: 閉じ括弧を削除
           (let* ((new-action (make-repair-action 'DELETE pos token))
                  (new-score (+ (state-score state) (score-action 'DELETE #f #t))))
             (set! candidates
               (cons (make-repair-state
                       (+ pos 1)
                       (state-stack state)
                       (append (state-actions state) (list new-action))
                       new-score
                       token-col
                       token-line
                       next-remaining
                       current-expr-root)
                     candidates))))

          ;; その他のトークン: そのまま保持
          (else
           (let ((new-action (make-repair-action 'KEEP pos token)))
             (set! candidates
               (cons (make-repair-state
                       (+ pos 1)
                       (state-stack state)
                       (append (state-actions state) (list new-action))
                       (state-score state)
                       token-col
                       token-line
                       next-remaining
                       current-expr-root)
                     candidates)))))

        ;; 遷移3: 任意の位置で閉じ括弧を挿入（スタックが空でない場合のみ）
        ;; 注: インデントが合う位置でのみ挿入を許可
        (when (and (not (null? (state-stack state)))
                   (not (t/paren? token))
                   (not (t/space? token)))  ; 括弧とスペース以外のトークンの前で挿入検討（改行前は許可）
          (let* ((paren-ctx (car (state-stack state)))
                 (open-indent (paren-indent paren-ctx))
                 (open-col (indent-column open-indent))
                 (open-line (indent-line open-indent))
                 (current-col token-col)
                 (current-line token-line)
                 ;; 改行トークンの前で挿入を検討
                 ;; ただし、開き括弧より左にインデントされた位置では挿入しない（リスト内部の要素を尊重）
                 (should-insert? (and (t/newline? token)
                                      (or (= current-line open-line)  ; 同じ行の改行
                                          (>= current-col open-col)))))
            (when should-insert?
              (let* ((new-stack (cdr (state-stack state)))
                     ;; スタックが空になったらexpr-rootをリセット
                     (new-expr-root (if (null? new-stack) #f current-expr-root))
                     (close-paren (t/make-close-paren-token))
                     (new-action (make-repair-action 'INSERT pos close-paren))
                     (indent-score (calculate-indent-score state new-action token-col token-line))
                     (expr-length-score (calculate-expression-length-score state))
                     (new-score (+ (state-score state)
                                  (score-action 'INSERT #f #f)
                                  indent-score
                                  expr-length-score
                                  (calculate-balance-score
                                    (make-repair-state 0 new-stack '() 0 0 0 '() #f) #f))))
                (set! candidates
                  (cons (make-repair-state
                          pos  ; 位置は進めない
                          new-stack
                          (append (state-actions state) (list new-action))
                          new-score
                          (state-current-indent state)  ; インデントは変更しない
                          (state-current-line state)    ; 行も変更しない
                          remaining  ; remaining-tokensは進めない（挿入なので）
                          new-expr-root)
                        candidates))))))

        candidates))

    ;; ========================================
    ;; ビームサーチ
    ;; ========================================

    ;; 上位K個の状態を選択
    (define (select-top-k states k)
      (let ((sorted (sort states (lambda (a b) (> (state-score a) (state-score b))))))
        (if (<= (length sorted) k)
          sorted
          (let loop ((i 0) (result '()) (remaining sorted))
            (if (>= i k)
              (reverse result)
              (loop (+ i 1) (cons (car remaining) result) (cdr remaining)))))))

    ;; 残った開き括弧を閉じる
    (define (close-remaining-parens state)
      (if (null? (state-stack state))
        (list state)
        (let* ((close-paren (t/make-close-paren-token))
               (new-stack (cdr (state-stack state)))
               (new-expr-root (if (null? new-stack) #f (state-expr-root state)))
               (new-action (make-repair-action 'INSERT (state-position state) close-paren))
               (expr-length-score (calculate-expression-length-score state))
               (new-score (+ (state-score state)
                            (score-action 'INSERT #f #f)  ; INSERTペナルティを追加
                            expr-length-score
                            (calculate-balance-score
                              (make-repair-state 0 new-stack '() 0 0 0 '() #f)
                              (null? new-stack)))))
          (close-remaining-parens
            (make-repair-state
              (state-position state)
              new-stack
              (append (state-actions state) (list new-action))
              new-score
              (state-current-indent state)
              (state-current-line state)
              (state-remaining-tokens state)
              new-expr-root)))))

    ;; ビームサーチのメインループ（インデント情報 + remaining-tokens付き）
    (define (beam-search tokens beam-width)
      (let* ((token-positions
               (vector-map
                 (lambda (x) (make-indent-info (car x) (cdr x)))
                 (t/extract-token-positions tokens)))
             (initial-state (make-repair-state 0 '() '() 0.0 0 0 tokens #f))
             (num-tokens (length tokens)))
        (let loop ((beam (list initial-state)))
          ;; すべての状態が最後まで達したかチェック
          (if (every (lambda (state) (>= (state-position state) num-tokens)) beam)
            ;; 最終処理：残った開き括弧を閉じる
            (let ((final-beam
                    (apply append
                           (map close-remaining-parens beam))))
              (car (select-top-k final-beam 1)))
            ;; 次の候補を生成
            (let* ((next-beam
                     (apply append
                            (map (lambda (state)
                                   (if (>= (state-position state) num-tokens)
                                     ;; この状態は既に終了している
                                     (list state)
                                     ;; まだトークンがある（remaining-tokensから取得）
                                     (let ((token (car (state-remaining-tokens state))))
                                       (expand-state state token token-positions))))
                                 beam)))
                   (filtered-beam (select-top-k next-beam beam-width)))
              (loop filtered-beam))))))))
