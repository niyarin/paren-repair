(define-library (paren-repair core)
  (import (scheme base)
          (prefix (scheme-reader core) rdr/)
          (only (chicken sort) sort)
          (only (srfi 1) every))
  (export beam-search
          repair-action-type
          repair-action-value
          state-actions
          state-score)
  (begin
    ;; 括弧関連
    (define (%open-paren? token)
      (and (rdr/lexical? token)
           (eq? (rdr/lexical-type token) 'OPEN-PAREN)))

    (define (%close-paren? token)
      (and (rdr/lexical? token)
           (eq? (rdr/lexical-type token) 'CLOSE-PAREN)))

    (define (%paren? token)
      (or (%open-paren? token)
          (%close-paren? token)))

    ;; 空白文字関連
    (define (%space? token)
      (and (rdr/lexical? token)
           (eq? (rdr/lexical-type token) 'SPACE)))

    (define (%newline? token)
      (and (rdr/lexical? token)
           (eq? (rdr/lexical-type token) 'NEWLINE)))

    (define (%whitespace? token)
      (or (%space? token)
          (%newline? token)))

    ;; コメント
    (define (%comment? token)
      (and (rdr/lexical? token)
           (eq? (rdr/lexical-type token) 'COMMENT)))

    ;; クォート関連
    (define (%quote? token)
      (and (rdr/lexical? token)
           (eq? (rdr/lexical-type token) 'QUOTE)))

    (define (%quasi-quote? token)
      (and (rdr/lexical? token)
           (eq? (rdr/lexical-type token) 'QUASI-QUOTE)))

    (define (%unquote? token)
      (and (rdr/lexical? token)
           (eq? (rdr/lexical-type token) 'UNQUOTE)))

    ;; リテラル
    (define (%string? token)
      (and (rdr/lexical? token)
           (eq? (rdr/lexical-type token) 'STRING)))

    (define (%atom? token)
      (and (rdr/lexical? token)
           (eq? (rdr/lexical-type token) 'ATOM)))

    (define (%keyword? token)
      (and (rdr/lexical? token)
           (eq? (rdr/lexical-type token) 'KEYWORD)))

    ;; その他
    (define (%dot? token)
      (and (rdr/lexical? token)
           (eq? (rdr/lexical-type token) 'DOT)))

    (define (%directive? token)
      (and (rdr/lexical? token)
           (eq? (rdr/lexical-type token) 'DIRECTIVE)))

    (define (%shebang? token)
      (and (rdr/lexical? token)
           (eq? (rdr/lexical-type token) 'SHEBANG)))

    ;; 視覚的なトークン（スペース、改行、コメント、ディレクティブ）
    (define (%visual-token? token)
      (or (%space? token)
          (%newline? token)
          (%comment? token)
          (%directive? token)))

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

    ;; 修正アクション
    (define-record-type <repair-action>
      (make-repair-action action-type position value)
      repair-action?
      (action-type repair-action-type)  ; 'KEEP, 'DELETE, 'INSERT
      (position repair-action-position)
      (value repair-action-value))

    ;; 修正状態（インデント情報付き）
    (define-record-type <repair-state>
      (make-repair-state position paren-stack actions score current-indent current-line)
      repair-state?
      (position state-position)      ; 現在のトークン位置
      (paren-stack state-stack)      ; 開き括弧のスタック (<paren-context>のリスト)
      (actions state-actions)        ; 修正アクション列
      (score state-score)            ; 累積スコア
      (current-indent state-current-indent)  ; 現在のインデント（列番号）
      (current-line state-current-line))     ; 現在の行番号

    ;; ========================================
    ;; スコアリング関数
    ;; ========================================

    ;; バランススコアを計算
    (define (calculate-balance-score state is-final?)
      (let ((stack-depth (length (state-stack state))))
        (cond
          ;; 最終状態で括弧が完全に閉じられている
          ((and is-final? (= stack-depth 0)) 20.0)
          ;; 最終状態でまだ開き括弧が残っている（ペナルティ）
          (is-final? (* -5.0 stack-depth))
          ;; 途中状態：深さが浅いほうが良い
          (else (* 0.5 (- 10 stack-depth))))))

    ;; アクションに対するスコアを計算
    (define (score-action action-type matched?)
      (case action-type
        ((KEEP) (if matched? 10.0 0.0))  ; マッチング成功なら高スコア
        ((DELETE) -0.5)                   ; 削除は軽いペナルティ
        ((INSERT) 0.0)                    ; 挿入はニュートラル
        (else 0.0)))

    ;; ========================================
    ;; 状態遷移関数
    ;; ========================================

    ;; 状態をコピー
    (define (copy-state state)
      (make-repair-state
        (state-position state)
        (list-copy (state-stack state))
        (list-copy (state-actions state))
        (state-score state)))

    ;; 閉じ括弧トークンを生成（簡易版）
    (define (make-close-paren-token)
      (rdr/read-token (open-input-string ")")))

    ;; 状態から次の候補状態を生成
    (define (expand-state state token)
      (let ((candidates '()))
        (cond
          ;; トークンが開き括弧の場合
          ((%open-paren? token)
           ;; 遷移1: 開き括弧を保持
           (let* ((new-stack (cons token (state-stack state)))
                  (new-action (make-repair-action 'KEEP (state-position state) token))
                  (new-score (+ (state-score state)
                               (score-action 'KEEP #f)
                               (calculate-balance-score
                                 (make-repair-state 0 new-stack '() 0) #f))))
             (set! candidates
               (cons (make-repair-state
                       (+ (state-position state) 1)
                       new-stack
                       (append (state-actions state) (list new-action))
                       new-score)
                     candidates)))

           ;; 遷移2: 開き括弧を削除
           (let* ((new-action (make-repair-action 'DELETE (state-position state) token))
                  (new-score (+ (state-score state) (score-action 'DELETE #f))))
             (set! candidates
               (cons (make-repair-state
                       (+ (state-position state) 1)
                       (state-stack state)
                       (append (state-actions state) (list new-action))
                       new-score)
                     candidates))))

          ;; トークンが閉じ括弧の場合
          ((%close-paren? token)
           (if (not (null? (state-stack state)))
             ;; 遷移1: マッチング成功
             (let* ((new-stack (cdr (state-stack state)))
                    (new-action (make-repair-action 'KEEP (state-position state) token))
                    (new-score (+ (state-score state)
                                 (score-action 'KEEP #t)
                                 (calculate-balance-score
                                   (make-repair-state 0 new-stack '() 0) #f))))
               (set! candidates
                 (cons (make-repair-state
                         (+ (state-position state) 1)
                         new-stack
                         (append (state-actions state) (list new-action))
                         new-score)
                       candidates))))

           ;; 遷移2: 閉じ括弧を削除
           (let* ((new-action (make-repair-action 'DELETE (state-position state) token))
                  (new-score (+ (state-score state) (score-action 'DELETE #f))))
             (set! candidates
               (cons (make-repair-state
                       (+ (state-position state) 1)
                       (state-stack state)
                       (append (state-actions state) (list new-action))
                       new-score)
                     candidates))))

          ;; その他のトークン: そのまま保持
          (else
           (let ((new-action (make-repair-action 'KEEP (state-position state) token)))
             (set! candidates
               (cons (make-repair-state
                       (+ (state-position state) 1)
                       (state-stack state)
                       (append (state-actions state) (list new-action))
                       (state-score state))
                     candidates)))))

        ;; 遷移3: 任意の位置で閉じ括弧を挿入（スタックが空でない場合のみ）
        (when (not (null? (state-stack state)))
          (let* ((new-stack (cdr (state-stack state)))
                 (close-paren (make-close-paren-token))
                 (new-action (make-repair-action 'INSERT (state-position state) close-paren))
                 (new-score (+ (state-score state)
                              (score-action 'INSERT #f)
                              (calculate-balance-score
                                (make-repair-state 0 new-stack '() 0) #f))))
            (set! candidates
              (cons (make-repair-state
                      (state-position state)  ; 位置は進めない
                      new-stack
                      (append (state-actions state) (list new-action))
                      new-score)
                    candidates))))

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
        (let* ((close-paren (make-close-paren-token))
               (new-stack (cdr (state-stack state)))
               (new-action (make-repair-action 'INSERT (state-position state) close-paren))
               (new-score (+ (state-score state)
                            (calculate-balance-score
                              (make-repair-state 0 new-stack '() 0)
                              (null? new-stack)))))
          (close-remaining-parens
            (make-repair-state
              (state-position state)
              new-stack
              (append (state-actions state) (list new-action))
              new-score)))))

    ;; ビームサーチのメインループ
    (define (beam-search tokens beam-width)
      (let ((initial-state (make-repair-state 0 '() '() 0.0))
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
                                     ;; まだトークンがある
                                     (let ((token (list-ref tokens (state-position state))))
                                       (expand-state state token))))
                                 beam)))
                   (filtered-beam (select-top-k next-beam beam-width)))
              (loop filtered-beam))))))

    ))
