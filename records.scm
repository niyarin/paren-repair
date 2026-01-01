(define-library (paren-repair records)
  (import (scheme base))
  (export make-indent-info indent-column indent-line
          make-paren-context paren-token paren-indent paren-prev-object-pos paren-element-count paren-accepts-arbitrary-expr paren-pda-state
          make-expr-root-context expr-root-paren-context expr-root-tokens-tail
          make-repair-action repair-action-type repair-action-position repair-action-value
          make-repair-state state-position state-stack state-actions
          state-score  state-current-indent state-current-line state-remaining-tokens
          state-expr-root state-in-fail-syntax state-pda-state)
  (begin
    (define-record-type <indent-info>
      (make-indent-info column line-number)
      indent-info?
      (column indent-column)         ; 0-based
      (line-number indent-line))     ; 0-based

    (define-record-type <paren-context>
      (make-paren-context token indent prev-object-pos element-count accepts-arbitrary-expr pda-state)
      paren-context?
      (token paren-token)            ; 括弧トークン
      (indent paren-indent)          ; その括弧のインデント位置 (<indent-info>)
      (prev-object-pos paren-prev-object-pos paren-prev-object-pos-set!)  ; 直前のオブジェクト位置 (#f or 位置)
      (element-count paren-element-count paren-element-count-set!)  ; リスト内の要素数
      (accepts-arbitrary-expr paren-accepts-arbitrary-expr)  ; 任意の式を受け入れるか
      (pda-state paren-pda-state))  ; 遷移後のPDA状態

    (define-record-type <expr-root-context>
      (make-expr-root-context paren-ctx tokens-tail)
      expr-root-context?
      (paren-ctx expr-root-paren-context)    ; ルートの開き括弧
      (tokens-tail expr-root-tokens-tail))   ; トークン列へのポインタ

    (define-record-type <repair-action>
      (make-repair-action action-type position value)
      repair-action?
      (action-type repair-action-type)  ; 'KEEP, 'DELETE, 'INSERT
      (position repair-action-position)
      (value repair-action-value))

    (define-record-type <repair-state>
      (make-repair-state position paren-stack actions score current-indent current-line remaining-tokens expr-root in-fail-syntax pda-state)
      repair-state?
      (position state-position)      ; 現在のトークン位置
      (paren-stack state-stack)      ; 開き括弧のスタック (<paren-context>のリスト)
      (actions state-actions)        ; 修正アクション列
      (score state-score)            ; 累積スコア
      (current-indent state-current-indent)  ; 現在のインデント（列番号）
      (current-line state-current-line)      ; 現在の行番号
      (remaining-tokens state-remaining-tokens)  ; まだ処理していないトークン列
      (expr-root state-expr-root)    ; 現在修正中の式のルート (<expr-root-context> or #f)
      (in-fail-syntax state-in-fail-syntax)  ; 構文エラー状態かどうか (#f or #t)
      (pda-state state-pda-state))   ; 構文チェッカーのPDA状態
    ))
