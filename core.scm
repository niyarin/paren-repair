(define-library (paren-repair core)
  (import (scheme base)
          (prefix (scheme-reader core) rdr/))
  (export )
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

    ))
