#lang racket/base
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))
(define-tokens tokens
  (ARG_ACTION
   ARG_OR_CHARSET
   LEXER_CHAR_SET
   RULE_REF
   SEMPRED
   TOKEN_REF
   UNICODE_ESC
   UNICODE_EXTENDED_ESC
   ALT
   BLOCK
   CLOSURE
   ELEMENT_OPTIONS
   EPSILON
   LEXER_ACTION_CALL
   LEXER_ALT_ACTION
   OPTIONAL
   POSITIVE_CLOSURE
   RULE
   RULEMODIFIERS
   RULES
   SET
   WILDCARD
   ; Comments
   DOC_COMMENT
   BLOCK_COMMENT
   LINE_COMMENT
   ; Integer
   INT
   ; Literal string
   STRING_LITERAL
   UNTERMINATED_STRING_LITERAL
   ; Arguments
   BEGIN_ARGUMENT
   ACTION
   ; Keywords
   OPTIONS
   TOKENS
   CHANNELS
   IMPORT
   FRAGMENT
   LEXER
   PARSER
   GRAMMAR
   PROTECTED
   PUBLIC
   PRIVATE
   RETURNS
   LOCALS
   THROWS
   CATCH
   FINALLY
   MODE
   ; Punctuation
   COLON
   COLONCOLON
   COMMA
   SEMI
   LPAREN
   RPAREN
   RBRACE
   RARROW
   LT
   GT
   ASSIGN
   QUESTION
   STAR
   PLUS_ASSIGN
   PLUS
   OR
   DOLLAR
   RANGE
   DOT
   AT
   POUND
   NOT
   ; Identifiers
   ID
   ; Whitespace
   WS))
(define-lex-abbrev NESTED_ACTION
  (:: "{"
      (:? (:* (:or NESTED_ACTION
                   token-STRING_LITERAL
                   DoubleQuoteLiteral
                   TripleQuoteLiteral
                   BacktickQuoteLiteral
                   (:: "/*" (:? (:* any-char)) "*/")
                   (:: "//" (:* (:~ #\return #\newline)))
                   (:: "\\" any-char)
                   (:~ "\\" "\"" "'" "`" "{"))))
      "}"))