#lang racket/base

(require "lexer.rkt"
         parser-tools/yacc
         parser-tools/lex)

(provide (all-defined-out))

;; ======================================================
;; AST Node Definitions
;; ======================================================

;; Grammar structure
(struct grammar-spec (type name prequels rules modes) #:transparent)
(struct grammar-decl (type name) #:transparent)

;; Prequel constructs
(struct options-spec (options) #:transparent)
(struct option (name value) #:transparent)
(struct delegate-grammars (grammars) #:transparent)
(struct delegate-grammar (name alias) #:transparent)
(struct tokens-spec (ids) #:transparent)
(struct channels-spec (ids) #:transparent)
(struct action (scope name block) #:transparent)

;; Rules
(struct parser-rule-spec (modifiers name args returns throws locals prequels block exceptions) #:transparent)
(struct lexer-rule-spec (fragment? name options block) #:transparent)
(struct mode-spec (name rules) #:transparent)

;; Rule elements
(struct rule-alt-list (alts) #:transparent)
(struct labeled-alt (alt label) #:transparent)
(struct alternative (options elements) #:transparent)
(struct element (content suffix) #:transparent)
(struct labeled-element (label op content) #:transparent)

;; Blocks and EBNF
(struct block (options actions alts) #:transparent)
(struct ebnf-node (block suffix) #:transparent)
(struct ebnf-suffix-node (type greedy?) #:transparent)

;; Atoms
(struct terminal-def (name options) #:transparent)
(struct ruleref (name args options) #:transparent)
(struct character-range (start end) #:transparent)
(struct not-set (content) #:transparent)
(struct set-element (content options) #:transparent)
(struct wildcard (options) #:transparent)
(struct predicate-options (options) #:transparent)

;; Lexer specific
(struct lexer-alt (elements commands) #:transparent)
(struct lexer-command (name arg) #:transparent)
(struct lexer-block (alts) #:transparent)

;; Exceptions
(struct exception-handler (arg action) #:transparent)
(struct finally-clause (action) #:transparent)

;; Identifiers
(struct qualified-identifier (parts) #:transparent)

;; ======================================================
;; Parser
;; ======================================================

(define antlr4-parser
  (parser
   [start grammar-spec]
   [end EOF]
   [tokens tokens]
   [error (lambda (tok-ok? tok-name tok-value)
            (error 'parser "Parse error: ~a ~v" tok-name tok-value))]

   ;; Precedence and associativity
   [precs (left OR)
          (left DOT)]

   ;; Grammar rules
   [grammar

    ;; grammarSpec: grammarDecl prequelConstruct* rules modeSpec* (EOF handled by parser end)
    [grammar-spec
     [(grammar-decl prequel-constructs rules mode-specs)
      (grammar-spec (grammar-decl-type $1) (grammar-decl-name $1) $2 $3 $4)]]

    ;; grammarDecl: grammarType identifier SEMI
    [grammar-decl
     [(grammar-type identifier SEMI)
      (grammar-decl $1 $2)]]

    ;; grammarType
    [grammar-type
     [(LEXER GRAMMAR) 'lexer-grammar]
     [(PARSER GRAMMAR) 'parser-grammar]
     [(GRAMMAR) 'grammar]]

    ;; prequelConstruct*
    [prequel-constructs
     [() '()]
     [(prequel-construct prequel-constructs) (cons $1 $2)]]

    ;; prequelConstruct
    [prequel-construct
     [(options-spec) $1]
     [(delegate-grammars) $1]
     [(tokens-spec) $1]
     [(channels-spec) $1]
     [(action_) $1]]

    ;; optionsSpec: OPTIONS (option SEMI)* RBRACE
    [options-spec
     [(OPTIONS options-list RBRACE)
      (options-spec $2)]]

    [options-list
     [() '()]
     [(option SEMI options-list) (cons $1 $3)]]

    ;; option: identifier ASSIGN optionValue
    [option
     [(identifier ASSIGN option-value)
      (option $1 $3)]]

    ;; optionValue
    [option-value
     [(identifier) $1]
     [(identifier dotted-ids) (qualified-identifier (cons $1 $2))]
     [(STRING_LITERAL) $1]
     [(ACTION) $1]
     [(INT) $1]]

    [dotted-ids
     [(DOT identifier) (list $2)]
     [(DOT identifier dotted-ids) (cons $2 $3)]]

    ;; delegateGrammars: IMPORT delegateGrammar (COMMA delegateGrammar)* SEMI
    [delegate-grammars
     [(IMPORT delegate-grammar-list SEMI)
      (delegate-grammars $2)]]

    [delegate-grammar-list
     [(delegate-grammar) (list $1)]
     [(delegate-grammar COMMA delegate-grammar-list) (cons $1 $3)]]

    ;; delegateGrammar: identifier ASSIGN identifier | identifier
    [delegate-grammar
     [(identifier ASSIGN identifier) (delegate-grammar $1 $3)]
     [(identifier) (delegate-grammar #f $1)]]

    ;; tokensSpec: TOKENS idList? RBRACE
    [tokens-spec
     [(TOKENS RBRACE) (tokens-spec '())]
     [(TOKENS id-list RBRACE) (tokens-spec $2)]]

    ;; channelsSpec: CHANNELS idList? RBRACE
    [channels-spec
     [(CHANNELS RBRACE) (channels-spec '())]
     [(CHANNELS id-list RBRACE) (channels-spec $2)]]

    ;; idList: identifier (COMMA identifier)* COMMA?
    [id-list
     [(identifier) (list $1)]
     [(identifier COMMA id-list) (cons $1 $3)]
     [(identifier COMMA) (list $1)]]

    ;; action_: AT (actionScopeName COLONCOLON)? identifier actionBlock
    [action_
     [(AT identifier ACTION) (action #f $2 $3)]
     [(AT action-scope-name COLONCOLON identifier ACTION) (action $2 $4 $5)]]

    [action-scope-name
     [(identifier) $1]
     [(LEXER) 'LEXER]
     [(PARSER) 'PARSER]]

    ;; modeSpec: MODE identifier SEMI lexerRuleSpec*
    [mode-specs
     [() '()]
     [(mode-spec mode-specs) (cons $1 $2)]]

    [mode-spec
     [(MODE identifier SEMI lexer-rule-specs)
      (mode-spec $2 $4)]]

    ;; rules: ruleSpec*
    [rules
     [() '()]
     [(rule-spec rules) (cons $1 $2)]]

    ;; ruleSpec: parserRuleSpec | lexerRuleSpec
    [rule-spec
     [(parser-rule-spec) $1]
     [(lexer-rule-spec) $1]]

    ;; parserRuleSpec: ruleModifiers? RULE_REF argActionBlock? ruleReturns? throwsSpec? localsSpec? rulePrequel* COLON ruleBlock SEMI exceptionGroup
    [parser-rule-spec
     [(rule-modifiers RULE_REF arg-action-block rule-returns throws-spec locals-spec rule-prequels COLON rule-block SEMI exception-group)
      (parser-rule-spec $1 $2 $3 $4 $5 $6 $7 $9 $11)]
     [(RULE_REF arg-action-block rule-returns throws-spec locals-spec rule-prequels COLON rule-block SEMI exception-group)
      (parser-rule-spec #f $1 $2 $3 $4 $5 $6 $8 $10)]]

    ;; ruleModifiers: ruleModifier+
    [rule-modifiers
     [(rule-modifier) (list $1)]
     [(rule-modifier rule-modifiers) (cons $1 $2)]]

    [rule-modifier
     [(PUBLIC) 'PUBLIC]
     [(PRIVATE) 'PRIVATE]
     [(PROTECTED) 'PROTECTED]
     [(FRAGMENT) 'FRAGMENT]]

    ;; argActionBlock
    [arg-action-block
     [() #f]
     [(BEGIN_ARGUMENT END_ARGUMENT) '()]
     [(BEGIN_ARGUMENT argument-contents END_ARGUMENT) $2]]

    [argument-contents
     [(ARGUMENT_CONTENT) (list $1)]
     [(ARGUMENT_CONTENT argument-contents) (cons $1 $2)]]

    ;; ruleReturns
    [rule-returns
     [() #f]
     [(RETURNS arg-action-block) $2]]

    ;; throwsSpec
    [throws-spec
     [() #f]
     [(THROWS qualified-identifier-list) $2]]

    [qualified-identifier-list
     [(qualified-identifier) (list $1)]
     [(qualified-identifier COMMA qualified-identifier-list) (cons $1 $3)]]

    ;; localsSpec
    [locals-spec
     [() #f]
     [(LOCALS arg-action-block) $2]]

    ;; rulePrequel*
    [rule-prequels
     [() '()]
     [(rule-prequel rule-prequels) (cons $1 $2)]]

    [rule-prequel
     [(options-spec) $1]
     [(rule-action) $1]]

    ;; ruleAction: AT identifier actionBlock
    [rule-action
     [(AT identifier ACTION) (action #f $2 $3)]]

    ;; ruleBlock: ruleAltList
    [rule-block
     [(rule-alt-list) $1]]

    ;; ruleAltList: labeledAlt (OR labeledAlt)*
    [rule-alt-list
     [(labeled-alt) (rule-alt-list (list $1))]
     [(labeled-alt OR rule-alt-list) (rule-alt-list (cons $1 (rule-alt-list-alts $3)))]]

    ;; labeledAlt: alternative (POUND identifier)?
    [labeled-alt
     [(alternative) (labeled-alt $1 #f)]
     [(alternative POUND identifier) (labeled-alt $1 $3)]]

    ;; exceptionGroup
    [exception-group
     [() '()]
     [(exception-handler-list finally-clause-opt) (append $1 $2)]]

    [exception-handler-list
     [() '()]
     [(exception-handler exception-handler-list) (cons $1 $2)]]

    ;; exceptionHandler: CATCH argActionBlock actionBlock
    [exception-handler
     [(CATCH arg-action-block ACTION)
      (exception-handler $2 $3)]]

    [finally-clause-opt
     [() '()]
     [(finally-clause) (list $1)]]

    ;; finallyClause: FINALLY actionBlock
    [finally-clause
     [(FINALLY ACTION) (finally-clause $2)]]

    ;; lexerRuleSpec: FRAGMENT? TOKEN_REF optionsSpec? COLON lexerRuleBlock SEMI
    [lexer-rule-specs
     [() '()]
     [(lexer-rule-spec lexer-rule-specs) (cons $1 $2)]]

    [lexer-rule-spec
     [(TOKEN_REF COLON lexer-rule-block SEMI)
      (lexer-rule-spec #f $1 #f $3)]
     [(TOKEN_REF options-spec COLON lexer-rule-block SEMI)
      (lexer-rule-spec #f $1 $2 $4)]
     [(FRAGMENT TOKEN_REF COLON lexer-rule-block SEMI)
      (lexer-rule-spec #t $2 #f $4)]
     [(FRAGMENT TOKEN_REF options-spec COLON lexer-rule-block SEMI)
      (lexer-rule-spec #t $2 $3 $5)]]

    ;; lexerRuleBlock: lexerAltList
    [lexer-rule-block
     [(lexer-alt-list) $1]]

    ;; lexerAltList: lexerAlt (OR lexerAlt)*
    [lexer-alt-list
     [(lexer-alt) (list $1)]
     [(lexer-alt OR lexer-alt-list) (cons $1 $3)]]

    ;; lexerAlt: lexerElements lexerCommands? |
    [lexer-alt
     [(lexer-elements) (lexer-alt $1 '())]
     [(lexer-elements lexer-commands) (lexer-alt $1 $2)]
     [() (lexer-alt '() '())]]

    ;; lexerElements: lexerElement+ |
    [lexer-elements
     [(lexer-element) (list $1)]
     [(lexer-element lexer-elements) (cons $1 $2)]
     [() '()]]

    ;; lexerElement
    [lexer-element
     [(lexer-atom) (element $1 #f)]
     [(lexer-atom ebnf-suffix) (element $1 $2)]
     [(lexer-block) (element $1 #f)]
     [(lexer-block ebnf-suffix) (element $1 $2)]
     [(ACTION) (element $1 #f)]
     [(ACTION QUESTION) (element $1 'optional-pred)]]

    ;; lexerBlock: LPAREN lexerAltList RPAREN
    [lexer-block
     [(LPAREN lexer-alt-list RPAREN)
      (lexer-block $2)]]

    ;; lexerCommands: RARROW lexerCommand (COMMA lexerCommand)*
    [lexer-commands
     [(RARROW lexer-command) (list $2)]
     [(RARROW lexer-command COMMA lexer-commands) (cons $2 $4)]]

    ;; lexerCommand
    [lexer-command
     [(lexer-command-name) (lexer-command $1 #f)]
     [(lexer-command-name LPAREN lexer-command-expr RPAREN) (lexer-command $1 $3)]]

    [lexer-command-name
     [(identifier) $1]
     [(MODE) 'MODE]]

    [lexer-command-expr
     [(identifier) $1]
     [(INT) $1]]

    ;; lexerAtom
    [lexer-atom
     [(character-range) $1]
     [(terminal-def) $1]
     [(not-set) $1]
     [(LEXER_CHAR_SET) $1]
     [(wildcard) $1]]

    ;; atom
    [atom
     [(terminal-def) $1]
     [(ruleref) $1]
     [(not-set) $1]
     [(wildcard) $1]]

    ;; terminalDef: TOKEN_REF elementOptions? | STRING_LITERAL elementOptions?
    [terminal-def
     [(TOKEN_REF) (terminal-def $1 '())]
     [(TOKEN_REF element-options) (terminal-def $1 $2)]
     [(STRING_LITERAL) (terminal-def $1 '())]
     [(STRING_LITERAL element-options) (terminal-def $1 $2)]]

    ;; ruleref: RULE_REF argActionBlock? elementOptions?
    [ruleref
     [(RULE_REF) (ruleref $1 #f '())]
     [(RULE_REF arg-action-block) (ruleref $1 $2 '())]
     [(RULE_REF element-options) (ruleref $1 #f $2)]
     [(RULE_REF arg-action-block element-options) (ruleref $1 $2 $3)]]

    ;; characterRange: STRING_LITERAL RANGE STRING_LITERAL
    [character-range
     [(STRING_LITERAL RANGE STRING_LITERAL)
      (character-range $1 $3)]]

    ;; notSet: NOT setElement | NOT blockSet
    [not-set
     [(NOT set-element) (not-set $2)]
     [(NOT block-set) (not-set $2)]]

    ;; blockSet: LPAREN setElement (OR setElement)* RPAREN
    [block-set
     [(LPAREN set-element set-element-list RPAREN)
      (cons $2 $3)]]

    [set-element-list
     [() '()]
     [(OR set-element set-element-list) (cons $2 $3)]]

    ;; setElement: TOKEN_REF elementOptions? | STRING_LITERAL elementOptions? | characterRange | LEXER_CHAR_SET
    [set-element
     [(TOKEN_REF) (set-element $1 '())]
     [(TOKEN_REF element-options) (set-element $1 $2)]
     [(STRING_LITERAL) (set-element $1 '())]
     [(STRING_LITERAL element-options) (set-element $1 $2)]
     [(character-range) $1]
     [(LEXER_CHAR_SET) $1]]

    ;; wildcard: DOT elementOptions?
    [wildcard
     [(DOT) (wildcard '())]
     [(DOT element-options) (wildcard $2)]]

    ;; ebnf: block blockSuffix?
    [ebnf
     [(block) (ebnf-node $1 #f)]
     [(block ebnf-suffix) (ebnf-node $1 $2)]]

    ;; blockSuffix: ebnfSuffix
    ;; ebnfSuffix: QUESTION QUESTION? | STAR QUESTION? | PLUS QUESTION?
    [ebnf-suffix
     [(QUESTION) (ebnf-suffix-node 'optional #f)]
     [(QUESTION QUESTION) (ebnf-suffix-node 'optional #t)]
     [(STAR) (ebnf-suffix-node 'zero-or-more #f)]
     [(STAR QUESTION) (ebnf-suffix-node 'zero-or-more #t)]
     [(PLUS) (ebnf-suffix-node 'one-or-more #f)]
     [(PLUS QUESTION) (ebnf-suffix-node 'one-or-more #t)]]

    ;; block: LPAREN (optionsSpec? ruleAction* COLON)? altList RPAREN
    [block
     [(LPAREN alt-list RPAREN) (block #f '() $2)]
     [(LPAREN options-spec alt-list RPAREN) (block $2 '() $3)]
     [(LPAREN rule-actions COLON alt-list RPAREN) (block #f $2 $4)]
     [(LPAREN options-spec rule-actions COLON alt-list RPAREN) (block $2 $3 $5)]]

    [rule-actions
     [() '()]
     [(rule-action rule-actions) (cons $1 $2)]]

    ;; altList: alternative (OR alternative)*
    [alt-list
     [(alternative) (list $1)]
     [(alternative OR alt-list) (cons $1 $3)]]

    ;; alternative: elementOptions? element+ |
    [alternative
     [(elements) (alternative '() $1)]
     [(element-options elements) (alternative $1 $2)]
     [() (alternative '() '())]]

    [elements
     [(element) (list $1)]
     [(element elements) (cons $1 $2)]]

    ;; element: labeledElement ebnfSuffix? | atom ebnfSuffix? | ebnf | actionBlock QUESTION? predicateOptions?
    [element
     [(labeled-element) $1]
     [(labeled-element ebnf-suffix) (element $1 $2)]
     [(atom) (element $1 #f)]
     [(atom ebnf-suffix) (element $1 $2)]
     [(ebnf) $1]
     [(ACTION) (element $1 #f)]
     [(ACTION QUESTION) (element $1 'optional-pred)]
     [(ACTION QUESTION predicate-options) (element (list $1 'optional-pred $3) #f)]]

    ;; labeledElement: identifier (ASSIGN | PLUS_ASSIGN) (atom | block)
    [labeled-element
     [(identifier ASSIGN atom) (labeled-element $1 'assign $3)]
     [(identifier ASSIGN block) (labeled-element $1 'assign $3)]
     [(identifier PLUS_ASSIGN atom) (labeled-element $1 'plus-assign $3)]
     [(identifier PLUS_ASSIGN block) (labeled-element $1 'plus-assign $3)]]

    ;; elementOptions: LT elementOption (COMMA elementOption)* GT
    [element-options
     [(LT element-option-list GT) $2]]

    [element-option-list
     [(element-option) (list $1)]
     [(element-option COMMA element-option-list) (cons $1 $3)]]

    ;; elementOption
    [element-option
     [(qualified-identifier) $1]
     [(identifier ASSIGN qualified-identifier) (option $1 $3)]
     [(identifier ASSIGN STRING_LITERAL) (option $1 $3)]
     [(identifier ASSIGN INT) (option $1 $3)]]

    ;; predicateOptions: LT elementOption (COMMA elementOption)* GT
    [predicate-options
     [(LT element-option-list GT) (predicate-options $2)]]

    ;; identifier: RULE_REF | TOKEN_REF
    [identifier
     [(RULE_REF) $1]
     [(TOKEN_REF) $1]
     [(ID) $1]]

    ;; qualifiedIdentifier: identifier (DOT identifier)*
    [qualified-identifier
     [(identifier) (qualified-identifier (list $1))]
     [(identifier qualified-id-tail) (qualified-identifier (cons $1 $2))]]

    [qualified-id-tail
     [(DOT identifier) (list $2)]
     [(DOT identifier qualified-id-tail) (cons $2 $3)]]]))

;; ======================================================
;; Parse function
;; =======================================================

(define (parse port)
  (port-count-lines! port)
  ;; Need to wrap lexer to extract token from position-token
  (antlr4-parser
   (lambda ()
     (let* ([pt (antlr4-lexer port)]
            [t (position-token-token pt)])
       ;; Return just the token (symbol or token struct)
       (if (token? t)
           t
           t)))))

(define (parse-string str)
  (parse (open-input-string str)))

(define (parse-file path)
  (call-with-input-file path parse))
