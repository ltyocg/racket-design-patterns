# ext-parser — 带扩展语法糖的 Racket Yacc 解析器生成器

`ext-parser` 是一个编译期宏，在 Racket 标准 `parser-tools/yacc` 的基础上增加了三种 EBNF 后缀语法糖（`?`、`*`、`+`），使文法规则更简洁。所有语法糖在宏展开时被降级为纯 yacc 格式，零运行时开销。

## 快速开始

```racket
#lang racket
(require "ext-parser.rkt"
         parser-tools/lex
         parser-tools/yacc)

;; 定义词法 token
(define-tokens value-tokens (NUM))
(define-empty-tokens op-tokens (PLUS LPAREN RPAREN EOF))

;; 用 ext-parser 定义解析器
(define my-parser
  (ext-parser
   [start expr]
   [end EOF]
   [error (lambda args (error 'my-parser "parse error: ~s" args))]
   [src-pos]
   [tokens value-tokens op-tokens]
   [grammar
    [expr
     [(term) $1]
     [(NUM?) $1]]
    [term
     [(NUM? LPAREN* RPAREN+) (list $1 $2 $3)]]]))
```

## 语法糖说明

### `?` — 可选（0 或 1 次）

在非终结符名后加 `?`，表示该成分可选。

```racket
;; 写法
[importDeclaration
 [(IMPORT importDeclaration.2? qualifiedName importDeclaration.4? SEMI) todo]]

;; 展开为两条规则
;; importDeclaration.2 → ε | importDeclaration.2_inner
```

- 空匹配（ε）：返回 `'()`
- 有匹配：返回 `$1`

### `*` — 零或多次重复

在非终结符名后加 `*`，表示 Kleene 星号闭包。

```racket
;; 写法
[compilationUnit.2
 [(importDeclaration*) todo]]

;; 展开为两条规则
;; importDeclaration* → ε               （返回 '()）
;; importDeclaration* → importDeclaration importDeclaration*
;;   （默认动作：cons $1 $2，即构建列表）
```

- 空匹配返回 `'()`
- 递归步骤用 `(cons $1 $2)` 构建列表

### `+` — 一或多次重复

在非终结符名后加 `+`，表示 Kleene 加号闭包。

```racket
;; 写法
[rep1-example
 [(statement+) (list $1)]]

;; 展开为三条规则
;; statement*  → ε                      （返回 '()）
;; statement*  → statement statement*   （cons $1 $2）
;; statement+  → statement statement*   （cons $1 $2）
```

- `+` 本质上是先匹配一个，再跟 `*` 闭包
- 如果对应的 `*` 形式（如 `statement*`）已被用户显式定义，则不会重复生成空规则和递归规则

## 语法糖展开的动作语义

| 标签 | 含义 | 动作 |
|------|------|------|
| `opt-empty` | `?` 的空匹配 | `'()` |
| `opt-some` | `?` 的有匹配 | `$1` |
| `rep0-empty` | `*` 的空匹配 | `'()` |
| `rep0-step` | `*` 的递归步骤 | `(cons $1 $2)` |
| `rep1-step` | `+` 的递归步骤 | `(cons $1 $2)` |

## 与标准 yacc 的差异

| 特性 | `parser` | `ext-parser` |
|------|----------|--------------|
| `?` / `*` / `+` 语法糖 | 不支持 | 支持 |
| 其余子句 | 完全相同 | 透传给 `parser` |

`ext-parser` 仅处理 `grammar` 子句中的语法糖，其余所有子句（`start`、`end`、`error`、`src-pos`、`tokens`、`precs` 等）原样传递给 `parser`。

## 完整子句格式

```racket
(ext-parser
  [start <非终结符>]            ; 起始符号
  [end <token>]                 ; 结束 token
  [error <错误处理过程>]         ; 解析错误回调
  [src-pos]                     ; 可选：启用源位置追踪
  [tokens <token-group> ...]    ; token 定义组
  [grammar                      ; 文法规则（支持 ? * + 语法糖）
   [非终结符
    [(rhs-item ...) action]     ; 自定义动作
    [(rhs-item ...)]]])         ; 默认动作
```

## 语法糖与用户定义规则的交互

当语法糖生成的非终结符名与用户显式定义的规则同名时，`ext-parser` 会检测到冲突并复用用户定义的规则：

```racket
;; 用户显式定义了 NUM*
[expr
 [(NUM+) $1]]
[NUM*
 [() 'user-empty]                       ; 自定义空匹配返回值
 [(NUM NUM*) (cons $1 $2)]]             ; 自定义递归动作

;; 此时 NUM+ 的展开不会重新生成 NUM* 的规则
;; 只生成：NUM+ → NUM NUM*
```

## 实际项目用法示例

本项目中的 Java 解析器（`java/parser.rkt`）大量使用了 `ext-parser`：

```racket
(define java-parser
  (ext-parser
   [start compilationUnit]
   [end EOF]
   [error (lambda (tok-ok? tok-name tok-value start end)
            (error 'java-parser "Parse error at line ~a, col ~a: ~a ~a"
                   (position-line start) (position-col start)
                   tok-name tok-value))]
   [src-pos]
   [tokens empty-tokens tokens]
   [grammar
    ;; ? 用于可选成分
    [typeParameters
     [(LT typeParameter typeParameters.3? GT) todo]]
    ;; * 用于列表
    [compilationUnit.2
     [(importDeclaration*) todo]]
    ;; 混合使用
    [classDeclaration
     [(CLASS identifier typeParameters?
       classDeclaration.4? classDeclaration.5? classDeclaration.6?
       classBody) todo]]]))
```

## 限制

1. 语法糖后缀仅适用于**非终结符**（符号），不能直接用于终结符 token
2. `?` / `*` / `+` 生成的辅助非终结符名即原符号名加后缀（如 `foo?`、`foo*`、`foo+`），用户不可定义同名非终结符用于其他用途

## 模块导出

- `ext-parser`：唯一的导出，语法形式（macro），用法等同于 `parser`
