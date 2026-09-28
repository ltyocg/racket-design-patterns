# ANTLR Java Parser Sidecar

这个目录放的是一套 ANTLR 版本的 Java parser sidecar。当前
[../parser.rkt](../parser.rkt) 默认调用这套实现

## 构建

```sh
racket java/antlr/build.rkt
```

构建脚本要求 `PATH` 里能找到 `java`、`javac` 和 `curl`。它会使用
`grammar/` 里签入的 `.g4` 文件；如果本地还没有
`antlr-4.13.2-complete.jar`，会下载到 `cache/`；随后把 ANTLR 生成的
Java 源码放到 `build/generated/`，并把编译后的 class 文件放到
`build/classes/`。

如果访问 GitHub 或 antlr.org 需要代理，可以这样构建：

```sh
HTTP_PROXY=http://127.0.0.1:7897 HTTPS_PROXY=http://127.0.0.1:7897 racket java/antlr/build.rkt
```

## 命令行

`parser.rkt` 可以直接作为命令行入口使用：

```sh
racket java/parser.rkt /path/to/File.java
```

默认会输出 `ast.rkt` 的 AST。大文件输出会很长，如果只是想检查语法是否能
通过，使用：

```sh
racket java/parser.rkt --check /path/to/File.java
```

如果要查看底层 ANTLR parse tree：

```sh
racket java/parser.rkt --raw /path/to/File.java
```

默认模式不包含 ANTLR hidden channel 里的空白和注释。如果要把 `WS`、
`COMMENT`、`LINE_COMMENT` 一起输出：

```sh
racket java/parser.rkt --include-hidden /path/to/File.java
racket java/parser.rkt --raw --include-hidden /path/to/File.java
```

## Racket API

默认 API 在 `parser.rkt`：

```racket
(require "../parser.rkt")

(parse-java-code "class C { int x; }")
(parse-java-file "/path/to/File.java")

(parse-java-code "class C { /* comment */ int x; }" #:include-hidden? #t)
(parse-java-file "/path/to/File.java" #:include-hidden? #t)
```

它们返回 `ast.rkt` 里的通用 AST：

```racket
(struct ast-node (kind children) #:transparent)
(struct ast-token (kind text line column start stop channel) #:transparent)
```

规则节点的 `kind` 是 ANTLR rule name，token 节点的 `kind` 是 ANTLR token
name，`channel` 通常是 `'DEFAULT` 或 `'HIDDEN`。这个 AST 层故意不再维持旧
parser-tools 版本里“一条 Java 语法规则一个 Racket struct”的模型，因为
现在完整 Java 语法由 `.g4` 文件负责维护。

如果需要调试原始 ANTLR parse tree，可以使用：

```racket
(require "../antlr-parser.rkt")

(parse-java-code/antlr "class C { int x; }")
(parse-java-file/antlr "/path/to/File.java")

(parse-java-code/antlr "class C { /* comment */ int x; }" #:include-hidden? #t)
(parse-java-file/antlr "/path/to/File.java" #:include-hidden? #t)
```

或者从 `parser.rkt` 使用：

```racket
(parse-java-parse-tree "class C { int x; }")
(parse-java-file-parse-tree "/path/to/File.java")

(parse-java-parse-tree "class C { /* comment */ int x; }" #:include-hidden? #t)
(parse-java-file-parse-tree "/path/to/File.java" #:include-hidden? #t)
```

这些接口返回的是 Racket 可以直接 `read` 的 ANTLR parse tree
S-expression。`parser.rkt` 的默认入口会在这个基础上调用
`parse-tree->ast`。
