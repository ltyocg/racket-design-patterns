# ANTLR v4 Parser for Racket - 修复总结

## 修复的文件

### 1. antlr/lexer.rkt
**修复内容：**
- ✅ 添加了 `RULE_REF` 和 `TOKEN_REF` token 类型的区分
  - `RULE_REF`: 小写开头的标识符 (如 `expr`, `term`)
  - `TOKEN_REF`: 大写开头的标识符 (如 `NUMBER`, `WS`)
- ✅ 修复了字符集识别：`[a-z]` 现在正确识别为 `LEXER_CHAR_SET`
- ✅ 改进了字符串和注释处理

### 2. antlr/parser.rkt
**修复内容：**
- ✅ `parserRuleSpec`: 使用 `RULE_REF` 代替通用 `ID`
- ✅ `lexerRuleSpec`: 使用 `TOKEN_REF` 代替通用 `ID`
- ✅ `terminalDef`: 使用 `TOKEN_REF` 引用词法 token
- ✅ `ruleref`: 使用 `RULE_REF` 引用解析器规则
- ✅ `setElement`: 在集合中使用 `TOKEN_REF`
- ✅ `identifier`: 支持三种类型 (`RULE_REF`, `TOKEN_REF`, `ID`)
- ✅ 添加了 `predicateOptions` 支持
- ✅ 所有语法规则都更新为使用正确的 token 类型

### 3. main.rkt
**修复内容：**
- ✅ 改进了词法分析错误处理
- ✅ 修复了 `struct->vector` 的正确用法
- ✅ 改进了 AST 格式化输出
- ✅ 添加了更清晰的测试用例
- ✅ 添加了所有 token 的显示
- ✅ 添加了完整的 AST 结构输出

## 测试结果

### Token 分类测试
```
RULE_REF: expr, term, factor  ✓ (小写开头 - parser 规则)
TOKEN_REF: NUMBER, WS, Expr   ✓ (大写开头 - lexer token/grammar名)
```

### 语法解析测试
```
Grammar: Expr (简单表达式语法)
- Type: grammar
- Rules: 5 (3 parser rules + 2 lexer rules)
- Parse: ✓ 成功
```

## 完整的 AST 结构

Parser 能正确构建完整的 AST，包括：
- Grammar 声明
- Parser 规则 (expr, term, factor)
- Lexer 规则 (NUMBER, WS)
- EBNF 结构 (*, +, ?, |)
- 嵌套块和替代项

## 符合规范

所有修改都严格遵循官方 ANTLR v4 语法规范：
https://github.com/antlr/grammars-v4/blob/master/antlr/antlr4/ANTLRv4Parser.g4

## 使用方法

```bash
# 运行测试
cd D:\Users\tianyu.liu\Documents\GitHub\racket-design-patterns
racket main.rkt

# 或者运行简单测试
cd antlr
racket test-parser.rkt
```

## 当前限制

- ✓ 基本 ANTLR v4 语法：完全支持
- ✓ Parser 规则：完全支持
- ✓ Lexer 规则：完全支持
- ✓ EBNF 操作符：完全支持
- ⚠ 复杂的 ANTLR 内置语法文件：部分支持
  - 某些复杂的字符串/注释组合可能需要进一步优化

## 下一步

可以考虑的改进：
1. 进一步优化 lexer 的字符串处理
2. 添加更多的语法验证
3. 实现 AST 到代码生成器
4. 添加更多测试用例
