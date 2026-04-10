# 需求描述

我希望扩展racket语言中parser-tools/yacc，grammar的语法：

1. 支持部分ebnf能力
    - 顺序，(example1 example2 example3) 转换为：
        [example1~example2~example3 ;; 也可以用其他的名字
         [(example1 example2 example3) $1 $2 $3]]
        > 为防止歧义，符号不能以`~`开头
    - symbol后面加`?`，example? 转换为
        [example?
         [() '()]
         [(example) $1]]
    - ~or：相当于`|`，(~or example1 example2) 转换为：
        [example1$example2 ;; 也可以用其他的名字
         [(example1) $1]
         [(example2) $1]]
    - symbol后面加`*`，(~rep0 example) 转换为：
        [example*
         [() '()]
         [(example example*) (cons $1 $2)]]
    - symbol后面加`+`，(~rep1 example) 转换为：
        [example+
         [(example example*) (cons $1 $2)]]

2. 支持表达式映射$n
    [grammar
     [factor
      [((~opt packageDeclaration) (~rep0 (~or importDeclaration SEMI)) (~rep0 (~or typeDeclaration SEMI)))] (list $1 $2 $3)]]
    映射关系：
    $1 -> (~opt packageDeclaration)
    $2 -> (~rep0 (~or importDeclaration ";"))
    $3 -> (~rep0 (~or typeDeclaration ";"))

写一个宏`ext-parser`代替原来的`parser`

# 理论基础

EBNF “降级”成 BNF

- A = [B]
  改成
  A_opt -> /* empty */ | B
- A = {B}
  改成
  A_list -> /* empty */ | A_list B（或右递归版本）
- A = B+
  改成
  A_list1 -> B | A_list1 B
- A = (B | C) D
  改成新非终结符
  A_tmp -> B | C，然后 A -> A_tmp D

---

expr 里支持的 EBNF 关键字/操作符

- seq：顺序
例：(seq A B C)
- or / alt：选择
例：(or A B) 或 (alt A B)
- ? / opt：可选
例：(? A) 或 (opt A)
- * / rep / rep0：0 次或多次
例：(* A)
- + / rep1：1 次或多次
例：(+ A)
- eps：空串
例：(eps)
- 额外规则：普通列表默认按 seq 处理
例：(A B C) 等价于 (seq A B C)