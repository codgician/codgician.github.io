---
title: "Church 编码学习笔记"
date: 2020-04-06
language: zh
canonical: true
translationKey: church-encoding
tags:
  - functional-programming
math: true
toc: false
draft: false
---


# 简介

既然 $\lambda$ 演算与图灵机等价，那么理论上图灵机能干的事情都能通过 $\lambda$ 演算表示。

考虑一个不支持任何除了 $\lambda$ 表达式的编程语言…… 假设我们要在其中建立一个支持计数，并且支持各种运算的表示系统，该如何只用 $\lambda$ 表达式来构造它？更进一步，是否可以仅用 $\lambda$ 表达式来构造诸如链表的数据结构？

Church 编码便能解决这一问题，其可以定义与布尔数等价的 Church Boolean，借助皮亚诺公理 (Peano Axioms) 能够定义出与自然数等价的 Church Numeral，甚至还可以定义出 Church Pairs 和 Church List…… 本文作为自己在学习 Church 编码所记录的笔记，可能有很多不合适的地方，等到以后自己有更加深入的理解后再来修改吧……

本文会随自己学习精度慢慢（~~咕咕~~）更新……

# Church Booleans

## 定义

首先来考虑构造真和假。既然有两样东西，考虑选择二元函数：对于真所代表的函数，它总是返回第一个参数；而对于否所代表的函数，它总是返回第二个参数。于是：

$$
\begin{aligned}
true & = \lambda x \ . \ \lambda y \ . \ x \\
false & = \lambda x \ . \ \lambda y \ . \ y
\end{aligned}
$$

发现这样构造带来一个非常有趣的性质：Church Boolean 似乎内置了 $\text{if - then - else}$ 这种表达形式。考虑如下 $\lambda$ 表达式：

$$
b \enspace T \enspace F
$$

其中 $b$ 是一个 Church Boolean。发现其实际含义就是，若 $b$ 为真，则结果为 $T$，否则结果为 $F$，跟 $\text{if } \ b \ \text{ then } \ T \ \text{ else } \ F$ 实际上没有什么区别。所以实际上可以定义自己的条件分支表达式：

$$
iff = \lambda b \ . \ \lambda t \ . \ \lambda f \ . \ b \enspace t \enspace f 
$$

## 运算

可以应用上面提到的性质比较轻松地定义出所有的布尔逻辑运算……

### 非运算

先来看最简单的非运算。非运算是一个单参数的函数，对于 Church Boolean $b$ 可以表示成 $\text{ if } \ b \ \text{ then } \ true \ \text{else} \ false$，因此可以写作：

$$
not = \lambda b \ . \ false \enspace true
$$

### 与运算

与运算则是一个双参数的函数，当且仅当两参数 $a, b$ 都为 $true$ 是才会返回 $true$，否则返回 $false$。可以表示成 $\text{ if } \ a \ \text{ then } ( \ \text{ if } \ b \ \text{ then } \ true \ \text{ else } \ false  \ ) \ \text { else } \ false$，因此：

$$
and = \lambda a \ . \ \lambda b \ . \ a \enspace (b \enspace true \enspace false) \enspace false
$$

### 或运算

或运算也是一个双参数的函数，只要两参数 $a, b$ 中存在一个 $true$ 即返回 $true$，否则返回 $false$。可以表示成 $\text{ if } \ a \ \text{ then } \ true \ \text{else} \ ( \ \text{ if } \ b \ \text{ then } \ true \ \text{ else } \ false \ )$，因此：

$$
or = \lambda a \ . \ \lambda b \ . \ a \enspace true \enspace (b \enspace true \enspace false)
$$

### 其他？

诸如异或、同或等运算已经可以由上述三种运算组合起来表示了。

## 实现

用 Haskell 实现（ [CodeWars: Church Booleans](https://www.codewars.com/kata/5ac739ed3fdf73d3f0000048)）：

```haskell
{-# Language RankNTypes #-}

import Prelude hiding (Bool, False, True, not, and, or, (&&), (||), (==), (/=))

type Boolean = forall a. a -> a -> a 

false, true :: Boolean
false = \ t f -> f
true  = \ t f -> t

not :: Boolean -> Boolean
and, or, xor :: Boolean -> Boolean -> Boolean

not = \ a   -> a false true
and = \ a b -> a (b true false) false
or  = \ a b -> a true (b true false)
```

# Church Maybe



# Church Numerals

## Peano Axioms

在讨论 Church 数前，先来看看自然数是怎么被定义的。显然可以递归地定义自然数（类似于数学归纳法），首先定义 $0$，其次对于任意自然数 $n$ 定义一个能获得其后继元素的函数 $S(n)$（对于自然数而言，显然 $S(n) = n + 1$）就行了。严谨地说，定义自然数集 $\mathbb{N}$ 的过程如下：

- $0 \in \mathbb{N}$
- 定义等价关系 (equality relation)：
  - **自反性**：$\forall x \in \mathbb{N},\ x = x$ 
  - **对称性**：$\forall x, y \in \mathbb{N},\ x = y \Rightarrow y = x$
  - **传递性**：$\forall x, y, z \in \mathbb{N},\ x = y,\ y = z \Rightarrow x = z$
  - **封闭性**：$\forall x \in \mathbb{N},\ x = y \Rightarrow y \in \mathbb{N}$
- 定义算术性质 (arithmetical properties)：
  - $\forall n \in \mathbb{N},\ S(n) \in \mathbb{N}$
  - $\forall m, n \in \mathbb{N},\ m = n \Leftrightarrow S(m) = S(n)$
  - $\forall n \in \mathbb{N},\ S(n) \neq 0$
- 归纳公理 (induction axiom)：
  - 若 $\mathbb{K}$ 是一个集合，满足：
    - $0 \in \mathbb{K}$ 
    - $\forall n \in \mathbb{K},\ S(n) \in \mathbb{K}$
  - 则：$\mathbb{N} \subseteq \mathbb{K}$

## 定义

既然只要有了后继函数和 $0$ 就可以表示出所有的自然数，那么依然考虑用二元函数来表述 Church 数，形如：

$$
\lambda f \ . \ \lambda x \ . \ something
$$

其中第一个参数 $f$ 可以看作后继函数，第二个参数 $x$ 可看作 $0$ 。由此可以很方便地使用这个式子定义 $0$ ：

$$
C_0 = \lambda f \ . \ \lambda x \ . \ x
$$

可以看到，不论我们传递给它什么后继函数，它所返回的结果都是 $0$ 。进一步，也可以比较轻松地定义：

$$
\begin{aligned}
C_1 & = \lambda f \ . \ \lambda x \ . \ f \enspace x \\
C_2 & = \lambda f \ . \ \lambda x \ . \ f \enspace (f \enspace x) \\
C_3 & = \lambda f \ . \ \lambda x \ . \ f \enspace (f \enspace (f \enspace x)) \\
& \dots
\end{aligned}
$$

可见，我们可以通过 Church Numeral 得到与自然数等价的任何事物。例如对于自然数 $n$，有：

$$
n = C_n \enspace (+1) \enspace 0
$$

## 基础运算

### 后继元素

在尝试解决加法前我们先解决一个看起来简单一些的问题：以 $C_n$ 为参数，而以 $C_{n + 1}$ 为返回值的函数 $succ$ 应当如何定义？

如果我们能够定义出 $succ$，那么将 $succ$ 作用 $b$ 次于 $C_a$ 即可达到 $C_a + C_b$ 的效果，更进一步，如果定义出了加法，则将 $(+ C_a)$ 作用 $b$ 次于 $0$ 即可达到 $C_a \cdot C_b$ 的效果。

为了方便理解，先继续以自然数 $n$ 作为例子：

$$
\begin{aligned}
n + 1 
& = C_{n + 1} \enspace (+1) \enspace 0 \\ 
& = C_n \enspace (+1) \enspace 1 \\
& = C_n \enspace (+1) \enspace (C_1 \enspace (+1) \enspace 0) \\
\end{aligned}
$$

这启示我们，当 $succ$ 作用于 $C_n$ 时，只需要保持后继函数 $f$ 不变，同时将零元由 $x$ 变为 $f \enspace x$ 就可以了。因此：

$$
succ = \lambda M \ . \ \lambda f \ . \ \lambda x \ . \ f \enspace (M \enspace f \enspace x)
$$

举个例子，试试用它作用于 $C_0$：

$$
\begin{aligned}
succ \enspace C_0 
& = (\lambda M \ . \ \lambda f \ . \ \lambda x \ . \ f \enspace (M \enspace f \enspace x)) \enspace \lambda f \ . \ \lambda x \ . \ x \\
& = (\lambda f \ . \ \lambda x \ . \ f \enspace (M \enspace f \enspace x)) \enspace [M := \lambda f \ . \ \lambda x \ . \ x] \\
& = (\lambda g \ . \ \lambda y \ . \ g \enspace (M \enspace g \enspace y)) \enspace [M := \lambda f \ . \ \lambda x \ . \ x] \\
& = (\lambda g \ . \ \lambda y \ . \ g \enspace ((\lambda f \ . \ \lambda x \ . \ x) \enspace g \enspace y)) \\
& = \lambda g \ . \ \lambda y \ . \ g \enspace y \\
& = \lambda f \ . \ \lambda x \ . \ f \enspace x \\
& = C_1
\end{aligned}
$$

### 加法

前面成功定义出了 $succ$，但还留下了一个问题：怎么表示 $\underbrace{succ \circ succ \circ \dots \circ succ}_{b \text{ times}}$（下面简写为 $succ^b$）？

$$
\begin{aligned}
succ^b
& = [ \lambda M \ . \ \lambda f \ . \ \lambda x \ . \ f \enspace (M \enspace f \enspace x) ]^b \\
& = \lambda x \ . \ f^b  \enspace x \enspace [ f := \lambda M \ . \ \lambda f \ . \ \lambda x \ . \ f \enspace (M \enspace f \enspace x) ] \\
& = (\lambda f \ . \ \lambda x \ . \ f^b \enspace x) \enspace \lambda M \ . \ \lambda f \ . \ \lambda x \ . \ f \enspace (M \enspace f \enspace x) \\
& = C_b \enspace succ
\end{aligned}
$$

简要推广，对于任意一元 Church Numeral 函数 $f$，都有：

$$
f^b = C_b \enspace f
$$

于是对于加法：

$$
\begin{aligned}
C_a + C_b
& = (+ C_b) \enspace C_a \\
& = (C_b \enspace succ) \enspace C_a 
\end{aligned} 
$$

---

我们也可以不考虑 $succ$，换一个角度考虑加法：其可看作二元函数，输入 $C_a, C_b$ 并且返回 $C_{a + b}$。依然以自然数 $a, b$ 举例：

$$
\begin{aligned}
a + b
& = C_{a + b} \enspace (+1) \enspace 0 \\
& = C_a \enspace (+1) \enspace b \\
& = C_a \enspace (+1) \enspace (C_b \enspace (+1) \enspace 0)
\end{aligned}
$$

故将 $C_a$ 作用于后继函数 $f$，同时将 $C_b$ 作用于零元 $x$ 即可得到 $C_{a + b}$：

$$
(+) = \lambda M \ . \ \lambda N \ . \ \lambda f \ . \ \lambda x \ . \ M \enspace f \enspace (N \enspace f \enspace x)
$$

### 乘法

与之前类似地，可以借助加法表示乘法，将 $(+ C_a)$ 作用 $b$ 次于 $C_0$ 即可：

$$
C_a \cdot C_b = C_b \enspace  (+ C_a) \enspace C_0
$$

---

若不借助加法，可考虑：乘法运算以 $C_a, C_b$ 为参数，并返回 $C_{a \cdot b}$。依然以自然数 $a, b$ 举例：

$$
\begin{aligned}
a \cdot b 
& = C_{a \cdot b} \enspace (+1) \enspace 0 \\
& = C_a \enspace (+b) \enspace 0 \\
& = C_a \enspace (C_b \enspace (+1)) \enspace 0
\end{aligned}
$$

这启示我们，只需要将 $C_a, C_b$ 依次作用于后继函数 $f$，并且保持零元 $x$ 不变即可：

$$
(\cdot) = \lambda M \ . \ \lambda N \ . \ \lambda f \ . \ \lambda x \ . \ M \enspace (N \enspace f) \enspace x 
$$

### 幂运算

与自然数的情况类似，幂运算还是得借助乘法实现，将 $(\cdot C_a)$ 作用 $b$ 次于 $C_1$ 即可：

$$
C_a \hat{\enspace} C_b = C_b \enspace (\cdot C_a) \enspace C_1
$$

## 实现

用 Haskell 实现（ [CodeWars: Church Numbers](https://www.codewars.com/kata/546dbd81018e956b51000077)）：

```haskell
{-# LANGUAGE RankNTypes #-}

import Prelude hiding (succ)

newtype Number = Nr (forall a. (a -> a) -> a -> a)

zero :: Number
zero = Nr (\ _ x -> x)

succ :: Number -> Number
succ (Nr a) = Nr (\ f x -> f (a f x))

one :: Number
one = succ zero

add :: Number -> Number -> Number
add (Nr a) = a succ
-- add (Nr a) (Nr b) = Nr (\ f x -> a f (b f x))

mult :: Number -> Number -> Number
mult (Nr a) b =  a (add b) zero
-- mult (Nr a) (Nr b) = Nr (\ f x -> a (b f) x)

pow :: Number -> Number -> Number
pow x (Nr n) = n (mult x) one
```

## 减法？

对于 Church Numeral 的减法是一个非常有趣且不简单的问题。先考虑较简单的情况：对于以 $C_n$ 为参数，$C_{n - 1}$ 为结果的函数 $pred$ 应当如何定义（即求前驱元素）。

很快发现一个问题：$pred \enspace C_0 = ?$ 为了保证 $pred$ 是 well-defined 的，则：

$$
pred \enspace C_n = 
\begin{cases}
C_0 & n = 0 \\
C_{n - 1} & \text{otherwise}
\end{cases}
$$



# Refs

- [Comp 311 - Review 2](https://www.cs.rice.edu/~javaplt/311/Readings/supplemental.pdf)
- [Church Encoding by Mark Seemann](https://blog.ploeh.dk/2018/05/22/church-encoding/)
- [Peano Aximos - Wikipedia](https://en.wikipedia.org/wiki/Peano_axioms)
