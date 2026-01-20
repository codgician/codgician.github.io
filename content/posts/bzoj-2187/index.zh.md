---
title: "BZOJ 2187: fraction"
date: 2018-10-28
language: zh
canonical: true
translationKey: bzoj-2187
tags:
  - icpc-solutions
  - algorithm
  - bzoj
  - competitive-programming
  - mathematics
  - number-theory
  - quasi-euclidean-algorithm
math: true
toc: false
draft: false
---


# 题面

给定四个正整数 $a, b, c, d$，试求最简正分数 $\frac{p}{q}$，满足：

$$
\frac{a}{b} < \frac{p}{q} < \frac{c}{d}
$$

若有多组解，取 $q$ 最小的一组解；若仍有多组解，取 $p$ 最小的一组解。

**数据范围**：$1 \le a, b, c, d \le 10^9$，数据组数 $T \le 500$，保证一定有解。

**题目链接**：[BZOJ 2187: fraction](https://darkbzoj.tk/problem/2187)

# 分析

我们记原问题的解为 $f(a, b, c, d) = \langle p, q \rangle$。

我们先来讨论两种可以直接得出答案的情况。

首先，如果 $\frac{a}{b}$ 和 $\frac{c}{d}$ 间存在正整数，那么显然取 $q = 1$。

正式地说，如果满足：

$$
\begin{cases}
\left\lfloor \frac{c}{d} \right\rfloor - \left\lfloor \frac{a}{b} \right\rfloor \ge 1 & c \not\equiv 0 \pmod d \\
\frac{c}{d} - 1 - \left\lfloor \frac{a}{b} \right\rfloor \ge 1 & c \equiv 0 \pmod d
\end{cases}
$$

那么我们取 $p = \left\lfloor \frac{a}{b} \right\rfloor + 1, q = 1$ 为解。

其次，如果 $a = 0$（虽然本题中不会直接出现这一情况，但我们需要解决的子问题中可能会出现），那么很显然我们可以取 $p = 1, q = \left\lfloor \frac{c}{d} \right\rfloor + 1$ 为解。

---

对于剩下不能直接得出答案的情况，我们可以考虑将问题逐步转化至上面可直接得解的情况从而递归地求解。

若 $a \le b$ 且 $c \le d$ ，我们可以考虑对问题进行转换：

$$
\frac{a}{b} < \frac{p}{q} < \frac{c}{d} \Rightarrow \frac{d}{c} < \frac{q}{p} < \frac{b}{a}
$$

我们对子问题 $f(d, c, b, a) = \langle p', q' \rangle$ 进行求解，并在回溯的时候令 $p = q', q = p'$ 即可。

接下来我们就只需要考虑 $c > d$ 的情况了。如果此时 $\frac{a}{b}$ 和 $\frac{c}{d}$ 间存在正整数，那么直接回溯即可。如果不存在，考虑对问题进行如下变换从而尽量缩小 $a$：

$$
\begin{aligned}
& \frac{a}{b} < \frac{p}{q} < \frac{c}{d} \\
& \Rightarrow \frac{a}{b} - \left\lfloor \frac{a}{b} \right\rfloor < \frac{p}{q} -\left\lfloor \frac{a}{b} \right\rfloor < \frac{c}{d} - \left\lfloor \frac{a}{b} \right\rfloor \\
& \Rightarrow \frac{a - b\left\lfloor \frac{a}{b} \right\rfloor}{b} < \frac{p - q\left\lfloor \frac{a}{b} \right\rfloor}{q} < \frac{c - d\left\lfloor \frac{a}{b} \right\rfloor}{d} \\
& \Rightarrow \frac{a \bmod b}{b} < \frac{p - q\left\lfloor \frac{a}{b} \right\rfloor}{q} < \frac{c - d\left\lfloor \frac{a}{b} \right\rfloor}{d} \\
\end{aligned}
$$

由此对子问题 $f(a \bmod b, b, c - d\left\lfloor \frac{a}{b} \right\rfloor, d) = \langle p'', q'' \rangle$ 求解，并在回溯的时候令 $p = p'' + q''\left\lfloor \frac{a}{b} \right\rfloor, q = q''$ 即可。

这就跟欧几里德算法很类似了…… 每常数步我们都可以将 $a$ 的规模缩小至 $a \bmod b$，故复杂度为 $\mathcal{O}(\log{n})$。不过为了防止中间爆 `int64` 我们在运算过程中可以约一下分，这样复杂度就变成了 $\mathcal{O}(\log^2{n})$。

最后附上我的 [代码](https://github.com/codgician/Competitive-Programming/blob/master/BZOJ/2187/quasi_euclidean.cpp) 以供参考。
