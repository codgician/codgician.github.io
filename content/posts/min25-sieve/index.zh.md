---
title: "浅谈 Min25 筛"
date: 2019-09-01
language: zh
canonical: true
translationKey: min25-sieve
tags:
  - icpc-notes
  - algorithm
  - arithmetic-function
  - competitive-programming
  - mathematics
  - number-theory
math: true
toc: false
draft: false
---


那对于常见的一些数论函数呢？

欧拉函数 $\varphi$：

- $\varphi(p) = p - 1$，是个多项式；
- $\varphi(p^e) = p^e (1 - \frac{1}{p}) = p^{e - 1}(p - 1)$，可以被快速计算；
- $\varphi$ 是个积性函数。


莫比乌斯函数 $\mu$：

- $\mu(p) = 1$；
- $\mu(p^e) = 0 \ (e > 1)$；
- $\mu$ 是积性函数。

除数函数 $d$：

- $d(p) = 2$；
- $d(p^e) = e + 1$；
- $d$ 是积性函数。

显然上述三种函数都是可以用 Min25 筛求解的。更多的例子这里就不列举了，大家可以自己试试看~

## 类积性函数？

注意到 Min25 筛求解前缀和的过程本质上就是个 DP。考虑是否可以魔改一下 DP 方程使得其适用于一些其他的 $f$ 函数，比如类似于满足 $\forall a \perp b, \  f(ab) = f(a) + f(b)$ 的函数 $f$。

前段时间乱逛计蒜客的时候发现了这道题：[Jisuanke A2243: A Simple Math Question](https://nanti.jisuanke.com/t/A2243)。不妨就拿它作为例子吧。为了避免混淆下面简述题意时我对原题中的部分符号进行了修改。

---

定义函数 $t$：

$$
t(x) = ax^3 + bx^2 + cx + d
$$

定义函数 $f$：

$$
f(\prod\limits_{i = 1}^{k} {p_i}^{a_i}) = \sum\limits_{i = 1}^{k} a_i t(p_i)
$$

其中 $\prod\limits_{i = 1}^{k} p_i$ 代表数 $n$ 的唯一分解式，且定义 $f(1) = 0$。

给定 $n, a, b, c, d$，试求解：$\sum\limits_{i = 1}^{n} f(i)$。

原题数据范围大概是 $10^8$ 级别的，标程大概是勒让德定理。这里我们不妨把这道题加强一番，假定 $n$ 是 $10^{10}$ 级别，看看 Min25 筛能否解决之。

---

首先观察一下 $f$：

- $f(p) = t(p) = ap^3 + bp^2 + cp + d$，嗯有多项式表示；
- $f(p^e) = e \cdot t(p)$，还是能够快速计算的；
- 虽然它并不是一个积性函数。但还是满足：$\forall \ a \perp b, f(ab) = f(a) + f(b)$

我们先来考虑 $s$ 的转移。首先回顾一下对于积性函数我们的转移是：

$$
\begin{aligned}
s(n, j) 
& = \sum\limits_{i = P_j + 1}^{n} \left[ i \in P \right]f(i) + \sum\limits_{i = P_j + 1}^{n} \left[i \not \in P \right] f(i) \\
& = g(n, |P|) - \sum\limits_{k = 1}^{j - 1}f(P_k) \\ 
& + \sum\limits_{k = j}^{|P|}\sum\limits_{{P_k}^{e + 1} \le n} \left[ f({P_k}^e) \cdot s(\left\lfloor \frac{n}{{P_k}^{e}} \right\rfloor, k + 1) + f({P_k}^{e + 1}) \right]
\end{aligned}
$$

对于第一部分，也就是 $\sum\limits_{i = P_j + 1}^{n} \left[ i \in P \right]f(i)$，不必对其进行修改。由于我们在筛 $s$ 的时候只会用到 $g(n, |P|)$，其中只包含素数处的值，因此我们大可将其当作积性函数来筛出 $g$，筛 $g$ 的过程保持和上文一样就行了。

对于第二部分，发现本质上要解决的是由 $\sum\limits_{p^e \perp k} f(k)$ 快速得出 $\sum\limits_{p^e \perp k} f(p^e \cdot k)$ 的问题。对于积性函数，有：

$$
\sum\limits_{p^e \perp k} f(p^e \cdot k) = f(p^e) \sum\limits_{p^e \perp k}f(k)
$$

而对于本题中的函数，如果我们记这个和式的项数为 $m$，则有：

$$
\begin{aligned}
\sum\limits_{p^e \perp k} f(p^e \cdot k) 
& = \sum\limits_{p^e \perp k} \left[ f(p^e) + f(k) \right] \\
& = m \cdot f(p^e) + \sum\limits_{p^e \perp k}f(k) 
\end{aligned}
$$

也就是说，第二部分可以改成如下形式：

$$
\sum\limits_{k = j}^{|P|}\sum\limits_{{P_k}^{e + 1} \le n} \left[ m \cdot f({P_k}^e) +  s(\left\lfloor \frac{n}{{P_k}^{e}} \right\rfloor, k + 1) + f({P_k}^{e + 1}) \right]
$$

当然，这会让这一部分的复杂度多一个快速幂带来的 $\mathcal{O}(\log{n})$。

接下来就是怎么确定 $m$ 了，不难发现这就是求对于函数 $y(n) = 1$ 的 $s$ 值。因此我们只需要在处理函数 $f$ 时，同时处理函数 $y$，并且在对函数 $f$ 的 $s$ 进行转移时借助函数 $y$ 转移过来的 $s$ 即可。实现的时候开一个结构体一起搜就好了。

具体的实现可以参考一下 [我的代码](https://github.com/codgician/Competitive-Programming/blob/master/Jisuanke/A2243/min25_sieve.cpp)。

## 带条件的前缀和？

这也是我自己 YY 的一种应用，不排除有更优秀的做法。对此我也 YY 了一道题：[春燕的板子题](https://www.luogu.com.cn/problem/U87962)（也没有严格验过题，而且由于洛谷私人题库有总时限限制所以可能有点卡常，这只是一个 Proof of concept）。

本质上我就是把洛谷上的模板题拉过来对所求前缀和加了一个限制 $i$ 余数的条件。更正式地表示即给定 $r, d (r < d \le 6)$，求：

$$
\sum\limits_{i = 0}^{n} \left[ i \equiv r \pmod d \right] \cdot f(i)
$$

只需要 DP 时再多一维表示模数为 $r$ 即可。而对于 $s$，可以对 $r$ 种状态开一个结构体同时转移，这样子复杂度多了一个 $r$ 并解决了问题。当时只是自己写着玩搞了搞，代码就不放了，大家也可以试着自己 YY 一下。

## 前缀积？

Min25 筛的思想是否可以应用在非前缀和的地方，比如…… 前缀积？对此我自己 YY 了一道题：[春燕的数列](https://www.luogu.com.cn/problem/U87823)（没有严格验过题，只是一个 Proof of concept）。

由乘法原理，题目本质即求解：

$$
\prod\limits_{i = 1}^{n} d(i) \pmod {10^9 + 7}
$$

---

首先回顾一下 $d$ 的性质：

- $d(p) = 2$；
- $d(p^e) = e + 1$；
- $d$ 是积性函数。

考虑魔改 $s$ 的转移。我们首先修改一下 $s$ 的定义：

$$
s(n, j) = \prod\limits_{i = 2}^{n} \left[ minp(i) \ge P_j \right] d(i)
$$

依然把它拆成素数和非素数部分考虑：

$$
s(n, j) = \prod\limits_{i = P_j + 1}^{n} \left[ i \in P \right]d(i) \cdot \prod\limits_{i = P_j + 1}^{n} \left[i \not \in P \right] d(i) 
$$

对于第一部分，由于 $d(p) = 2$，因此第一部分就是 $2^{m}$，其中 $m$ 为对应区间中质数个数。因此我们依然没有必要魔改 $g$，只需要通过 $g$ 筛出质数个数即可。

对于第二部分，需要解决的变成了由 $\prod\limits_{p^e \perp k} f(k)$ 快速得出 $\prod\limits_{p^e \perp k} f(p^e \cdot k)$ 的问题。有：

$$
\begin{aligned}
\prod\limits_{p^e \perp k} f(p^e \cdot k) 
& = \prod\limits_{p^e \perp k} \left[ f(p^e) \cdot f(k) \right] \\
& = f(p^e)^m \prod\limits_{p^e \perp k} f(k)
\end{aligned}
$$

对于 $m$ 则与前例相同，同时筛一下 $y(n) = 1$ 的 $s$ 值即可。

# 小结

Min25 筛是一个非常精妙并且灵活多变的 DP。自己在学习的时候就感觉它很精妙，因此就思考了它能否应用在更多的场景，也就 YY 出了后几种粗浅的应用。大概就当是抛砖引玉一下吧，感觉可能还会有更加有趣的变形，欢迎大家一起讨论鸭~