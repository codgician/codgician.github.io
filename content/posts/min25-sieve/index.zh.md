---
title: "浅谈 Min25 筛"
date: 2019-09-01
language: zh
canonical: true
tags:
  - competitive programming
  - algorithm
  - arithmetic function
  - math
  - number theory
math: true
toc: true
draft: false
---

# 前言

Min25 筛是一种对于满足特定条件积性函数的前缀和的亚线性筛法。虽说 Min25 筛对积性函数有一定要求，但其实常见的很多积性函数都是能被筛出来的。另外，Min25 筛也能引出不少灵活变化，故个人感觉也是一种非常有趣的筛法。

# Min25 筛

## 核心思想

下文默认记 $P$ 为素数集，$P_i$ 代表其中第 $i$ 大的素数并默认 $p \in P$。若函数 $f(x)$ 满足：

- $f(p)$ 存在多项式表示；
- $f(p^e)$ 可被快速计算；
- 若 $a \perp b$，则 $f(ab) = f(a)f(b)$。

Min25 筛可在 $\mathcal{O}(\frac{n^\frac{3}{4}}{\log{n}})$ 的复杂度内计算 $\sum\limits_{i = 1}^{n} f(i)$，其核心思想即将 $[1, n]$ 分为质数、$1$ 和剩余数三个部分进行讨论：

$$
\begin{aligned}
\sum\limits_{i = 1}^{n} f(i) & = \sum\limits_{i = 1}^{n} \left[i \in P \right] f(i) + \sum\limits_{i = 1}^{n} \left[i \not \in P \right] f(i) \\
& = \sum\limits_{i = 1}^{n} \left[i \in P \right] f(i) + f(1) + \sum\limits_{p \in P}\sum\limits_{1 \le p^e \le n}f(p^e)\sum\limits_{i = 1}^{\left\lfloor \frac{n}{p^e} \right\rfloor}f(i)
\end{aligned}
$$

## 筛素数答案

既然前面提到了在素数处 $f$ 应有多项式表示，那么我们可以对每个幂次分开考虑。即我们可以把问题简化为计算 $f(p) = p^k$ 的情形，只要对所有的 $k$ 分别计算出结果再乘上对应系数即可。

首先我们考虑求解：

$$
\sum\limits_{i = 1}^{n} [i \in P] f(i)
$$

考虑先线性筛出不大于 $\sqrt{n}$ 的所有素数作为 $P$。记 $minp(x)$ 代表 $x$ 的最小素因子，令：

$$
g(n, j) = \sum\limits_{i = 2}^{n} \left[i \in P \text{ or } minp(i) > P_j \right] f(i)
$$

则有：

$$
\sum\limits_{i = 1}^{n} \left[i \in P \right] f(i) = g(n, |P|)
$$

考虑如何对 $g$ 进行状态转移。$g(n, j)$ 在 $g(n, j - 1)$ 的基础上少了所有非质数并且最小质因子恰为 $P_j$ 的数作为因变量的 $f$ 值。显然，这些数可以由一个最小质因子大于 $P_j$ 的数 $t$ 乘上 $P_j$ 得到。因为 $f$ 是积性函数且 $t \perp P_j$，因此 $f(tP_j) = f(P_j)f(t)$。考虑把 $f(P_j)$ 提出来，则需要减去的部分即为：

$$
f(P_j) \left[ g(\left\lfloor \frac{n}{P_j} \right\rfloor, j - 1) - \sum\limits_{i = 1}^{j - 1} f(P_i) \right]
$$

式子中的 $\sum\limits_{i = 1}^{j - 1} f(P_i)$ 本质上就是 $\sum\limits_{i = 1}^{j - 1} P_i^k$，因此完全可以在筛素数的时候一并处理出来。

完整的状态转移即：

$$
g(n, j) =
\begin{cases}
g(n, j - 1) - f(P_j)\left[ g(\left\lfloor \frac{n}{P_j} \right\rfloor, j - 1) - \sum\limits_{i = 1}^{j - 1}f(P_i) \right] & {P_j}^2 \le n \\
g(n, j - 1) & \text{otherwise}
\end{cases}
$$

对于初值，有 $g(n, 0) = \sum\limits_{i = 2}^{n}f(i)$。如果次数较低，比如平方和立方和都是有公式可以快速得出的，而如果次数较高的话就是多项式插值经典问题了。

接下来讨论一些实现上的细节。首先，第二维可以滚动以节约空间。另外由于 $n$ 比较大，故显然需要对第一维离散化。

又由向下取整的一个性质：

$$
\left\lfloor \frac{\left\lfloor\frac{n}{a}\right\rfloor}{b} \right\rfloor = \left\lfloor \frac{n}{ab} \right\rfloor
$$

故转移时所需要的第一维一定形如 $\left\lfloor \frac{n}{x} \right\rfloor$ 形式，我们可以对这 $\mathcal{O}(\sqrt{n})$ 个数离散化一下。如果用 `std::map` 的话会多白给一个 $\mathcal{O}(\log{n})$，一种更高效的做法是用两个数组，其中 $indx1[x]$ 表示 $x$ 离散化后的下标，而 $indx2[x]$ 表示 $\frac{n}{x}$ 对应的下标。这样的好处在于两个数组的下标都不会超过 $\sqrt{n}$。在访问 $x$ 离散化后的下标时，若 $x < \sqrt{n}$ 则可直接访问 $indx1[x]$，而若 $x \ge \sqrt{n}$ 则可访问 $indx2[\frac{n}{x}]$，实属精妙。

## 筛非质数答案

接下来我们考虑求解：

$$
\sum\limits_{i = 1}^{n} \left[i \not \in P \right] f(i)
$$

由于 $f$ 在素数处有多项式表示，因此在筛 $g$ 的时候我们可以按每一个幂次分开计算。但是如果把合数也拉进来，就不可以拆开了。因此这一部分中 $f$ 指的就是原方程。

令：

$$
s(n, j) = \sum\limits_{i = 2}^{n} \left[ minp(i) \ge P_j \right] f(i)
$$

很显然，我们最终想要的答案即：

$$
\sum\limits_{i = 1}^{n}f(i) = s(n, 1) + f(1)
$$

现在我们来考虑如何求解 $s(n, 1)$。对 $s$ 进行状态转移的时候，我们可以沿用分类的思想，即把它分成素数处之和和剩余数处之和两部分：

$$
s(n, j) = \sum\limits_{i = P_j + 1}^{n} \left[ i \in P \right]f(i) + \sum\limits_{i = P_j + 1}^{n} \left[i \not \in P \right] f(i)
$$

第一部分我们之前已经解决了，就是 $g(n, |P|) - \sum\limits_{k = 1}^{j - 1} f(P_k)$。

对于第二部分，既然是满足 $minp \ge P_j$ 的合数，那么自然可以表示为 ${P_j}^e \cdot t$ 的形式（且满足 ${P_j}^e \perp t$）。与此同时，显然有 $minp(t) \ge P_{j + 1}$。我们可以先枚举最小素因子 $P_k$，然后对其枚举幂次 $e$，然后利用积性函数的性质作如下计算：

$$
\sum\limits_{k = j}^{|P|}\sum\limits_{{P_k}^{e + 1} \le n} \left[ f({P_k}^e) \cdot s(\left\lfloor \frac{n}{{P_k}^{e}} \right\rfloor, k + 1) + f({P_k}^{e + 1}) \right]
$$

最后加上 $f({P_k}^{e + 1})$ 的原因是前面利用积性函数性质的部分无法计算形如 $f(p^k)$ 处的值。

完整的状态转移如下：

$$
\begin{aligned}
s(n, j) & =  g(n, |P|) - \sum\limits_{k = 1}^{j - 1}f(P_k) \\
& + \sum\limits_{k = j}^{|P|}\sum\limits_{{P_k}^{e + 1} \le n} \left[ f({P_k}^e) \cdot s(\left\lfloor \frac{n}{{P_k}^{e}} \right\rfloor, k + 1) + f({P_k}^{e + 1}) \right]
\end{aligned}
$$

直接递归搜即可，不需要记忆化复杂度也是对的（~~证明不来~~），如果有多个状态可以考虑开个结构体一起转移。

# 应用

## 模板

首先可以试试洛谷上的模板题：[Luogu P5325: Min25筛](https://www.luogu.com.cn/problem/P5325)。同时附上 [我的代码](https://github.com/codgician/Competitive-Programming/blob/master/Luogu/P5325/min25_sieve.cpp) 以供参考。

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
