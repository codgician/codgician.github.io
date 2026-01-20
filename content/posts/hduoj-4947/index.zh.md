---
title: "HDUOJ 4947: GCD Array"
date: 2018-11-24
language: zh
canonical: true
translationKey: hduoj-4947
tags:
  - icpc-solutions
  - algorithm
  - arithmetic-function
  - binary-indexed-tree
  - competitive-programming
  - data-structure
  - hduoj
  - mathematics
  - number-theory
math: true
toc: false
draft: false
---


# 题面

对于长度为 $l$ 且初始全为 $0$ 的序列 $a$（下标 $1 \sim l$），有 $q$ 次操作。操作分为两种：

1. 给定 $n, d, v$，对于每个满足 $\gcd(x, n) = d$ 的 $x$，为 $a_x$ 加上 $v$；
2. 给定 $x$，询问 $\sum\limits_{i = 1}^{x} a_i$。

**数据范围**：$1 \le l, q \le 5 \times 10^4, \ 1 \le n, d, v \le 2 \times 10^5, \ 1 \le x \le l$

**题目链接**：[HDUOJ 4947: GCD Array](http://acm.hdu.edu.cn/showproblem.php?pid=4947)

# 分析

对于操作 $1$，如果 $d \nmid n$，则显然不可能满足 $\gcd(x, n) = d$。对于这样的情况我们忽略即可。

我们记 $\Delta{a}(x)$ 代表某次进行第一种操作时位置 $x$ 上值的增量，有：

$$
\begin{aligned}
\Delta{a}(x) = & v \cdot [\gcd(x, n) = d] \\
= & v \cdot [\gcd(\frac{x}{d}, \frac{n}{d}) = 1]\\
\end{aligned}
$$

借助莫比乌斯函数性质，有：

$$
\begin{aligned}
\Delta{a}(x) = & \sum\limits_{k \mid \gcd(\frac{x}{d}, \frac{n}{d})} \mu(k) \cdot v \\
= & \sum\limits_{k \mid \frac{x}{d}, k \mid \frac{n}{d}} \mu(k) \cdot v \\
= & \sum\limits_{k \mid \frac{n}{d}} \sum\limits_{kd \mid x} \mu(k) \cdot v \\
\end{aligned}
$$

我们注意到，$\sum\limits_{kd \mid x} \mu(k) \cdot v$ 看起来十分类似莫比乌斯反演中的 $F(n)$ 函数。既然如此，我们不妨考虑引入一个辅助数组 $f$，使其满足：

$$
a(x) = \sum\limits_{k \mid x}f(k)
$$

由此一来，我们可以通过维护 $f(x)$ 并通过莫比乌斯反演从而求得 $a(x)$。

根据上面推导而来的式子，我们不难发现，操作一实则是对于 $\frac{n}{d}$ 的每一个因子 $k$，向 $f(kd)$ 中增加 $\mu(k) \cdot v$。

---

而对于操作 $2$：

$$
\begin{aligned}
\sum\limits_{i = 1}^{x} a(i) = & \sum\limits_{i = 1}^{x}\sum\limits_{k \mid i}f(k) \\
= & \sum\limits_{k = 1}^{x} \left\lfloor \frac{x}{k} \right\rfloor f(k)
\end{aligned}
$$

整除分块搞一下就好了…… 由于分块需要快速求出区间和，所以可以考虑使用树状数组维护 $f(x)$。

---

接下来我们简要分析一下复杂度：

对于操作 $1$，$\frac{n}{d}$ 分解质因数的复杂度 $\mathcal{O}(\sqrt{N})$，加上树状数组单点更新后复杂度 $\mathcal{O}(\sqrt{N}\log{N})$；

对于操作 $2$，整除分块复杂度 $\mathcal{O}(\sqrt{N})$，加上树状数组区间查询后复杂度 $\mathcal{O}(\sqrt{N}\log{N})$。

妙啊！

最后附上 [我的代码](https://github.com/codgician/Competitive-Programming/blob/master/HDUOJ/4947/mobius_inversion_binary_indexed_tree.cpp) 以供参考。

# %%%

- flipped - [【HDU4947】GCD Array (莫比乌斯反演+树状数组)](https://www.cnblogs.com/flipped/p/HDU4947.html)
