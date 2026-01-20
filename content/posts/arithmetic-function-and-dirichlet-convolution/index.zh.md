---
title: "数论函数与狄利克雷卷积"
date: 2019-08-06
language: zh
canonical: true
translationKey: arithmetic-function-and-dirichlet-convolution
tags:
  - icpc-notes
  - abstract-algebra
  - algorithm
  - arithmetic-function
  - competitive-programming
  - dirichlet-convolution
  - mathematics
  - number-theory
math: true
toc: false
draft: false
---


# 简介

本文先从狄利克雷卷积讲起，然后会简要介绍一种基于这一理论的有趣数论函数前缀和筛法（江湖人称 “杜教筛”）。

# 狄利克雷卷积

前面的博文中我们从初等数论的角度初步领略了莫比乌斯反演，是不是感觉推导有一些小复杂…… 但是，如果我们从抽象代数的角度来对其进行研究，就会发现反演这一公式自然而又优雅。

## 定义

我们研究的对象是诸如欧拉函数 $\varphi$、莫比乌斯函数 $\mu$ 等等的*数论函数 (arithmetic function)*。在此我们不妨把数论函数定义为 $\mathbb{Z}_{+} \longmapsto \mathbb{C}$ 的函数（正整数集合映射至复数集）。

**定义**： 对于数论函数 $f, g$，定义其 *狄利克雷卷积 (Dirichlet convolution)*：

$$
(f \cdot g)(n) = \sum\limits_{d \mid n} f(d)g(\frac{n}{d})
$$

注：后文常把 $(f \cdot g)(n)$ 简写为 $f \cdot g$。

## 构造一个环

**定理**：记 $\mathbb{F}$ 为全体数论函数构成的集合，则 $\langle \mathbb{F}, +, \cdot \rangle$ 构成可交换环。其中 $+$ 就是正常的加法，$\cdot$ 则为刚刚提到的卷积（后文中可能会将其称作“乘法”）。

*证明*：

首先我们可以列一个证明清单：

1. 运算的封闭性；
2. $+$：交换律、结合律；存在零元（零函数 $0(n) = 0$，简记为 $0$）、负元；
3. $\boldsymbol{\cdot}$：交换律，结合律；存在单位元（元函数 $\epsilon(n) = n$，简记为 $\epsilon$）；
4. $+$ 和 $\cdot$ 间满足分配律。

第 $1, 2$ 点中的内容较为显然，这里就略去证明了。下面对 $3, 4$ 进行简要证明：

- 乘法交换律：
  
  $$
  \begin{aligned}
  f \cdot g & = \sum\limits_{d | n}f(d)g(\frac{n}{d}) \\
  & \text{let } k = \frac{n}{d}, \ \therefore d = \frac{n}{k} \\
  & = \sum\limits_{k \mid n}f(\frac{n}{k})g(k) \\
  & = g \cdot f
  \end{aligned}
  $$

- 乘法结合律：

  $$
  \begin{aligned}
  (f \cdot g) \cdot h & = \sum\limits_{t \mid n}\left[\sum\limits_{d \mid t} f(d)g(\frac{t}{d})\right]h(\frac{n}{t}) \\
  & = \sum\limits_{t \mid n}\sum\limits_{d \mid t} f(d)g(\frac{t}{d})h(\frac{n}{t})
  \end{aligned}
  $$

  $$
  \begin{aligned}
  f \cdot (g \cdot h) & = \sum\limits_{d \mid n} f(d) \left[ \sum\limits_{t \mid \frac{n}{d}} g(t)h(\frac{n}{dt}) \right] \\
  & \text{let } T = dt \\
  & = \sum\limits_{d \mid n} f(d) \left[ \sum\limits_{T \mid n} g(\frac{T}{d})h(\frac{n}{T}) \right] \\
  & = \sum\limits_{T \mid n}\sum\limits_{d \mid T} f(d)g(\frac{T}{d})h(\frac{n}{T})
  \end{aligned}
  $$

  $$
  \therefore (f \cdot g) \cdot h = f \cdot (g \cdot h)
  $$

- 加法和乘法间的分配律（由于已经证明交换律，因此只证左分配律）：
  
  $$
  \begin{aligned}
  f \cdot (g + h) & = \sum\limits_{d \mid n} f(d) \left[ g(\frac{n}{d}) + h(\frac{n}{d}) \right] \\
  & = \sum\limits_{d \mid n} f(d)g(\frac{n}{d}) + f(d)h(\frac{n}{d}) \\
  & = f \cdot g + f \cdot h
  \end{aligned}
  $$

Voilà! 于是我们便得到了一个可交换环。我们来看看用它能够发现哪些有趣的事情~

## 有趣的性质

**定义**：若数论函数 $f$ 满足 $f(1) = 1$，且 $\forall a,b \ \text{ s.t. } \ a \perp b$（若 $a, b$ 互质）满足 $f(ab) = f(a)f(b)$，则称 $f$ 为 *积性函数 (multiplicative function)*。

**定理**：若 $f, g$ 为积性函数，则 $f \cdot g$ 也为积性函数。

*证明*：

$\forall a, b \in \mathbb{Z}_{+}, \text{ s.t. } a \perp b$，有：

$$
\begin{cases}
f(ab) & = f(a)f(b) \\
g(ab) & = g(a)g(b)
\end{cases}
$$

$$
\begin{aligned}
(f \cdot g)(a) (f \cdot g)(b) & = \sum\limits_{t_1 \mid a}f(t_1)g(\frac{a}{t_1})\sum\limits_{t_2 \mid b}f(t_2)g(\frac{b}{t_2}) \\
& = \sum\limits_{t_1 \mid a}\sum\limits_{t_2 \mid b}f(t_1)f(t_2)g(\frac{a}{t_1})g(\frac{b}{t_2}) \\
& \because a \perp b, \ \therefore t_1 \perp t_2, \ \frac{a}{t_1} \perp \frac{b}{t_2} \\
& = \sum\limits_{t_1 \mid a}\sum\limits_{t_2 \mid b}f(t_1t_2)g(\frac{ab}{t_1t_2}) \\
& \text {let } d = t_1t_2\\
& = \sum\limits_{d | ab}f(d)g(\frac{ab}{d}) \\
& = (f \cdot g)(ab)
\end{aligned}
$$

对最后一步进行一个补充说明，本质上是 $\sum\limits_{t_1 \mid a}\sum\limits_{t_2 \mid b} 1 = \sum\limits_{t | ab}1$。不妨令 $t_1$ 的质因子集合为 $S_1$，$t_2$ 的质因子集合为 $S_2$，则 $\because t_1 \perp t_2, \ \therefore S_1 \cap S_2 = \Phi$。因此，$\forall t \mid ab$，一定存在唯一的 $t_1, t_2$ 满足 $t = t_1t_2$ 且 $t_1 \perp t_2$（可用反证法容易得证）。

## 再谈莫比乌斯反演？

我们再回过头来看看 $\mu$ 的一个重要性质：

$$
\sum\limits_{d \mid n} \mu(d) =
\begin{cases}
1 & n = 1\\
0 & \text{otherwise} \\
\end{cases}
$$

如果我们定义一个数论函数 $1(n) = 1$，并且简记为 $1$，那么我们可以将这一性质借助狄利克雷卷积表示为（其中 $\epsilon$ 为我们之前定义的单位元）：

$$
\mu \cdot 1 = \epsilon
$$

那么，如果有 $f(n) = \sum\limits_{d \mid n}g(d)$，即：

$$
\begin{aligned}
f = g \cdot 1 & \Rightarrow f \cdot \mu = g \cdot 1 \cdot \mu \\
& \Rightarrow f \cdot \mu = g \cdot (1 \cdot \mu) \\
& \Rightarrow f \cdot \mu = g \cdot (\mu \cdot 1) \\
& \Rightarrow f \cdot \mu = g \cdot \epsilon \\
& \Rightarrow f \cdot \mu = g 
\end{aligned}
$$

于是我们得到了莫比乌斯反演：

$$
g(n) = \sum\limits_{d \mid n}g(d)\mu(\frac{n}{d})
$$

在引入了数论函数和卷积构成的代数结构后，这一证明看起来无比简单明了、清新自然~

---

既然提到了莫比乌斯函数 $\mu$，我们也不能忘了跟他一样经常出现的好基友欧拉函数 $\varphi$。我们知道欧拉函数有一个性质：

$$
\sum\limits_{d \mid n} \varphi(d) = n
$$

我们定义一个数论函数 $N(n) = n$，并试着将上式表示成狄利克雷卷积？

$$
\varphi \cdot 1 = N
$$

我们试着对其操作一下：

$$
\begin{aligned}
\varphi \cdot 1 = N & \Rightarrow \varphi \cdot 1 \cdot \mu = N \cdot \mu \\
& \Rightarrow \varphi = N \cdot \mu
\end{aligned}
$$

于是我们再次发现了两者之间非同一般的关系：

$$
\sum\limits_{d \mid n} \frac{\mu(d)}{d} = \frac{\varphi(n)}{n}
$$

---

另外，除数函数也可以用狄利克雷卷积表示：

$$
\begin{aligned}
d(n) = \sum\limits_{d \mid n}1 & \Rightarrow d = 1 \cdot 1 \\ 
\sigma(n) = \sum\limits_{d \mid n}d & \Rightarrow \sigma = N \cdot 1
\end{aligned}
$$

# 杜教筛

杜教筛之所以有趣，并不是因为其具有怎样的普适性，而是其本身就在于 “构造”。只要能构造出来，我们就可以对特定的积性函数 $f$ 求出其前缀和 $S(n) = \sum\limits_{i = 1}^{n} f(i)$。

## 构造？

构造两个积性函数 $g, h \ \text{ s.t. } h = f \cdot g$：

$$
\begin{aligned}
\sum\limits_{i = 1}^{n} h(i) & = \sum\limits_{i = 1}^{n} \sum\limits_{d \mid i} f(d)g(\frac{i}{d}) \\
& = \sum\limits_{d = 1}^{n} g(d) \sum\limits_{d \mid i} f(\frac{i}{d}) \\
& = \sum\limits_{d = 1}^{n} g(d) \sum\limits_{i = 1}^{\left\lfloor \frac{n}{d} \right\rfloor} f(i) \\
& = \sum\limits_{d = 1}^{n}g(d) S(\left\lfloor \frac{n}{d} \right\rfloor)
\end{aligned}
$$

整理一下可得：

$$
g(1)S(n) = \sum\limits_{i = 1}^{n}h(i) - \sum\limits_{d = 2}^{n} g(d)S(\left\lfloor\frac{n}{d}\right\rfloor)
$$

这就是杜教筛的本体。换句话说，只要我们构造出的 $\sum\limits_{i = 1}^{n}h(i)$ 是可以 $\mathcal{O}(1)$ 求得的，那么结合适当的预处理和记忆化（预处理规模 $\mathcal{O}(n ^ \frac{2}{3})$），我们可以用 $\mathcal{O}(n ^ \frac{2}{3})$ 的复杂度计算出 $S(n)$。

更具体地说，即我们应当先用线性筛预处理出前 $\mathcal{O}(n ^ \frac{2}{3})$ 左右的前缀和，然后对于大于这一阈值的前缀和递归求解。同时，每当求出一个值我们将其记忆化在哈希表里以加速未来的运算。

对于这一筛法复杂度的详细证明大家可以参考这篇博文：[杜教筛时间复杂度证明 - _Ark](https://blog.csdn.net/Ike940067893/article/details/84781307)

## 应用

下面我们来看几点应用，感受一下杜教筛的魅力所在~

- $f(i) = \varphi(i), \ S(n) = \sum\limits_{i = 1}^{n} \varphi(i)$：
  
  由 $\varphi \cdot 1 = N$ （对应 $f \cdot g = h$）：
  $$
  S(n) = \sum\limits_{i = 1}^{n}i - \sum\limits_{d = 2}^{n} S(\left\lfloor \frac{n}{d} \right\rfloor)
  $$

- $f(i) = \mu(i), \ S(n) = \sum\limits_{i = 1}^{n} \mu(i)$：

  由 $\mu \cdot 1 = \epsilon$（对应 $f \cdot g = h$）：

  $$
  S(n) = 1 - \sum\limits_{d = 2}^{n} S(\left\lfloor \frac{n}{d} \right\rfloor)
  $$

- $f(i) = i \varphi(i), \ S(n) = \sum\limits_{i = 1}^{n} i  \varphi(i)$：

   $$
   \begin{aligned}
   h= f \cdot g & = \sum\limits_{d \mid n}f(d)g(\frac{n }{d}) \\
   & = \sum\limits_{d \mid n} d \varphi(d)g(\frac{n}{d}) \\
   & \text{let } g = N \\ 
   & = n\sum\limits_{d \mid n}\varphi(d) \\
   & = n^2
   \end{aligned}
   $$

   $$
   S(n) = \sum\limits_{i = 1}^{n}i^2 - \sum\limits_{d = 2}^{n} dS(\left\lfloor \frac{n}{d} \right\rfloor)
   $$
   
- $f(i) = i^2 \varphi(i), \ S(n) = \sum\limits_{i = 1}^{n} i^2 \varphi(i)$：

   $$
   \begin{aligned}
   h = f \cdot g & = \sum\limits_{d \mid n} f(d)g(\frac{n}{d}) \\
   & = \sum\limits_{d \mid n}d^2\varphi(d)g(\frac{n}{d}) \\
   & \text{let } g = N^2 \\
   & = n^2\sum\limits_{d \mid n} \varphi(d) \\
   & = n^3
   \end{aligned}
   $$

   $$
   S(n) = \sum\limits_{i = 1}^{n}i^3 - \sum\limits_{d = 2}^{n}d^2S(\left\lfloor \frac{n}{d} \right\rfloor)
   $$

至于具体的代码，大家可以去试试 [洛谷 P4213: 杜教筛 (Sum)](https://www.luogu.com.cn/problem/P4213)，顺便提供 [我的代码](https://github.com/codgician/Competitive-Programming/blob/master/Luogu/P4213/arithmetic_function.cpp) 供参考。

完结撒花~

# %%%

- [Dirichlet convolution - Wikipedia](https://en.wikipedia.org/wiki/Dirichlet_convolution)
- [狄利克雷卷积 - Hanano](https://hanano-yuuki.github.io/2018/08/14/Dirichlet-convolution)
- [杜教筛 - peng_ym](https://www.cnblogs.com/peng-ym/p/9446555.html)
- [杜教筛时间复杂度证明 - _Ark](https://blog.csdn.net/Ike940067893/article/details/84781307)

