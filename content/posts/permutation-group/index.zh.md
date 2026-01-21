---
title: "浅谈置换群计数"
date: 2020-03-26
language: zh
canonical: true
translationKey: permutation-group
tags:
  - icpc-notes
math: true
toc: false
draft: false
---


# 前言

我们先引入一些例子来介绍一下 Burnside 引理常见的应用。

考虑一个等边三角形玩具，要对其顶点用红蓝两种颜色进行染色。由乘法原理，如果不考虑其他条件染色方案数量为 $2^3 = 8$ 种。但是，`红 - 蓝 - 红` 和 `红 - 红 - 蓝` 本质上对应的是同一种方案，后者可以由前者通过旋转得到。故本质上不同的染色方案一定小于 $8$ 种（事实上只有 $4$ 种）。应对这一类问题，仅靠传统的组合数学是非常难以应对的，而如果引入群论、置换群、置换群在集合上的作用等概念，再结合 Burnside 引理的话，就可以较好地解决这一类”求本质不同染色方案数“的问题。

另外，Burnside 引理也常被应用于化学上同分异构体种类的计数，大家感兴趣的话也可以了解一下~

本文大致分为三个部分：第一部分会首先对证明 Burnside 引理所需要的基本抽象代数知识进行介绍；第二部分会引入文章的主题 —— Burnside 引理和基础的 Pólya 计数法；最后会浅谈一下 Burnside 引理在算法竞赛中的几类常见题型。同时，本文某种程度上也可以作为之前在集训队内做过的讲座 [浅谈置换群](https://codgician.me/pgslides) 的讲义。

另外，我的水平也有限，所以本文中的部分用语可能不太严谨…… 欢迎大家提出指正 QwQ。 

# 群论基础

## 集合论基础

### 关系

**关系 (relation)** 指集合内部分元素之间的某种关联。比如整数构成的集合内元素间可以存在倍数关系，三角形构成的集合内元素间可以存在相似关系。

首先回顾集合 $A$ 和 $B$ 的 **笛卡尔积 (Cartesian product)** 定义：

$$
A \times B = \left\{ (a, b) \mid a \in A, b \in B \right\}
$$

可见 $A \times B$ 后得到的是一个由二元组的集合，并且这些二元组的左部来自于集合 $A$，右部来自于集合 $B$。

接下来尝试相对严格地定义关系：对于集合 $A$，集合 $A \times A$ 的每个子集 $R$ 都叫做集合 $A$ 上的一个关系。如果 $(a, b) \in R$，则称 $a$ 和 $b$ 有关系 $R$，记作 $aRb$。

### 等价关系

等价关系是一类特殊的关系。若集合 $A$ 上的关系 $\sim$ 满足如下条件：

- **自反性**：$\forall a \in A$，$a \sim a$；
- **对称性**：$\forall a, b \in A$，若 $a \sim b$ 则 $b \sim a$；
- **传递性**：$\forall a, b \in A$，若 $a \sim b, \ b \sim c$，则 $a \sim c$；

则称 $\sim$ 是**等价关系 (equivalence relation)**。

前面提到的整除关系并不一定满足对称性、传递性，因此不属于等价关系；而三角形间的相似则满足全部 $3$ 条性质，因此属于等价关系。

再举一个例子，定义关系 $a \sim b := a \equiv b \pmod 7$，即若 $a$ 和 $b$ 除以 $7$ 所得的余数相等则称 $a$ 和 $b$ 间存在关系 $\sim$，也可以很容易验证自反性、对称性、传递性。

### 等价类

发现等价关系可以对集合内的元素进行分类：

- 依据三角形的相似关系可以对三角形集合进行分类，
- 依据模 $7$ 的余数可以把所有自然数分成 $7$ 类。

设 $\sim$ 是 $A$ 上的等价关系，$\forall a \in A$，$[a]$ 表示 $A$ 中与 $a$ 等价的全部元素构成的集合：

$$
[a] = \{ b \sim a \mid b \in A \}
$$

称 $[a]$ 为 $a$ 所在的**等价类 (equivalence class)**。

---

注意到一个元素似乎只可能属于一个等价类，而不能同时存在于多个等价类内。这也就使得，不同等价类之间的交集必然为空集。

性质：若 $a, b \in A$ 且 $[a] \cap [b] \neq \emptyset$，则 $[a] = [b]$。

运用反证法可以证明这一性质：

- 假设存在 $[a] \neq [b]$ 且 $[a] \cap [b] \neq \emptyset$；
- 令 $k_1 \in [a]$ 且 $k_1 \notin [b]$，$k_2 \in [a] \cap [b]$；
- 则有 $k_1 \sim a, \ k_2 \sim a, \ k_2 \sim b$；
- 由传递性得 $k_1 \sim b$，与假设不符。

这启示我们：

- 集合 $A$ 可看作一些两两不相交的等价类的并：

  $$
  A = \bigcup\limits_{a \in R} [a] \text{（两两不相交之并）}
  $$
  
  其中，式子里的 $R$ 称之为完全代表系，由等价类 $[a_i]$ 中选出一个元素构成，使得 $A$ 中每个元素都与 $R$ 中的某个元素等价，同时 $R$ 中的元素彼此不等价。

- $A$ 上的每个等价关系给出集合 $A$ 的一个**划分 (partition)**。
  
  划分的定义：若 $A$ 是它的某些子集 $\{ A_i | i \in I \}$ 之并，且 $A_i$ 两两不交，则称其为集合 $A$ 的一个划分（或分拆）。 

引入等价类的意义就是为了对集合中的元素进行分类。后面要介绍的轨道、陪集等本质上都是基于等价关系的。

## 群论基础

### 群

设 $G$ 是非空集合，且二元运算 $\cdot$ 满足：

- 结合律：$(a \cdot b) \cdot c = a \cdot (b \cdot c)$
- 单位元 $e$：$\forall a \in G, \ ea = ae = a$ 
- 逆元：$\forall a \in G, \ \exist b \in G \text{ \ s.t. \ } ab = ba = e$

则称 $(G, \cdot)$ 是一个**群 (group)**，有时也简写成 $G$。

需要注意的是，群并不要求运算满足交换律。如果运算满足交换律，称这样的群为**阿贝尔群 (Abelian group)**，或交换群。另外，若群 $G$ 的大小有限，则成其为**有限群 (finite group)**。

---

例如在集合 $\mathbb{Z}_7 = [0, 1, 2, 3, 4, 5, 6]$ 上定义模 $7$ 加法，即 $a + b := (a + b) \bmod 7$。我们来验证一下 $(\mathbb{Z}_7, +)$ 是否成群：

- 结合律：$(a + b) + c = a + (b + c)$；
- 单位元 $e = 0$：$0 + a = a + 0 = a$；
- 逆元：对于 $a$，其逆元 $a^{-1} = (7 - a) \bmod 7$；

所以 $(\mathbb{Z}_7, +)$ 成群。

---

群有两条非常重要的性质：

- 左右逆元相等：
  
  设 $x$ 是 $a$ 的左逆元，$y$ 是 $a$ 的右逆元，有：

  $$
  x = xe = x(ay) = (xa)y = y
  $$

- 满足消去律：
  
  $$
  \forall a, b, c \in G, \ ab = ac \Leftrightarrow b = c
  $$

  可见，只要逆元存在就存在消去律。


### 子群

设 $(G, \cdot)$ 为群，$H$ 是 $G$ 的子集，若 $(H, \cdot)$ 成群，则称 $H$ 为 $G$ 的**子群 (subgroup)**，记作 $H \le G$；

### 陪集

前面提到可以通过等价类来对集合进行划分，而现在我们需要找到一种东西来对群进行划分。基于此引入陪集这一概念。

设 $H \leq G$，对于 $x \in G$：

- $H$ 的一个**左陪集 (left coset)** $xH$：
  $$
  xH = \{ x \cdot h \mid h \in H \}
  $$
- $H$ 的一个**右陪集 (right coset)** $Hx$：
  $$
  Hx = \{ h \cdot x \mid h \in H \}
  $$

由于左陪集和右陪集性质上相似，故后文只讨论左陪集。对于右陪集，请读者自行尝试~

---

对于 $x, y \in G$，定义如下关系 $\sim$：

$$
x \sim y := x \in yH
$$

发现这其实是一个**等价关系**：

- **自反性**：$x \in xH$；
  - 既然 $H$ 是群，则 $e \in H$，故 $x \cdot e = x \in xH$
- **对称性**：若 $y \in xH$，则 $x \in yH$；
  - $y \in xH \Rightarrow \exist h \in H \text{ \ s.t. \ } y = x \cdot h$
  - $H$ 中逆元存在，则 $\exists h \in H, \ \text{ s.t. } \ x = y \cdot h^{-1}$
  - 由 $h^{-1} \in H$，故 $x \in yH$
- **传递性**：若 $z \in yH, \ y \in xH$，则 $z \in xH$。
  - $z \in yH \Rightarrow \exist h_1 \in H \text{ \ s.t. \ } y = z \cdot h_1$
  - $y \in xH \Rightarrow \exist h_2 \in H \text{ \ s.t. \ } x = y \cdot h_2$
  - 令 $h = h_1h_2$，则 $x = z \cdot h$ 且 $h \in H$，故 $z \in xH$

---

故直接将讨论等价类时得出的结论搬到此处：

- 若 $xH \cap yH \neq \emptyset$，则 $xH = yH$；
- 利用陪集可以对群 $G$ 进行划分（陪集分解）：
  
  $$
  G = \bigcup\limits_{g \in R} gH \text{（两两不相交之并）}
  $$

  这里展现了对群 $G$ 的左陪集分解。与之前类似， $R$ 称作 $G$ 对 $H$ 左陪集的代表元系。$R$ 由 $G$ 中的元素构成，并且这些用元素生成的左陪集彼此互不相同，与此同时这些左陪集的并集恰好为 $G$。

### 拉格朗日定理

对于群 $H \leq G$（两者均为有限群），$\forall a, b \in H, g \in G$，由消去律：

$$
a \neq b \Leftrightarrow ga \neq gb
$$

这启示我们，$\forall g \in G$，$gH$ 内的元素其实和 $H$ 内的元素是一一对应的。因为 $H$ 内不同的元素左乘 $g$ 后并不会变得相等。因此两者大小也是相等的： $|H| = |gH|$。

这也意味着群 $G$ 对子群 $H$ 的所有陪集的大小都是相等的，并且都等于 $|H|$。

记 $R$ 为 $H$ 的左陪集代表元系，有：

$$
\begin{aligned}
|G| & = \sum\limits_{g \in R} |gH| \\
& = \sum\limits_{g \in R} |H| \\
& = |R| \cdot |H|
\end{aligned}
$$

若把 $H$ 的左陪集代表元系的大小 $|R|$ 称作群 $H$ 对于群 $G$ 的**指数 (index)** 并记作 $[G : H]$，便得到抽象代数里的**拉格朗日定理 (Lagrange's Theorem)**：

设 $G$ 为有限群，$H \leq G$，则：

$$
|G| = [G : H] \cdot |H|
$$

## 置换、置换群

### 置换

一个集合的**置换 (permutation)** 即从该集合映射至自身的双射。

例如，对于 $[1, 2, \dots n]$ 的置换 $\sigma$ 可记作：

$$
\sigma = 
\left(\begin{array}{cccc}
1 & 2 & \dots & n \\
\sigma(1) & \sigma(2) & \dots & \sigma(n)
\end{array}\right)
$$

其含义为，置换将 $1$ 变成 $\sigma(1)$，$2$ 变成 $\sigma(2)$…… 依此类推。

置换之间存在复合运算： $(f \circ g)(x) = f(g(x))$，后文中时常简写为 $f \circ g$，有时也称其为置换间的乘法。

---

举一个例子：

$$
\left(\begin{array}{cccccc} 
1 & 2 & 3 & 4 & 5 & 6 \\ 
4 & 5 & 1 & 3 & 6 & 2 
\end{array}\right)
$$

试着写出其对应的“映射关系链”：

$$
\begin{aligned}
1 & \rightarrow 4 \rightarrow 3 \\
2 & \rightarrow 5 \rightarrow 6
\end{aligned}
$$

任何一个置换都能被划分成若干不交的映射链吗？如果可以的话，这就意味着我们发现了一种能够更简单表示置换的方式 —— 以“映射链”相乘的形式表示置换（也就是马上会讲到的轮换表示法）。

### 轮换表示法

$$ 
\left(\begin{array}{cccc}
a_1 & a_2 & \dots & a_n \\
a_2 & a_3 & \dots & a_1
\end{array}\right) \xRightarrow{\text{记作}} (a_1 \enspace a_2 \enspace \dots \enspace a_n)
$$

借助轮换表示法来表示刚才的例子：

$$
\left(\begin{array}{cccccc} 
1 & 2 & 3 & 4 & 5 & 6 \\ 
4 & 5 & 1 & 3 & 6 & 2 
\end{array}\right)
= (1 \enspace 4 \enspace 3) \cdot (2 \enspace 5 \enspace 6)
$$

这令人联想到对于整数的质因数分解…… 那么若不计轮换内的次序（即 $(a, b, c)$ 和 $(b, c, a)$ 当作相同置换）以及轮换间的次序（即 $(a, b, c) \cdot (d, e, f)$ 与 $(d, e, f) \cdot (a, b, c)$ 当作相同分解方案），对于任意置换的不交轮换分解是唯一的吗？

Hmm... 显然是唯一的。下面给出一个构造性的说明：

- 对于恒等置换，显然分解是唯一的；
- 对于非恒等置换，$\exist i \text{ \ s.t. \ } \sigma(i) \neq i$。
  - $i \rightarrow \sigma(i) \rightarrow \sigma^2(i) \rightarrow \dots$
  - 由抽屉原理，$\exist t_1 < t_2 \text{ \ s.t. \ } \sigma^{t_1}(i) = \sigma^{t_2}(i)$
  - 令 $t$ 为使得 $\sigma^t(i) = i$ 的最小正整数，则：
    $$
    (i \enspace \sigma(i) \enspace \dots \enspace \sigma^{t - 1}(i))
    $$
    是一个轮换。
- 对于每个这样的 $i$ 都如此操作即可构造出一个唯一的不相交轮换分解式：
  - 每个元素在分解式中恰好出现 $1$ 次；
  - 每个元素所属于的轮换是固定的。

### 置换的幂运算

下面讨论如何快速得到置换 $\sigma$ 的 $t$ 次幂 $\sigma^t$，即与先后作用 $t$ 次 $\sigma$ 置换等价的置换。举几个例子：

$$
\begin{aligned}
(1 \enspace 2 \enspace 3 \enspace 4 \enspace 5 \enspace 6)^2 & = (1 \enspace 3 \enspace 5) \cdot (2 \enspace 4 \enspace 6) \\
(1 \enspace 2 \enspace 3 \enspace 4 \enspace 5 \enspace 6)^3 & =  (1 \enspace 4) \cdot (2 \enspace 5) \cdot (3 \enspace 6) \\
(1 \enspace 2 \enspace 3 \enspace 4 \enspace 5 \enspace 6)^4 & = (1 \enspace 5 \enspace 3) \cdot (2 \enspace 6 \enspace 4)
\end{aligned}
$$

直接考虑置换的幂并不方便，但由于置换可被分解成若干不相交轮换，不妨先看简单一些的情形：求一个轮换的幂次。

$$
\sigma = (a_0 \enspace a_1 \enspace \dots \enspace a_{n - 1})
$$

首先根据轮换的定义，不难发现：

$$
\sigma^t(a_i) = a_{[(i + t) \bmod n]}
$$


接下来看看 $\sigma^t$ 中 $a_i$ 所在的轮换大小，实际上也就是 $a_i$ 所在“映射链”的长度。只需要求得最小正整数的 $k$，使得 $\sigma$ 作用于 $a_i$ $k$ 次后能够回到 $a_i$（也就是找到周期），就能够知道其所在的映射链的长度了。

令 $k \in N^{*} \text{ \ s.t. \ } \sigma^{tk}(a_i) = a_i$：

$$
\begin{aligned}
& i + tk \equiv i \pmod n \\
& \Rightarrow tk \equiv 0 \pmod n
\end{aligned}
$$

最小正整数解：$k = \frac{n}{\gcd(n, t)}$

这意味着 $\sigma^t$ 可表示为 $\gcd(n, t)$ 个长为 $\frac{n}{\gcd(n, t)}$ 的轮换。

另外注意到 $a_i$ 所在轮换里第 $j \ (0 \le j < \gcd(n, t) )$ 个元素为 $a_{(i + jt) \bmod n}$。由于 $i + jt \equiv i \pmod t$ 且 $\gcd(n, t) \mid t$，有 $i + jt \equiv i \pmod {\gcd(n, t)}$。这意味着：

- $a_i$ 所在轮换内元素下标模 $\gcd(n, t)$ 均为 $i$；
- $a_0, a_1, \dots a_{\gcd(n, t) - 1}$ 一定位于不同轮换。

这些性质足以快速求得任一长度为 $n$ 的置换的幂次：

- 将置换分解为轮换：$\mathcal{O}(n)$；
- 对轮换内的每一个元素应用上述性质以生成结果的轮换分解式：$\mathcal{O}(n)$；
- 还原成置换：$\mathcal{O}(n)$。

### 置换群

$n$ 个元的所有置换，在复合运算 $\circ$ 下成群，称作 $n$ 元**对称群 (symmetric group)**，记作 $S_n$

- **结合律**：$(\sigma \circ \tau) \circ \phi = \sigma \circ (\tau \circ \phi)$
- **单位元**：恒等置换 $\epsilon \circ x = x$；
- **逆元**：置换是双射，故必然存在逆置换。

## 群在集合上的作用

群在集合上作用是一个非常重要的概念。考虑如下映射 $\phi$：

$$
\begin{aligned}
\phi: G \times M & \longrightarrow M \\
(\sigma, x) & \longmapsto \sigma \circ x
\end{aligned}
$$

若 $\forall x \in M$ 同时满足：

- **单位元**：$\exist \epsilon \in G \text{ \ s.t. \ }\epsilon \circ x = x$
- **结合律**：$\tau \circ (\sigma \circ x) = (\tau \circ \sigma) \circ x$

则称群 $G$ 在集合 $M$ 上有群作用。

根据 Cayley 定理，每个群均同构于某个置换群。有了这个前提可能会更好理解群在集合上的作用。但是今天碍于主题，我们主要探讨置换群对于集合的作用。

---

为了更加清晰地介绍这一概念，再来看看本文开头所举的对等边三角形顶点染色的例子。

考虑置换群 $G$ 和集合 $M$：

$$
\begin{aligned}
G& = \{ \text{顺时针旋转 } 0^\circ, 120^\circ, 240^\circ \} \\
M & = \{ \text{不考虑同构时的染色方案} \}
\end{aligned}
$$

首先来看看不考虑同构时的所有染色方案：

![不考虑同构时的染色方案](../../slides/permutation-group/ga-example-2.png)

再来看看 $\phi$ 作用下得到的结果：

![$\phi$ 作用下得到的结果](../../slides/permutation-group/ga-example-3.png)

可以看到，本质上 $\phi$ 作用后是并没有产生新元素的。另外，存在单位置换（旋转 $0^\circ$）使得它与任何一个染色方案作用都不发生变化；多个旋转作用于染色方案也是满足结合律的。所以在这个例子里 $G$ 对 $M$ 有群作用。

另外，图中每一列其实都是一个等价类。发现实际上不同的等价类只有四种（第 $2, 3, 4$ 列是相同的，第 $5, 6, 7$ 列是相同的）。可见，在旋转群的作用下，本质不同的方案实际上只有 $4$ 种。

![等价类](../../slides/permutation-group/ga-example-6.png)

### 轨道

我们把之前图中每一列都称作**轨道**。换言之，过 $x$ 的轨道就是将 $G$ 种每一个置换分别作用于 $x$ 得到的元素所组成的集合。由于群作用保证了不会产生新元素，因此这个集合是 $M$ 的子集。

---

群 $G$ 作用于集合 $M$ 上，$x \in M$，称 $M$ 的子集

$$
\text{orb}_G(x) = \{ \sigma \circ x \mid \sigma \in G \}
$$

为 $x$ 在 $G$ 作用下的**轨道 (orbit)**，简称过 $x$ 的轨道。

---

在之前的例子中，我们发现每一个元素都是属于唯一轨道的。换句话说，借助轨道，我们可以对集合 $M$ 中的元素进行分类。那对于更一般的情况这也成立吗？为了验证这一点，不妨继续把之前讨论等价类的那一套理论搬过来：

定义如下关系 $\sim$：

$$
x \sim y := x \in \text{orb}_G(y)
$$

只需要验证 $\sim$ 是一个**等价关系**即可。

- **自反性**： $x \in \text{orb}_G(x)$；
  - 恒等置换 $\epsilon \in G$，故 $\epsilon \circ x = x \in \text{orb}_G(x)$
- **对称性**：若 $y \in \text{orb}_G(x)$，则 $x \in \text{orb}_G(y)$；
  - $y \in orb_G(x) \Rightarrow \exist \sigma \ \text{ s.t. } \ \sigma \circ x = y$
  - $G$ 中逆元存在，故 $\exist \sigma \ \text{ s.t. } \ \sigma^{-1} \circ y = x$
  - 由 $\sigma^{-1} \in G$，故 $x \in \text{orb}_G(y)$
- **传递性**：若 $z \in \text{orb}_G(y), \ y \in \text{orb}_G(x)$，则 $z \in \text{orb}_G(x)$
  - $z \in orb_G(y) \Rightarrow \exist \sigma \ \text{ s.t. } \ \sigma \circ y = z$
  - $y \in orb_G(x) \Rightarrow \exist \tau \ \text{ s.t. } \ \tau \circ x = y$
  - 令 $\beta = \sigma \circ \tau$，则 $\beta \circ x = z$ 且 $\beta \in G$，故 $z \in \text{orb}_G(x)$

Voilà! 这样一来，之前的那一套结论也可以搬过来了：

- 若 $\text{orb}_G(x) \cap \text{orb}_G(y) \neq \emptyset$，则 $\text{orb}_G(x) = \text{orb}_G(y)$；
- 在 $M$ 的每一条轨道上取一个元素组成 $M$ 的一个子集 $R$，称为 $M$ 的**轨道的代表元集**，则：

  $$
  M = \bigcup\limits_{x \in R} \text{orb}_G(x)
  $$

  并且此中各 $\text{orb}_G(x)$ 互不相交。


---

既然可用于分类，则更进一步：如果 $G$ 中两个不同的置换 $\sigma, \tau$ 作用于 $x$ 后的结果是相同的，可以认为 $\sigma, \tau$ 在仅考虑作用于 $x$ 时是两个等价的置换（试着验证一下这是等价关系？）。由此，$| \text{orb}_G(x) |$ 实际上等价于仅考虑作用于 $x$ 时 $G$ 中本质不同的置换种数。

### 稳定子

另外发现元素 $x$ 可能在部分置换下所得到的结果依然是 $x$。将这些置换所组成的集合称作群 $G$ 作用下 $x$ 的稳定子。

---

设群 $G$ 作用于集合 $M$，对 $x \in M$，称

$$
\text{stab}_G(x) = \{ \sigma \mid \sigma \in G, \sigma \circ x = x \}
$$

为群 $G$ 作用下 $x$ 的**稳定子 (stabilizer)**。

---

发现 $\text{stab}_G(x)$ 其实是置换群 $G$ 的子群：

- **封闭性**：$\forall \sigma, \tau \in \text{stab}_G(x)$，$\sigma \circ \tau \circ x = \sigma \circ x = x$，故 $(\sigma \circ \tau) \in \text{stab}_G(x)$；
- **结合律**：显然置换的复合满足结合律；
- **单位元**：恒等置换 $\epsilon \circ x = x$；
- **逆元**：$\forall \sigma \in \text{stab}_G(x)$，$\sigma^{-1} \circ x = \sigma^{-1} \circ (\sigma \circ x) = \epsilon(x) = x$。

于是得到 $\text{stab}_G(x) \leq G$。

### 轨道-稳定子定理

联想之前的陪集划分，既然 $\text{stab}_G(x) \leq G$，是否也可用子群 $\text{stab}_G(x)$ 对置换群 $G$ 进行左陪集划分？

$\forall \beta \in G, \ \beta \text{stab}_G(x)$ 里的元素相当于作用于 $x$ 时 $G$ 中所有与 $\beta$ 等价的置换：

$$
\begin{aligned}
\beta \text{stab}_G(x) 
& = \{ (\beta \circ \sigma) \circ x = \beta \circ x \mid \sigma \in G \} \\
& \text{let } \tau = \beta \circ \sigma \\
& = \{ \tau \circ x = \beta \circ x \mid \tau \in G \}
\end{aligned}
$$

由拉格朗日定理：

$$
|G| = |\text{stab}_G(x)| \cdot [G:\text{stab}_G(x)]
$$

$[G:\text{stab}_G(x)]$ 实际上就是本质不同的陪集种数。回忆前文提到了$| \text{orb}_G(x) |$ 实际上等价于仅考虑作用于 $x$ 时 $G$ 中本质不同的置换种数，因此：

$$
[G:\text{stab}_G(x)] = |\text{orb}_G(x)|
$$

便得到了**轨道-稳定子定理 (oribt-stabilizer theorem)**。

---

设有限群 $G$ 作用于集合 $M$ ，$x \in M$，则：

$$
|G| = |\text{stab}_G(x)| \cdot |\text{orb}_G(x)|
$$

# Burnside 引理

## 内容

设有限群 $G$ 作用于有限集 $M$ 上，则轨道数：

$$
| M/G | = \frac{1}{|G|} \sum\limits_{\sigma \in G} |\text{fix}(\sigma)|
$$

其中 $\text{fix}(\sigma)$ 代表 $\sigma$ 的不动元构成的集合：

$$
\text{fix}(\sigma) = \{ x \mid x \in M, \sigma \circ x = x \}
$$

## 证明

回顾：

$$
\begin{aligned}
\text{stab}_G(x) & = \{ \sigma \mid \sigma \in G, \sigma \circ x = x \}\\
\text{fix}(\sigma) & = \{ x \mid x \in M, \sigma \circ x = x \}
\end{aligned}
$$

首先有一个引理：

$$
\sum\limits_{x \in M} | \text{stab}_G(x) | = \sum\limits_{\sigma \in G} | \text{fix}(\sigma) |
$$

发现等号左边实际上是对于集合 $M$ 内的每一个元素 $x$，看有多少置换 $\sigma$ 满足 $\sigma \circ x = x$；而等号右边是对于群 $G$ 内每一个置换 $\sigma$，看有多少元素 $x$ 满足 $\sigma \circ x = x$。换句话说，等号两边本质上都是求集合 $\{ (\sigma, x) \mid \sigma \in G, x \in M, \sigma \circ x = x \}$ 的大小，因此是相等的。

---

接下来证明 Burnside 引理就很容易了：

每个轨道对轨道数贡献为 $1$，故 $x \in M$ 对答案的贡献为 $\frac{1}{| \text{orb}_G(x) |}$：

$$
  \begin{aligned}
  | M/G | 
  & = \sum\limits_{x \in M} \frac{1}{ | \text{orb}_G(x) | } \\
  & = \sum\limits_{x \in M}\frac{ | \text{stab}_G(x) | }{ |G| } \text{（轨道-稳定子定理）} \\
  & = \frac{1}{|G|}\sum\limits_{\sigma \in G} | \text{fix}(\sigma) |
  \end{aligned}
$$

# Pólya 计数法

Burnside 引理启示我们要求轨道数，本质上还是要看不动元的数量之和。进一步，考虑在没有额外限制的情况下，对于置换 $\sigma$ 什么样的染色方案会称为不动元。

显然置换 $\sigma$ 可以被分解成若干个轮换，如：

$$
\sigma = (a_0 \enspace \dots \enspace a_t) \cdot (b_0 \enspace \dots \enspace b_s) \cdot \dots
$$

每一次置换作用时，每个轮换内的元素都会变成其右边的元素。故若要成为不动元，每个轮换内元素的颜色必然相同。这样一来，不动元数量之和其实就只与 $\sigma$ 所能被分解成的轮换个数相关了。

记染色可选的颜色数为 $m$， $c(\sigma)$ 为置换 $\sigma$ 被分解为不交轮换乘积的个数，则由乘法原理：

$$
\text{fix}(\sigma) = m^{c(\sigma)}
$$

故：

$$
| M/G | = \frac{1}{|G|} \sum\limits_{\sigma \in G} m^{c(\sigma)}
$$

这就是算法竞赛中常见的 Pólya 计数法。

# 常见题型

Hmm... 感觉这一部分的当时[幻灯片](https://codgician.me/pgslides/)说的还是比较清楚的，这里就不额外补充了（犯懒qwq）……

- [项链染色](https://codgician.me/pgslides/#/coloring-necklace) | [洛谷 P4980：Pólya定理](https://www.luogu.com.cn/problem/P4980)（这道题里只有旋转）| [参考代码](https://github.com/codgician/Competitive-Programming/blob/master/Luogu/P4980/polya_enumeration_theorem.cpp)
- [带限制的项链染色](https://codgician.me/pgslides/#/icpc-2019-nanchang-j) | [ICPC 2019 南昌 J: Summon](https://nanti.jisuanke.com/t/42585) | [参考代码](https://github.com/codgician/Competitive-Programming/blob/master/Jisuanke/42585/burnside_theorem_dp_matrix_fast_pow.cpp)
- [无向图染色](https://codgician.me/pgslides/#/coloring-undirected-graph) | [SHOI 2006: 有色图](https://www.luogu.com.cn/problem/P4128) | [参考代码](https://github.com/codgician/Competitive-Programming/blob/master/Luogu/P4128/polya_enumeration_theorem.cpp)
