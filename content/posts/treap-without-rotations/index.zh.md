---
title: "浅谈无旋转 Treap"
date: 2018-07-28
language: zh
canonical: true
translationKey: treap-without-rotations
tags:
  - icpc-notes
  - algorithm-
  - competitive-programming
  - data-structure
  - treap
math: true
toc: true
draft: false
---


# 简介

**Treap = Tree + Heap**。Treap 是一种**弱平衡**的二叉搜索树。

相较普通的二叉搜索树，平衡二叉树与之最显著的区别就是后者是 “平衡的”，即采取了一些措施防止搜索树退化为一条链。严格来说，平衡二叉搜索树要求对于任意节点，左子树和右子树的高度差不可超过 $1$。维护这一性质往往需要各种复杂的旋转，反而可能会带来较大的常数。而弱平衡二叉树虽然不能保证左右子树高度差不超过 $1$，但是可以基本保证树的平衡，难以退化成长链的情况。

**Treap 通过二叉堆的性质来维护二叉树的平衡**。一个二叉（小根）堆满足：一个节点的两个儿子的值都小于节点本身。但是这样的规定与二叉搜索树的性质矛盾…… 为了解决这一问题只好让每个节点包含两个键值，其中随机生成的 $\text{rnd}$ 满足二叉堆性质，而要保存的数据 $\text{value}$ 满足二叉搜索树性质。由于主流的随机数生成算法很难出现完全单调的序列，故以此可以保证 Treap “基本平衡“。

# 无旋转的 Treap

## 节点定义

根据前面的描述，我们可以如下定义 Treap 中的节点：

```cpp
class Treap {
public:
    int val;      // 当前节点处的值
    int rnd;      // 随机值
    int siz;      // 以当前节点为根子树的大小
    int son[2];   // 左右儿子的下标
};

Treap trp[SIZE];  // 内存池
int trpPt;        // 内存池中第一个空闲节点的下标
```

在后文的实现中有一个略微取巧的地方：将 `trp[0]` 初始化为空节点，因此节点下表应当从 $1$ 开始分配。这样带来的好处是判断节点是否为空的时候更加方便（只需要判断下标是否为 $0$ 就行了）。此外，后文为了让代码更加直观，定义了如下函数：

```cpp
/* node(rt): int -> Treap& 获取下标为 rt 的节点信息 */
Treap& node(int rt) { return trp[rt]; }

/* lson(rt): int -> Treap& 获取下标为 rt 的左儿子节点信息 */
Treap& lson(int rt) { return trp[trp[rt].son[0]]; }

/* rson(rt): int -> Treap& 获取下标为 rt 的右儿子节点信息 */
Treap& rson(int rt) { return trp[trp[rt].son[1]]; }

/* maintain(rt): int -> void 更新以 rt 为根的子树大小 */
void maintain(int rt) { node(rt).siz = lson(rt).siz + rson(rt).siz + 1; }
```

## 核心操作

一般的 Treap 是带有旋转操作的（正如大部分平衡树一样），而无旋转的 Treap 却用巧妙的方式避免了旋转。无旋转 Treap 抽象出了如下两种核心操作：

- $\text{merge}(A, B)$：合并两棵子树（它们的根节点分别是 $A$ 和 $B$，并且满足 $A$ 中的所有 $\text{value}$ 均小于 $B$ 中的任意 $\text{value}$），复杂度 $\mathcal{O}(\log{n})$；
- $\text{split}(T, A, B, k)$：将以 $T$ 为根节点的树分裂为分别以 $A, B$ 为根的两棵子树，其中前者包含 $T$ 中的前 $k$ 小的节点（也可以实现为值小于 $k$ 的节点），后者包含剩余节点。复杂度 $\mathcal{O}(\log{n})$。

**注意：合并操作的前提是其中一棵 Treap 上的所有键值（即 $\text{value}$）小于另一棵 Treap 上的键值，否则只能启发式合并**！

有了这两个操作，我们就可以实现平衡树的大部分功能了。但在此之前，让我们先来看看如何实现这两个核心函数。

### 合并

我们假设现在维护的是满足**小根堆**性质的 Treap，记将被合并的两棵子树的根节点为 $A$ 和 $B$。我们以分治的思想考虑合并。

首先我们考虑 $\text{rnd}$ 应满足小根堆性质，故 $A$ 和 $B$ 中 $\text{rnd}$ 较小的点应当成为另一点的祖先。不妨假设 $A.\text{rnd} < B.\text{rnd}$（反之是同理的），则 $A$ 应当成为 $B$ 的祖先。

接下来考虑 $\text{value}$ 应满足二叉搜索树性质。由于 $A$ 节点原先的左子树中的值一定小于 $B$ 中的值，所以事实上我们只需要合并 $A$ 节点原先的右子树和 $B$。因此我们可以递归下去，也就完成了合并操作。

当然，最后还需要更新一下新的子树大小。

参考代码如下：

```cpp
int merge(int fstRt, int sndRt) {
    if (fstRt == 0)
        return sndRt;
    if (sndRt == 0)
        return fstRt;

    if (node(fstRt).rnd < node(sndRt).rnd) {
        node(fstRt).son[1] = merge(node(fstRt).son[1], sndRt);
        maintain(fstRt); 
        return fstRt;
    } else {
        node(sndRt).son[0] = merge(fstRt, node(sndRt).son[0]);
        maintain(sndRt); 
        return sndRt;
    }
}
```

### 分裂

分裂其实有两种方式：

- 按排名分裂：前 $k$ 小的数构成一棵子树，剩余的构成另一棵；
- 按权值分裂：小于等于 $k$ 的数构成一棵子树，剩余的构成另一棵。

接下来我们主要演示按排名分裂（按权值分裂基本同理），其实现也基于分治思想。

对于以 $T$ 为根的树，我们首先要考虑根要被拆分到值较小的左子树 $A$ 还是值较大的右子树 $B$ 去。如果当前左子树的大小大于等于 $k$，则说明 $T$ 以及其右子树都是应当被分到 $B$ 中去的。那我们不妨先把 $T$ 当作 $B$。接下来的问题就是要在 $B$ 的中继续分裂出前 $k$ 小的值给 $A$。因此我们递归地调用下去就好了。

而如果 $T$ 的左子树大小小于 $k$，则说明整个左子树都应该给 $A$。类似地，不妨把 $T$ 当作 $A$，然后递归地在 $A$ 中分裂出前 $k - \text{左子树大小}$ 的值给 $B$ 就好了。

最后不要忘了需要更新新的子树大小。

参考代码如下：

```cpp
void split(int rt, int k, int & fstRt, int & sndRt) {
    if (rt == 0) {
        fstRt = 0; sndRt = 0;
        return;
    }

    if (k <= lson(rt).siz) {
        sndRt = rt; 
        split(node(rt).son[0], k, fstRt, node(rt).son[0]);
    } else {
        fstRt = rt; 
        split(node(rt).son[1], k - lson(rt).siz - 1, node(rt).son[1], sndRt);
    }
    maintain(rt);
}
```

---

对于按权值分裂也是同理的，这里就只给出代码了：

```cpp
void splitByVal(int rt, int val, int & fstRt, int & sndRt) {
    if (rt == 0) {
        fstRt = 0; sndRt = 0;
        return;
    }

    if (node(rt).val > val) {
        sndRt = rt;
        splitByVal(node(rt).son[0], val, fstRt, node(rt).son[0]);
    } else {
        fstRt = rt;
        splitByVal(node(rt).son[1], val, node(rt).son[1], sndRt);
    }
    maintain(rt);
}
```

# 这意味着什么？

## 支持快速插入删除的数组

- 在任意位置 $k$ 插入节点 $V$：
  - $\text{split}(T, k, A, B)$;
  - $T = \text{merge}(\text{merge}(A, V), B)$;
- 在任意位置 $k$ 删除节点：
  - $\text{split}(T, k - 1, A, B)$;
  - $\text{split}(B, 1, \_, B)$;
  - $T = \text{merge}(A, B)$;
- 询问位置 $k$ 处节点的值：
  - $\text{split}(T, k - 1, A, B)$;
  - $\text{split}(B, 1, B, C)$;
  - 得到了 $B$ 节点的值~
  - $\text{merge}(\text{merge}(A, B), C)$;

看起来很有用~ 基本可以代替掉块状链表了。

## 平衡树

我们不妨先直接拉一道家喻户晓的模板题来举个例子：[普通平衡树 - 题目 - LibreOJ](https://loj.ac/problem/104)。

题目中要求我们实现 $6$ 类操作：

1. 插入数 $x$；
2. 删除数 $x$（若有多个相同的 $x$ 则只删除一个）；
3. 查询数 $x$ 的排名（若有多个相同的 $x$ 则输出最小排名）；
4. 查询排名为 $x$ 的数；
5. 求 $x$ 的前驱（即小于 $x$ 的数中的最大值）；
6. 求 $x$ 的后继（即大于 $x$ 的数中的最小值）。

下面我们就试着主要靠两个核心函数来解决上述 $6$ 个问题。

### 插入

 首先我们将带插入节点的 $\text{rnd}$ 赋成随机值，然后按 $x$ 将 Treap $\text{split}$ （按权值分裂） 成小于等于 $x$ 的 $A$ 和大于 $x$ 的 $B$。我们不妨把待插入的 $x$ 看成一棵只含一个节点的 Treap，先将其与 $A$ $\text{merge}$，再得到的 Treap 与 $B$ $\text{merge}$，就完成插入了。

### 删除

与插入类似，首先我们按 $x$ 将 Treap $\text{split}$ 成小于等于 $x$ 的 $A$ 和大于 $x$ 的 $B$，再将 $A$ $\text{split}$ 成小于等于 $x - 1$ 的 $C$ 和大于 $x - 1$（即等于 $x$）的 $D$。接下来，我们将 $B$ 和 $C$ $\text{merge}$ 在一起即可。

上述操作会把所有等于 $x$ 的数全部删掉。如果题目只要求删掉一个 $x$，可以考虑将 $D$ 的左子树和右子树合并在一起得到 $E$。这样一来就等于舍弃了 $D$ 的根节点，使得 $D$ 中等于 $x$ 的数少了一个。最后再把 $E$ 也 $\text{merge}$ 进答案。

### 查询数的排名

实际上就是查询有多少个数比 $x$ 小，然后将结果加 $1$ 即是排名。

考虑现按 $x - 1$ 将 Treap $\text{split}$ 成 $A$ 和 $B$，这样一来 $A$ 的大小加上 $1$ 就是我们需要的答案。查询完后再将 $A$ 和 $B$ $\text{merge}$ 起来恢复原状即可。

### 查询排名对应的数

我们只需要实现按排名分裂，就可以跟上面类似直接 $\text{split}$ 一下就好了。

### 查询前驱或后继

这里我们以查询前驱为例（后继完全同理）：

我们考虑先将 Treap 按 $x - 1$ $\text{split}$ 成 $A$ 和 $B$，那么 $A$ 树中最大的数（即第 $A.size$ 大）就是我们要查询的答案。借用上面的查询排名对应数的函数即可实现。

### 参考代码

大家可以看看 [我的代码](https://github.com/codgician/Competitive-Programming/blob/master/LOJ/104/treap_without_rotations.cpp) 来对照理解一番~

# %%%

- chen_tr - [偷懒专用平衡树——Treap](https://blog.csdn.net/chen_tr/article/details/50924073)
- LadyLex - [无旋treap：从好奇到入门](https://www.cnblogs.com/LadyLex/p/7182491.html)
- yyf0309 - [无旋转Treap简介](https://www.cnblogs.com/yyf0309/p/Unrotated_Treap.html)
