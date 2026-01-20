---
title: "Hello World... Again!"
date: 2019-10-15
language: zh
canonical: true
translationKey: hello-world
tags:
  - life
  - hello
math: true
toc: false
draft: false
---


你好，世界…… 又来一次~

花了几天终于把博客从 Hexo 迁移到了 Blogdown + Hugo，整个过程比我想象的要麻烦一些。

现在有了 Hugo，页面渲染速度和多语言环境支持都好了不少。另外，有了 RMarkdown，也可以比较优雅地用代码绘制图表了。

新的博客主题采用的是魔改版的 [Hermit](https://github.com/Track3/Hermit)。这是一个非常不错的主题，并且如果你也是暗黑主题爱好者的话我非常非常推荐它。我按照自己的口味调整了一下主题的配色，并且做了一些乱七八糟的魔改来更好兼容 Blogdown。不管怎样现在这样子看起来还不错~

像之前一样，我依然在 [仓库](https://github.com/codgician/codgician.github.io) 的 `source` 分支下开源了博客的源文件，如果感兴趣的话可以参考一番。

---

下面用~~三个代表~~展示一下这个新组合的一些功能，另外如果你是用大屏设备访问的话可以在菜单里面打开目录。

# 三个代表

## 一点微小的代码

```haskell
putStrLn "Hello World... Again!"
```

## 一点微小的公式

$$
\begin{aligned}
\sum\limits_{i = 1}^{n} f(i) & = \sum\limits_{i = 1}^{n} \left[i \in \mathbb{P} \right] f(i) + \sum\limits_{i = 1}^{n} \left[i \not \in \mathbb{P} \right] f(i) \\
& = \sum\limits_{i = 1}^{n} \left[i \in \mathbb{P} \right] f(i) + f(1) + \sum\limits_{p \in \mathbb{P}}\sum\limits_{1 \le p^e \le n}f(p^e)\sum\limits_{i = 1}^{\left\lfloor \frac{n}{p^e} \right\rfloor}f(i)
\end{aligned}
$$

## 一点微小的图表

（此处原为 R Markdown 图表，已在迁移至 Hakyll 时移除）
